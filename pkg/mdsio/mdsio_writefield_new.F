C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_writefield_new.F,v 1.4 2005/11/05 00:52:19 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSWRITEFIELD_NEW(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   useCurrentDir,
     I   arrType,
     I   nNzdim,nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to written
C filePrec	integer	number of bits per word in file (32 or 64)
C globalFile	logical	selects between writing a global or tiled file
C useCurrentDir logical	always write to the current directory (even if
C			"mdsioLocalDir" is set)
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNzdim	integer	size of third dimension: normally either 1 or Nr
C nNz		integer	number of vertical levels to be written
C arr		RS/RL	array to write, arr(:,:,nNzdim,:,:)
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C MDSWRITEFIELD creates either a file of the form "fName.data" and
C "fName.meta" if the logical flag "globalFile" is set true. Otherwise
C it creates MDS tiled files of the form "fName.xxx.yyy.data" and
C "fName.xxx.yyy.meta". A meta-file is always created.
C Currently, the meta-files are not read because it is difficult
C to parse files in fortran. We should read meta information before
C adding records to an existing multi-record file.
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL". nNz allows for both 2-D and
C 3-D arrays to be handled. nNz=1 implies a 2-D model field and
C nNz=Nr implies a 3-D model field. irecord is the record number
C to be read and must be >= 1. NOTE: It is currently assumed that
C the highest record number in the file was the last record written.
C Nor is there a consistency check between the routine arguments and file.
C ie. if your write record 2 after record 4 the meta information
C will record the number of records to be 2. This, again, is because
C we have read the meta information. To be fixed.
C
C Created: 03/16/99 adcroft@mit.edu
C
C Changed: 05/31/00 heimbach@mit.edu
C          open(dUnit, ..., status='old', ... -> status='unknown'
C
C Changed: 01/06/02 menemenlis@jpl.nasa.gov
C          added useSingleCpuIO hack
C changed:  1/23/04 afe@ocean.mit.edu
C          added exch2 handling -- yes, the globalfile logic is nuts

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      logical useCurrentDir
      character*(2) arrType
      integer nNzdim, nNz
cph(
cph      Real arr(*)
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNzdim,nSx,nSy)
cph)
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,metaFName,pfName
      character*(MAX_LEN_MBUF) msgbuf
      logical fileIsOpen
      integer iG,jG,irec,bi,bj,j,k,dUnit,IL,pIL
      integer dimList(3,3),ndims
      integer x_size,y_size,length_of_rec
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      PARAMETER ( x_size = exch2_domain_nxt * sNx )
      PARAMETER ( y_size = exch2_domain_nyt * sNy )
#else
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
#endif
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
#ifdef ALLOW_USE_MPI
      INTEGER iG_IO,jG_IO,npe,i
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 global(Nx,Ny)
      _RL    local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_EXCH2
      integer domainHeight,domainLength,tgy,tgx,tny,tnx,tn
#endif /* ALLOW_EXCH2 */

C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEFIELD: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELD: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELD'
      endif

C Assume nothing
      fileIsOpen=.FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
       pfName = fName
      ELSE
       WRITE(pfName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      ENDIF
      pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

#ifdef ALLOW_USE_MPI
      _END_MASTER( myThid )
C If option globalFile is desired but does not work or if
C globalFile is too slow, then try using single-CPU I/O.
      if (useSingleCpuIO) then

C Master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         write(dataFName,'(2a)') fName(1:IL),'.data'
         length_of_rec=MDS_RECLEN(filePrec,x_size*y_size,mythid)
         if (irecord .EQ. 1) then
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &        access='direct', recl=length_of_rec )
         else
          open( dUnit, file=dataFName, status=_OLD_STATUS,
     &        access='direct', recl=length_of_rec )
         endif
        ENDIF
       _END_MASTER( myThid )

C Gather array and write it to file, one vertical level at a time
       DO k=1,nNz
C-      copy from arr(level=k) to 2-D "local":
        IF ( arrType.EQ.'RS' ) THEN
          CALL MDS_PASStoRS( local, arr, k, nNzdim, .FALSE., myThid )
        ELSEIF ( arrType.EQ.'RL' ) THEN
          CALL MDS_PASStoRL( local, arr, k, nNzdim, .FALSE., myThid )
        ELSE
          WRITE(msgbuf,'(A)')
     &         ' MDSWRITEFIELD: illegal value for arrType'
          CALL PRINT_ERROR( msgbuf, mythid )
          STOP 'ABNORMAL END: S/R MDSWRITEFIELD'
        ENDIF
        CALL GATHER_2D( global, local, myThid )
        _BEGIN_MASTER( myThid )
         IF( mpiMyId .EQ. 0 ) THEN
          irec=k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           DO J=1,y_size
            DO I=1,x_size
             xy_buffer_r4(I,J) = 0.0
            ENDDO
           ENDDO
           bj=1
           DO npe=1,nPx*nPy
            DO bi=1,nSx
             DO J=1,sNy
              DO I=1,sNx
               iG=mpi_myXGlobalLo(npe)-1+(bi-1)*sNx+i
               jG=mpi_myYGlobalLo(npe)-1+(bj-1)*sNy+j
               iG_IO=exch2_txglobalo(W2_mpi_myTileList(npe,bi))+i-1
               jG_IO=exch2_tyglobalo(W2_mpi_myTileList(npe,bi))+j-1
               xy_buffer_r4(iG_IO,jG_IO) = global(iG,jG)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r4(I,J) = global(I,J)
            ENDDO
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
           write(dUnit,rec=irec) xy_buffer_r4
          elseif (filePrec .eq. precFloat64) then
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           DO J=1,y_size
            DO I=1,x_size
             xy_buffer_r8(I,J) = 0.0
            ENDDO
           ENDDO
           bj=1
           DO npe=1,nPx*nPy
            DO bi=1,nSx
             DO J=1,sNy
              DO I=1,sNx
               iG=mpi_myXGlobalLo(npe)-1+(bi-1)*sNx+i
               jG=mpi_myYGlobalLo(npe)-1+(bj-1)*sNy+j
               iG_IO=exch2_txglobalo(W2_mpi_myTileList(npe,bi))+i-1
               jG_IO=exch2_tyglobalo(W2_mpi_myTileList(npe,bi))+j-1
               xy_buffer_r8(iG_IO,jG_IO) = global(iG,jG)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r8(I,J) = global(I,J)
            ENDDO
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( x_size*y_size, xy_buffer_r8 )
#endif
           write(dUnit,rec=irec) xy_buffer_r8
          else
           write(msgbuf,'(a)')
     &       ' MDSWRITEFIELD: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSWRITEFIELD'
          endif
         ENDIF
        _END_MASTER( myThid )
       ENDDO

C Close data-file and create meta-file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         close( dUnit )
         write(metaFName,'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=x_size
         dimList(2,1)=1
         dimList(3,1)=x_size
         dimList(1,2)=y_size
         dimList(2,2)=1
         dimList(3,2)=y_size
         dimList(1,3)=nNz
         dimList(2,3)=1
         dimList(3,3)=nNz
         ndims=3
         if (nNz .EQ. 1) ndims=2
         call MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, mythid )
        ENDIF
       _END_MASTER( myThid )
C To be safe, make other processes wait for I/O completion
       _BARRIER

      elseif ( .NOT. useSingleCpuIO ) then
      _BEGIN_MASTER( myThid )
#endif /* ALLOW_USE_MPI */

C If we are writing to a global file then we open it here
      if (globalFile) then
       write(dataFName,'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
        open( dUnit, file=dataFName, status=_OLD_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       endif
      endif

#ifdef ALLOW_EXCH2
      if (globalFile) then
      domainLength = exch2_domain_nxt
      domainHeight = exch2_domain_nyt
C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
        tn = W2_myTileList(bi)
        tgy = exch2_tyglobalo(tn)
        tgx = exch2_txglobalo(tn)
        tny = exch2_tny(tn)
        tnx = exch2_tnx(tn)
        if (fileIsOpen) then
         do k=1,nNz
          do j=1,tNy

           irec = domainLength*(tgy-1) + (tgx-1)/tnx + 1 +
     &            domainLength*(j-1) + 
     &            domainLength*domainHeight*tny*(k-1) +
     &            domainLength*domainHeight*tny*nNz*(irecord-1) 


           if (filePrec .eq. precFloat32) then
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS( j,bi,bj,k,nNzdim, r4seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nNzdim, r4seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &        ' MDSWRITEFIELD: illegal value for arrType'
                call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD'
            endif
#ifdef _BYTESWAPIO
                        call MDS_BYTESWAPR4( sNx, r4seg )
#endif
                        write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS( j,bi,bj,k,nNzdim, r8seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nNzdim, r8seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgbuf,'(a)')
     &        ' MDSWRITEFIELD: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSWRITEFIELD'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
        else ! .not. fileIsOpen 
         write(msgbuf,'(a)')
     &     ' MDSWRITEFIELD: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEFIELD'
        endif
       enddo
      enddo
      else ! not global file

#endif /* ALLOW_EXCH2 */
C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFName,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         if (irecord .EQ. 1) then
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status=_OLD_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         endif
        endif
        if (fileIsOpen) then
         do k=1,nNz
          do j=1,sNy
           if (globalFile) then
            iG = myXGlobalLo-1+(bi-1)*sNx
            jG = myYGlobalLo-1+(bj-1)*sNy
            irec=1+INT(iG/sNx)+nSx*nPx*(jG+j-1)+nSx*nPx*Ny*(k-1)
     &           +nSx*nPx*Ny*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=j + sNy*(k-1) + sNy*nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS( j,bi,bj,k,nNzdim, r4seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nNzdim, r4seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS( j,bi,bj,k,nNzdim, r8seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nNzdim, r8seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgbuf,'(a)')
     &        ' MDSWRITEFIELD: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSWRITEFIELD'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEFIELD: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEFIELD'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen .AND. (.NOT. globalFile)) then
         close( dUnit )
         fileIsOpen = .FALSE.
        endif
C Create meta-file for each tile if we are tiling
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(metaFname,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.meta'
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
         tn = W2_myTileList(bi)
         dimList(1,1)=x_size
         dimList(2,1)=exch2_txGlobalo(tn)
         dimList(3,1)=exch2_txGlobalo(tn)+sNx-1
         dimList(1,2)=y_size
         dimList(2,2)=exch2_tyGlobalo(tn)
         dimList(3,2)=exch2_tyGlobalo(tn)+sNy-1
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
C- jmc: if MISSING_TILE_IO, keep meta files unchanged 
C       to stay consistent with global file structure
         dimList(1,1)=Nx
         dimList(2,1)=myXGlobalLo+(bi-1)*sNx
         dimList(3,1)=myXGlobalLo+bi*sNx-1
         dimList(1,2)=Ny
         dimList(2,2)=myYGlobalLo+(bj-1)*sNy
         dimList(3,2)=myYGlobalLo+bj*sNy-1
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
         dimList(1,3)=nNz
         dimList(2,3)=1
         dimList(3,3)=nNz
         ndims=3
         if (nNz .EQ. 1) ndims=2
         call MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, mythid )
        endif
C End of bi,bj loops
       enddo
      enddo
c#endif /* ALLOW_EXCH2 */

#ifdef ALLOW_EXCH2
      endif ! global fle
#endif /* ALLOW_EXCH2 */

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

C Create meta-file for the global-file
      if (globalFile) then
C We can not do this operation using threads (yet) because of the
C "barrier" at the next step. The barrier could be removed but
C at the cost of "safe" distributed I/O.
       if (nThreads.NE.1) then
        write(msgbuf,'(a,a)')
     &    ' MDSWRITEFIELD: A threads version of this routine',
     &    ' does not exist.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    ' MDSWRITEFIELD: This needs to be fixed...'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i3.2)')
     &    ' MDSWRITEFIELD: nThreads = ',nThreads
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    ' MDSWRITEFIELD: Stopping because you are using threads'
        call print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MDSWRITEFIELD'
       endif
C We put a barrier here to ensure that all processes have finished
C writing their data before we update the meta-file
       _BARRIER
       write(metaFName,'(2a)') fName(1:IL),'.meta'
       dimList(1,1)=x_size
       dimList(2,1)=1
       dimList(3,1)=x_size
       dimList(1,2)=y_size
       dimList(2,2)=1
       dimList(3,2)=y_size
       dimList(1,3)=nNz
       dimList(2,3)=1
       dimList(3,3)=nNz
       ndims=3
       if (nNz .EQ. 1) ndims=2
       call MDSWRITEMETA( metaFName, dataFName,
     &   filePrec, ndims, dimList, irecord, myIter, mythid )
       fileIsOpen=.TRUE.
      endif

      _END_MASTER( myThid )

#ifdef ALLOW_USE_MPI
C endif useSingleCpuIO
      endif
#endif /* ALLOW_USE_MPI */

C     ------------------------------------------------------------------
      return
      end
