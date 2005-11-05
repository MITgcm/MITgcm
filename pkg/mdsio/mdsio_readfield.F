C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_readfield.F,v 1.24 2005/11/05 00:52:19 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSREADFIELD(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     O   arr,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to read
C filePrec	integer	number of bits per word in file (32 or 64)
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNz		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to read into, arr(:,:,nNz,:,:)
C irecord	integer	record number to read
C myThid	integer thread identifier
C
C MDSREADFIELD first checks to see if the file "fName" exists, then
C if the file "fName.data" exists and finally the tiled files of the
C form "fName.xxx.yyy.data" exist. Currently, the meta-files are not
C read because it is difficult to parse files in fortran.
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL". nNz allows for both 2-D and
C 3-D arrays to be handled. nNz=1 implies a 2-D model field and
C nNz=Nr implies a 3-D model field. irecord is the record number
C to be read and must be >= 1. The file data is stored in
C arr *but* the overlaps are *not* updated. ie. An exchange must
C be called. This is because the routine is sometimes called from
C within a MASTER_THID region.
C
C Created: 03/16/99 adcroft@mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer nNz
      Real arr(*)
      integer irecord
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,pfName
      character*(max_len_mbuf) msgbuf
      logical exst
      logical globalFile,fileIsOpen
      integer iG,jG,irec,bi,bj,i,j,k,dUnit,IL,pIL
      integer x_size,y_size,length_of_rec
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      integer iG_IO,jG_IO,npe
      PARAMETER ( x_size = exch2_domain_nxt * sNx )
      PARAMETER ( y_size = exch2_domain_nyt * sNy )
#else
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
#endif
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 global   (Nx,Ny)
      _RL    local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_EXCH2
      integer domainHeight,domainLength,tgy,tgx,tny,tnx,tn
#endif /* ALLOW_EXCH2 */

C     ------------------------------------------------------------------

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADFIELD: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADFIELD: Invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELD'
      endif

C Assign special directory
      if ( pIL.EQ.0 ) then
       pfName= fName
      else
       write(pfName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      endif
      pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

      if ( .not. useSingleCPUIO ) then

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFName, exist=exst )
      if (exst) then
       if ( debugLevel .GE. debLevA ) then
        write(msgbuf,'(a,a)')
     &   ' MDSREADFIELD: opening global file: ',dataFName(1:IL)
        call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       endif
       globalFile = .TRUE.
      endif

C If negative check for global file with MDS name (ie. fName.data)
      if (.NOT. globalFile) then
       write(dataFName,'(2a)') fName(1:IL),'.data'
       inquire( file=dataFName, exist=exst )
       if (exst) then
        if ( debugLevel .GE. debLevA ) then
         write(msgbuf,'(a,a)')
     &    ' MDSREADFIELD: opening global file: ',dataFName(1:IL+5)
         call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        endif
        globalFile = .TRUE.
       endif
      endif

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

#ifdef ALLOW_EXCH2

      if (globalFile) then
       domainLength = exch2_domain_nxt
       domainHeight = exch2_domain_nyt

cafe          write(*,fmt='(1X,A,I3,A,I3)') 'L=', domainlength,
cafe     &            'H=', domainheight
C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
cafe        if (.NOT. globalFile) then
cafe             write(msgbuf,'(a,a)')
cafe     &         ' MDSREADFIELD: non-global input files not '
cafe     &           'implemented with exch2'
cafe             call print_error( msgbuf, mythid )
cafe             stop 'ABNORMAL END: S/R MDSREADFIELD'
cafe        endif
        tn = W2_myTileList(bi)
        tgy = exch2_tyglobalo(tn)
        tgx = exch2_txglobalo(tn)
        tny = exch2_tny(tn)
        tnx = exch2_tnx(tn)
        if (fileIsOpen) then
         do k=1,nNz
          do j=1,tNy
cafe          write(*,fmt='(1X,A,I3,A,I3,A,I3,A,I3,A,I3,A,I3)') 'tgy=', tgy,
cafe     &        ', tgx=', tgx,
cafe     &            ', tnx=',tnx, ', tny=', tny, ', j=',j,', tn=',tn
            
            irec = domainLength*(tgy-1) + (tgx-1)/tnx + 1 +
     &             domainLength*(j-1) + 
     &             domainLength*domainHeight*tny*(k-1) +
     &             domainLength*domainHeight*tny*nNz*(irecord-1) 
cafe             write(*,fmt='(1X,A,I6,A,I3)') 'record = ',irec,',thingy=',
cafe     &          (tgx-1)/tnx
cafe             write(*,fmt='(1X,A,I6)') 'record = ',irec
 
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS( j,bi,bj,k,nNz, r4seg, .TRUE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nNz, r4seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD'
            endif
           elseif (filePrec .eq. precFloat64) then
            read(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS( j,bi,bj,k,nNz, r8seg, .TRUE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nNz, r8seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD'
            endif
           else
            write(msgbuf,'(a)')
     &        ' MDSREADFIELD: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADFIELD'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
         if (.NOT. globalFile) then
          close( dUnit )
          fileIsOpen = .FALSE.
         endif
        endif
C End of bi,bj loops
       enddo
      enddo
      else ! if globalFile
c#else /* .not. ALLOW_EXCH2 */
#endif /* ALLOW_EXCH2 */


C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFName,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         inquire( file=dataFName, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
         if (exst) then
          if ( debugLevel .GE. debLevA ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADFIELD: opening file: ',dataFName(1:pIL+13)
           call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          endif
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(4a)') ' MDSREADFIELD: filename: ',
     &             fName(1:IL),' , ', dataFName(1:pIL+13)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELD: Files do not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELD'
         endif
        endif

        if (fileIsOpen) then
         do k=1,nNz
          do j=1,sNy
           if (globalFile) then
            iG = myXGlobalLo-1 + (bi-1)*sNx
            jG = myYGlobalLo-1 + (bj-1)*sNy
            irec=1 + INT(iG/sNx) + nSx*nPx*(jG+j-1) + nSx*nPx*Ny*(k-1)
     &             + nSx*nPx*Ny*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=j + sNy*(k-1) + sNy*nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS( j,bi,bj,k,nNz, r4seg, .TRUE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nNz, r4seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD'
            endif
           elseif (filePrec .eq. precFloat64) then
            read(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS( j,bi,bj,k,nNz, r8seg, .TRUE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nNz, r8seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD'
            endif
           else
            write(msgbuf,'(a)')
     &        ' MDSREADFIELD: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADFIELD'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
         if (.NOT. globalFile) then
          close( dUnit )
          fileIsOpen = .FALSE.
         endif
        endif
C End of bi,bj loops
       enddo
      enddo

#ifdef ALLOW_EXCH2
      endif ! globalFile

#endif /* ALLOW_EXCH2 */

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

      endif
c     endif ( .not. useSingleCPUIO )

      _END_MASTER( myThid )

      if ( useSingleCPUIO ) then

C master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
        IF( mpiMyId .EQ. 0 ) THEN
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */

C Check first for global file with simple name (ie. fName)
         dataFName = fName
         inquire( file=dataFName, exist=exst )
         if (exst) then 
           if ( debugLevel .GE. debLevA ) then
             write(msgbuf,'(a,a)')
     &        ' MDSREADFIELD: opening global file: ',dataFName(1:IL)
             call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
           endif
           globalFile = .TRUE.
         endif

C If negative check for global file with MDS name (ie. fName.data)
         if (.NOT. globalFile) then
          write(dataFName,'(2a)') fName(1:IL),'.data'
          inquire( file=dataFName, exist=exst )
          if (exst) then 
            if ( debugLevel .GE. debLevA ) then
             write(msgbuf,'(a,a)')
     &        ' MDSREADFIELD: opening global file: ',dataFName(1:IL+5)
             call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            endif
            globalFile = .TRUE.
          endif
         endif

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         if ( globalFile) then
          length_of_rec=MDS_RECLEN( filePrec, x_size*y_size, mythid )
          open( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         else
          write(msgbuf,'(2a)') 
     &      ' MDSREADFIELD: filename: ', dataFName(1:IL+5)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELD: File does not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELD'
         endif

        ENDIF
       _END_MASTER( myThid )

       DO k=1,nNz

        _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
         IF( mpiMyId .EQ. 0 ) THEN
#else
         IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
          irec = k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
           read(dUnit,rec=irec) xy_buffer_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           bj=1
           DO npe=1,nPx*nPy
            DO bi=1,nSx
             DO J=1,sNy
              DO I=1,sNx
#ifdef ALLOW_USE_MPI
               iG=mpi_myXGlobalLo(npe)-1+(bi-1)*sNx+i
               jG=mpi_myYGlobalLo(npe)-1+(bj-1)*sNy+j
#else
               iG= myXGlobalLo-1+(bi-1)*sNx+i
               jG= myYGlobalLo-1+(bj-1)*sNy+j
#endif /* ALLOW_USE_MPI */
               iG_IO=exch2_txglobalo(W2_mpi_myTileList(npe,bi))+i-1
               jG_IO=exch2_tyglobalo(W2_mpi_myTileList(npe,bi))+j-1
               global(iG,jG) = xy_buffer_r4(iG_IO,jG_IO)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = xy_buffer_r4(I,J)
            ENDDO
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) xy_buffer_r8
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( x_size*y_size, xy_buffer_r8 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           bj=1
           DO npe=1,nPx*nPy
            DO bi=1,nSx
             DO J=1,sNy
              DO I=1,sNx
#ifdef ALLOW_USE_MPI
               iG=mpi_myXGlobalLo(npe)-1+(bi-1)*sNx+i
               jG=mpi_myYGlobalLo(npe)-1+(bj-1)*sNy+j
#else
               iG= myXGlobalLo-1+(bi-1)*sNx+i
               jG= myYGlobalLo-1+(bj-1)*sNy+j
#endif /* ALLOW_USE_MPI */
               iG_IO=exch2_txglobalo(W2_mpi_myTileList(npe,bi))+i-1
               jG_IO=exch2_tyglobalo(W2_mpi_myTileList(npe,bi))+j-1
               global(iG,jG) = xy_buffer_r8(iG_IO,jG_IO)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = xy_buffer_r8(I,J)
            ENDDO
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          else
           write(msgbuf,'(a)')
     &            ' MDSREADFIELD: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELD'
          endif
         ENDIF
        _END_MASTER( myThid )
        CALL SCATTER_2D(global,local,mythid)
        if (arrType .eq. 'RS') then
           CALL MDS_PASStoRS( local,arr,k,nNz, .TRUE., mythid )
        elseif (arrType .eq. 'RL') then
           CALL MDS_PASStoRL( local,arr,k,nNz, .TRUE., mythid )
        else
           write(msgbuf,'(a)')
     &          ' MDSREADFIELD: illegal value for arrType'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELD'
        endif

       ENDDO
c      ENDDO k=1,nNz

       _BEGIN_MASTER( myThid )
        close( dUnit )
       _END_MASTER( myThid )

      endif
c     endif ( useSingleCPUIO )

      return
      end
