C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_readfield.F,v 1.7 2004/01/23 21:26:55 afe Exp $
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
#ifdef USE_W2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* USE_W2 */

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
      character*(80) dataFName,pfName
      integer iG,jG,irec,bi,bj,j,k,dUnit,IL,pIL
      logical exst
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
      integer i
      Real*4 global_r4(Nx,Ny)
      Real*8 global   (Nx,Ny)
      _RL local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef USE_W2
      integer domainHeight,domainLength,tby,tgx,tny,tnx,tn
#endif /* USE_W2 */
C     ------------------------------------------------------------------

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

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      if ( mdsioLocalDir .NE. ' ' ) then
       write(pFname(1:80),'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      else
       pFname= fName
      endif
      pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       if ( debugLevel .GE. debLevA ) then
        write(msgbuf,'(a,a)')
     &   ' MDSREADFIELD: opening global file: ',dataFName
        call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
        endif
       globalFile = .TRUE.
      endif

C If negative check for global file with MDS name (ie. fName.data)
      if (.NOT. globalFile) then
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       inquire( file=dataFname, exist=exst )
       if (exst) then
        if ( debugLevel .GE. debLevA ) then
         write(msgbuf,'(a,a)')
     &    ' MDSREADFIELD: opening global file: ',dataFName
         call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
         endif
        globalFile = .TRUE.
       endif
      endif

      if ( .not. ( globalFile .and. useSingleCPUIO ) ) then

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

#ifdef USE_W2


       domainLength = exch2_domain_nxt
       domainHeight = exch2_domain_nyt

          write(*,fmt='(1X,A,I3,A,I3)') 'L=', domainlength,
     &            'H=', domainheight
C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD: non-global input files not 
     &           implemented with exch2'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD'
        endif
        tn = W2_myTileList(bi)
        tby = exch2_tbasey(tn)
        tgx = exch2_txglobalo(tn)
        tny = exch2_tny(tn)
        tnx = exch2_tnx(tn)
        if (fileIsOpen) then
         do k=1,nNz
          do j=1,tNy
          write(*,fmt='(1X,A,I3,A,I3,A,I3,A,I3,A,I3,A,I3)') 'tby=', tby,
     &        ', tgx=', tgx,
     &            ', tnx=',tnx, ', tny=', tny, ', j=',j,', tn=',tn
            
            irec = domainLength*tby + (tgx-1)/tnx + 1 +
     &             domainLength*(j-1) + 
     &             domainLength*domainHeight*tny*(k-1) +
     &             domainLength*domainHeight*tny*nNz*(irecord-1) 
c             write(*,fmt='(1X,A,I6,A,I3)') 'record = ',irec,',thingy=',
c     &          (tgx-1)/tnx
             write(*,fmt='(1X,A,I6)') 'record = ',irec
 
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

#else /* .not. USE_W2 */



C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
         if (exst) then
          if ( debugLevel .GE. debLevA ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADFIELD: opening file: ',dataFName
           call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          endif
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(3a)')
     &      ' MDSREADFIELD: filename: ',dataFName, pfName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          write(msgbuf,'(a)')
     &      ' MDSREADFIELD: File does not exist'
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

#endif /* USE_W2 */

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

      endif
c     endif ( .not. ( globalFile .and. useSingleCPUIO ) )

      _END_MASTER( myThid )

      if ( globalFile .and. useSingleCPUIO ) then

#ifdef USE_W2
        write(msgbuf,'(a)')
     &         ' MDSREADFIELD: useSingleCPUIO not 
     &           implemented with exch2'
        call print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MDSREADFIELD'
#endif /* USE_W2 */

C master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
        IF( mpiMyId .EQ. 0 ) THEN
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
         length_of_rec=MDS_RECLEN( filePrec, Nx*Ny, mythid )
         open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
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
           read(dUnit,rec=irec) global_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( Nx*Ny, global_r4 )
#endif
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = global_r4(I,J)
            ENDDO
           ENDDO
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) global
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( Nx*Ny, global )
#endif
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
           call PASStoRS( local,arr,k,nNz,mythid )
        elseif (arrType .eq. 'RL') then
           call PASStoRL( local,arr,k,nNz,mythid )
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
c     endif ( globalFile .and. useSingleCPUIO )

      return
      end


C     ==================================================================

      subroutine passToRS(local,arr,k,nNz,mythid)
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
      _RL local(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nSx,nSy)
      integer i,j,k,bi,bj,nNz
      _RS arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy) 
      integer mythid
      DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
            DO J=1-Oly,sNy+Oly
               DO I=1-Olx,sNx+Olx
                  arr(I,J,k,bi,bj) = local(I,J,bi,bj)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      return
      end

      subroutine passToRL(local,arr,k,nNz,mythid)
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
      _RL local(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nSx,nSy)
      integer i,j,k,bi,bj,nNz
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      integer mythid
      DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
            DO J=1-Oly,sNy+Oly
               DO I=1-Olx,sNx+Olx
                  arr(I,J,k,bi,bj) = local(I,J,bi,bj)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      return
      end
