C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_slice_loc.F,v 1.6 2005/08/19 18:01:29 heimbach Exp $

#include "MDSIO_OPTIONS.h"

C=======================================================================
      SUBROUTINE MDSREADFIELDXZ_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
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
C form "fName.xxx.yyy.data" exist. 
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL". 
C This routine reads vertical slices (X-Z) including the overlap region.
C irecord is the record number to be read and must be >= 1. 
C The file data is stored in arr *but* the overlaps are *not* updated. 
C
C Created: 06/03/00 spk@ocean.mit.edu
C 

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

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
      character*(80) dataFName
      integer iG,jG,irec,bi,bj,k,dUnit,IL
      logical exst
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
cph(
      integer i, x_size
      PARAMETER ( x_size = Nx )
      Real*4 x_buffer_r4(x_size)
      Real*8 x_buffer_r8(x_size)
      Real*8 global   (Nx)
      _RL    local(1-OLx:sNx+OLx,nSx,nSy)
cph)
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADFIELDXZ: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADFIELDXZ: Invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
      endif

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

      if ( .not. useSingleCPUIO ) then

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       if ( debugLevel .GE. debLevA ) then
        write(msgbuf,'(a,a)')
     &   ' MDSREADFIELDXZ: opening global file: ',dataFName
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
     &    ' MDSREADFIELDXZ: opening global file: ',dataFName
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

C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
         if (exst) then
          if ( debugLevel .GE. debLevA ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADFIELDXZ: opening file: ',dataFName
           call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          endif
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(a,a)')
     &      ' MDSREADFIELDXZ: filename: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          write(msgbuf,'(a)')
     &      ' MDSREADFIELDXZ: File does not exist'
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
         endif
        endif

        if (fileIsOpen) then
         do k=1,nNz
           if (globalFile) then
            iG = myXGlobalLo-1 + (bi-1)*sNx
            jG = (myYGlobalLo-1)/sNy + (bj-1)
            irec=1 + INT(iG/sNx) + nSx*nPx*(k-1)
     &           + nSx*nPx*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=k + nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4(sNx,r4seg)
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS_2D(sNx,oLx,nNz,bi,bj,k,.TRUE.,r4seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL_2D(sNx,oLx,nNz,bi,bj,k,.TRUE.,r4seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELDXZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
            endif
           elseif (filePrec .eq. precFloat64) then
            read(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS_2D(sNx,oLx,nNz,bi,bj,k,.TRUE.,r8seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL_2D(sNx,oLx,nNz,bi,bj,k,.TRUE.,r8seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELDXZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
            endif
           else
            write(msgbuf,'(a)')
     &        ' MDSREADFIELDXZ: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
           endif
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
         inquire( file=dataFname, exist=exst )
         if (exst) globalFile = .TRUE.

C If negative check for global file with MDS name (ie. fName.data)
         if (.NOT. globalFile) then
          write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
          inquire( file=dataFname, exist=exst )
          if (exst) globalFile = .TRUE.
         endif

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         if ( globalFile) then
          length_of_rec=MDS_RECLEN( filePrec, x_size, mythid )
          open( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         else
          write(msgbuf,'(2a)') ' MDSREADFIELDXZ: filename: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELDXZ: File does not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
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
           read(dUnit,rec=irec) x_buffer_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size, x_buffer_r4 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO I=1,Nx
             global(I) = x_buffer_r4(I)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) x_buffer_r8
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( x_size, x_buffer_r8 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO I=1,Nx
             global(I) = x_buffer_r8(I)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          else
           write(msgbuf,'(a)')
     &            ' MDSREADFIELDXZ: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
          endif
         ENDIF
        _END_MASTER( myThid )
        CALL SCATTER_XZ(global,local,mythid)
        if (arrType .eq. 'RS') then
           call PASStoRSxz( local,arr,k,nNz,mythid )
        elseif (arrType .eq. 'RL') then
           call PASStoRLxz( local,arr,k,nNz,mythid )
        else
           write(msgbuf,'(a)')
     &          ' MDSREADFIELDXZ: illegal value for arrType'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELDXZ'
        endif

       ENDDO
c      ENDDO k=1,nNz

       _BEGIN_MASTER( myThid )
        close( dUnit )
       _END_MASTER( myThid )

      endif
c     endif ( useSingleCPUIO )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSREADFIELDYZ_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
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
C form "fName.xxx.yyy.data" exist. 
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL". 
C This routine reads vertical slices (Y-Z) including overlap regions.
C irecord is the record number to be read and must be >= 1. 
C The file data is stored in arr *but* the overlaps are *not* updated. 
C
C Created: 06/03/00 spk@ocean.mit.edu
C 

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

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
      character*(80) dataFName
      integer iG,jG,irec,bi,bj,k,dUnit,IL
      logical exst
      Real*4 r4seg(sNy)
      Real*8 r8seg(sNy)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
cph(
      integer j, y_size
      PARAMETER ( y_size = Ny )
      Real*4 y_buffer_r4(y_size)
      Real*8 y_buffer_r8(y_size)
      Real*8 global   (Ny)
      _RL    local(1-OLy:sNy+OLy,nSx,nSy)
cph)
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADFIELDYZ: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADFIELDYZ: Invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
      endif

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )

      if ( .not. useSingleCPUIO ) then

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       if ( debugLevel .GE. debLevA ) then
        write(msgbuf,'(a,a)')
     &   ' MDSREADFIELDYZ: opening global file: ',dataFName
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
     &    ' MDSREADFIELDYZ: opening global file: ',dataFName
         call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        endif
        globalFile = .TRUE.
       endif
      endif

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNy, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
         if (exst) then
          if ( debugLevel .GE. debLevA ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADFIELDYZ: opening file: ',dataFName
           call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          endif
          length_of_rec=MDS_RECLEN( filePrec, sNy, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(a,a)')
     &      ' MDSREADFIELDYZ: filename: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          write(msgbuf,'(a)')
     &      ' MDSREADFIELDYZ: File does not exist'
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
         endif
        endif

        if (fileIsOpen) then
         do k=1,nNz
           if (globalFile) then
            iG = (myXGlobalLo-1)/sNx + (bi-1)
            jG = myYGlobalLo-1 + (bj-1)*sNy
            irec=1 + INT(jG/sNy) + nSy*nPy*(k-1)
     &           + nSy*nPy*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=k + nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4(sNy,r4seg)
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS_2D(sNy,oLy,nNz,bi,bj,k,.TRUE.,r4seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL_2D(sNy,oLy,nNz,bi,bj,k,.TRUE.,r4seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELDYZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
            endif
           elseif (filePrec .eq. precFloat64) then
            read(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNy, r8seg )
#endif
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS_2D(sNy,oLy,nNz,bi,bj,k,.TRUE.,r8seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL_2D(sNy,oLy,nNz,bi,bj,k,.TRUE.,r8seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELDYZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
            endif
           else
            write(msgbuf,'(a)')
     &        ' MDSREADFIELDYZ: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
           endif
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
         inquire( file=dataFname, exist=exst )
         if (exst) globalFile = .TRUE.

C If negative check for global file with MDS name (ie. fName.data)
         if (.NOT. globalFile) then
          write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
          inquire( file=dataFname, exist=exst )
          if (exst) globalFile = .TRUE.
         endif

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         if ( globalFile) then
          length_of_rec=MDS_RECLEN( filePrec, y_size, mythid )
          open( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         else
          write(msgbuf,'(2a)') ' MDSREADFIELDYZ: filename: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELDYZ: File does not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
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
           read(dUnit,rec=irec) y_buffer_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( y_size, y_buffer_r4 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO J=1,Ny
             global(J) = y_buffer_r4(J)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) y_buffer_r8
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( y_size, y_buffer_r8 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO J=1,Ny
             global(J) = y_buffer_r8(J)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          else
           write(msgbuf,'(a)')
     &            ' MDSREADFIELDYZ: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
          endif
         ENDIF
        _END_MASTER( myThid )
        CALL SCATTER_YZ(global,local,mythid)
        if (arrType .eq. 'RS') then
           call PASStoRSyz( local,arr,k,nNz,mythid )
        elseif (arrType .eq. 'RL') then
           call PASStoRLyz( local,arr,k,nNz,mythid )
        else
           write(msgbuf,'(a)')
     &          ' MDSREADFIELDYZ: illegal value for arrType'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELDYZ'
        endif

       ENDDO
c      ENDDO k=1,nNz

       _BEGIN_MASTER( myThid )
        close( dUnit )
       _END_MASTER( myThid )

      endif
c     endif ( useSingleCPUIO )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSWRITEFIELDXZ_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
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
C C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNz		integer	size of second dimension: Nr
C arr		RL	array to write, arr(:,nNz,:,:)
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C MDSWRITEFIELDXZ creates either a file of the form "fName.data"  
C if the logical flag "globalFile" is set true. Otherwise
C it creates MDS tiled files of the form "fName.xxx.yyy.data".
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL".
C This routine writes vertical slices (X-Z) including overlap regions.
C irecord is the record number to be read and must be >= 1. 
C NOTE: It is currently assumed that
C the highest record number in the file was the last record written.
C
C Modified: 06/02/00 spk@ocean.mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      character*(2) arrType
      integer nNz
cph(
cph      Real arr(*)
      _RL arr(1-oLx:sNx+oLx,nNz,nSx,nSy)
cph)
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName
      integer iG,jG,irec,bi,bj,k,dUnit,IL
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
cph(
      character*(80) metaFName
      integer dimList(3,3),ndims
      integer i, x_size
      PARAMETER ( x_size = Nx )
      Real*4 x_buffer_r4(x_size)
      Real*8 x_buffer_r8(x_size)
      Real*8 global(Nx)
      _RL    local(1-OLx:sNx+OLx,nSx,nSy)
cph)
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEFIELDXZ: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELDXZ: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELDXZ'
      endif

C Assume nothing
      fileIsOpen=.FALSE.
      IL=ILNBLNK( fName )

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
         write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
         length_of_rec=MDS_RECLEN(filePrec,x_size,mythid)
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
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO I=1-Olx,sNx+Olx
            local(I,bi,bj) = arr(I,k,bi,bj)
           ENDDO
         ENDDO
        ENDDO
        CALL GATHER_XZ( global, local, myThid )
        _BEGIN_MASTER( myThid )
         IF( mpiMyId .EQ. 0 ) THEN
          irec=k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO I=1,Nx
             x_buffer_r4(I) = global(I)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size, x_buffer_r4 )
#endif
           write(dUnit,rec=irec) x_buffer_r4
          elseif (filePrec .eq. precFloat64) then
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO I=1,Nx
             x_buffer_r8(I) = global(I)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( x_size, x_buffer_r8 )
#endif
           write(dUnit,rec=irec) x_buffer_r8
          else
           write(msgbuf,'(a)')
     &       ' MDSWRITEFIELDXZ: illegal value for filePrec'
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
         write(metaFName(1:80),'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=x_size
         dimList(2,1)=1
         dimList(3,1)=x_size
         dimList(1,2)=1
         dimList(2,2)=1
         dimList(3,2)=1
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
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
        open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       endif
      endif

C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         if (irecord .EQ. 1) then
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status='old',
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         endif
        endif
        if (fileIsOpen) then
         do k=1,nNz
           if (globalFile) then
            iG = myXGlobalLo-1 + (bi-1)*sNx
            jG = (myYGlobalLo-1)/sNy + (bj-1)
            irec=1 + INT(iG/sNx) + nSx*nPx*(k-1)
     &           + nSx*nPx*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=k + nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS_2D(sNx,oLx,nNz,bi,bj,k,.FALSE.,r4seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL_2D(sNx,oLx,nNz,bi,bj,k,.FALSE.,r4seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELDXZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELDXZ'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4(sNx,r4seg)
#endif
            write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS_2D(sNx,oLx,nNz,bi,bj,k,.FALSE.,r8seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL_2D(sNx,oLx,nNz,bi,bj,k,.FALSE.,r8seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELDXZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELDXZ'
            endif              
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgbuf,'(a)')
     &        ' MDSWRITEFIELDXZ: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSWRITEFIELDXZ'
           endif
C End of k loop
         enddo
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEFIELDXZ: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEFIELDXZ'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen .AND. (.NOT. globalFile)) then
         close( dUnit )
         fileIsOpen = .FALSE.
        endif
C End of bi,bj loops
       enddo
      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

C We put a barrier here to ensure that all processes have finished
C writing their data before we update the meta-file
       _BARRIER

      _END_MASTER( myThid )

#ifdef ALLOW_USE_MPI
C endif useSingleCpuIO
      endif
#endif /* ALLOW_USE_MPI */

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSWRITEFIELDYZ_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
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
C C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNz		integer	size of second dimension: Nr
C arr		RL	array to write, arr(:,nNz,:,:)
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C MDSWRITEFIELDYZ creates either a file of the form "fName.data"  
C if the logical flag "globalFile" is set true. Otherwise
C it creates MDS tiled files of the form "fName.xxx.yyy.data".
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL".
C This routine writes vertical slices (Y-Z) including overlap regions.
C irecord is the record number to be read and must be >= 1. 
C NOTE: It is currently assumed that
C the highest record number in the file was the last record written.
C
C Modified: 06/02/00 spk@ocean.mit.edu


      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      character*(2) arrType
      integer nNz
cph(
cph      Real arr(*)
      _RL arr(1-oLy:sNy+oLy,nNz,nSx,nSy)
cph)
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName
      integer iG,jG,irec,bi,bj,k,dUnit,IL
      Real*4 r4seg(sNy)
      Real*8 r8seg(sNy)
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
cph(
      character*(80) metaFName
      integer dimList(3,3),ndims
      integer j, y_size
      PARAMETER ( y_size = Ny )
      Real*4 y_buffer_r4(y_size)
      Real*8 y_buffer_r8(y_size)
      Real*8 global(Ny)
      _RL    local(1-OLy:sNy+OLy,nSx,nSy)
cph)
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEFIELDYZ: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELDYZ: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELDYZ'
      endif

C Assume nothing
      fileIsOpen=.FALSE.
      IL=ILNBLNK( fName )

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
         write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
         length_of_rec=MDS_RECLEN(filePrec,y_size,mythid)
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
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO J=1-Oly,sNy+Oly
            local(J,bi,bj) = arr(J,k,bi,bj)
           ENDDO
         ENDDO
        ENDDO
        CALL GATHER_YZ( global, local, myThid )
        _BEGIN_MASTER( myThid )
         IF( mpiMyId .EQ. 0 ) THEN
          irec=k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO J=1,Ny
             y_buffer_r4(J) = global(J)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( y_size, y_buffer_r4 )
#endif
           write(dUnit,rec=irec) y_buffer_r4
          elseif (filePrec .eq. precFloat64) then
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
            DO J=1,Ny
             y_buffer_r8(J) = global(J)
            ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( y_size, y_buffer_r8 )
#endif
           write(dUnit,rec=irec) y_buffer_r8
          else
           write(msgbuf,'(a)')
     &       ' MDSWRITEFIELDYZ: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSWRITEFIELDYZ'
          endif
         ENDIF
        _END_MASTER( myThid )
       ENDDO

C Close data-file and create meta-file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         close( dUnit )
         write(metaFName(1:80),'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=1
         dimList(2,1)=1
         dimList(3,1)=1
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
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec=MDS_RECLEN( filePrec, sNy, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec=MDS_RECLEN( filePrec, sNy, mythid )
        open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       endif
      endif

C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         if (irecord .EQ. 1) then
          length_of_rec=MDS_RECLEN( filePrec, sNy, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec=MDS_RECLEN( filePrec, sNy, mythid )
          open( dUnit, file=dataFName, status='old',
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         endif
        endif
        if (fileIsOpen) then
         do k=1,nNz
           if (globalFile) then
            iG = (myXGlobalLo-1)/sNx + (bi-1)
            jG = myYGlobalLo-1 + (bj-1)*sNy
            irec=1 + INT(jG/sNy) + nSy*nPy*(k-1)
     &           + nSy*nPy*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=k + nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            if (arrType .eq. 'RS') then
             call MDS_SEG4toRS_2D(sNy,oLy,nNz,bi,bj,k,.FALSE.,r4seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL_2D(sNy,oLy,nNz,bi,bj,k,.FALSE.,r4seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELDYZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELDYZ'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4(sNy,r4seg)
#endif
            write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
             call MDS_SEG8toRS_2D(sNy,oLy,nNz,bi,bj,k,.FALSE.,r8seg,arr)
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL_2D(sNy,oLy,nNz,bi,bj,k,.FALSE.,r8seg,arr)
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELDYZ: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELDYZ'
            endif             
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNy, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgbuf,'(a)')
     &        ' MDSWRITEFIELDYZ: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSWRITEFIELDYZ'
           endif
C End of k loop
         enddo
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEFIELDYZ: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEFIELDYZ'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen .AND. (.NOT. globalFile)) then
         close( dUnit )
         fileIsOpen = .FALSE.
        endif
C End of bi,bj loops
       enddo
      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

C We put a barrier here to ensure that all processes have finished
C writing their data before we update the meta-file
       _BARRIER

      _END_MASTER( myThid )

#ifdef ALLOW_USE_MPI
C endif useSingleCpuIO
      endif
#endif /* ALLOW_USE_MPI */

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

