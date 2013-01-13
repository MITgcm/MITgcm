C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_gl.F,v 1.21 2013/01/13 22:43:53 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

C--  File mdsio_gl.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o MDSREADFIELD_3D_GL
C--   o MDSWRITEFIELD_3D_GL
C--   o MDSREADFIELD_2D_GL
C--   o MDSWRITEFIELD_2D_GL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELD_3D_GL(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     O   arr_gl,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to read
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C arrType   (char(2)) :: type of array "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr        (RS/RL)  :: array to read into, arr(:,:,nNz,:,:)
C irecord   (integer) :: record number to read
C myThid    (integer) :: thread identifier
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

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer nNz
      _RL arr_gl(sNx,nSx,nPx,sNy,nSy,nPy,Nr)
      integer irecord
      integer myThid

#ifdef ALLOW_CTRL

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName
      integer ip,jp,iG,jG,irec,bi,bj,i,j,k,dUnit,IL
      logical exst
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,Nr,nSx,nSy)
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
cph-usesingle(
      integer ii,jj
c     integer iG_IO,jG_IO,npe
      integer x_size,y_size
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 global(Nx,Ny)
c     _RL    local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cph-usesingle)
CMM(
      integer pIL
CMM)

C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

#ifndef REAL4_IS_SLOW
      if (arrType .eq. 'RS') then
       write(msgbuf,'(a)')
     &   ' MDSREADFIELD_GL is wrong for arrType="RS" (=real*4)'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
      endif
#endif

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADFIELD_GL: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADFIELD_GL: Invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
      endif

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )
CMM(
      pIL = ILNBLNK( mdsioLocalDir )
CMM)
CMM(
C Assign special directory
      if ( pIL.NE.0 ) then
       write(fName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      endif
CMM)

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

      if ( useSingleCPUIO ) then

#ifdef ALLOW_USE_MPI
        IF( myProcId .EQ. 0 ) THEN
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */

C Check first for global file with simple name (ie. fName)
         dataFName = fName
         inquire( file=dataFname, exist=exst )
         if (exst) globalFile = .TRUE.

C If negative check for global file with MDS name (ie. fName.data)
         if (.NOT. globalFile) then
          write(dataFname,'(2a)') fName(1:IL),'.data'
          inquire( file=dataFname, exist=exst )
          if (exst) globalFile = .TRUE.
         endif

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         if ( globalFile) then
          length_of_rec=MDS_RECLEN( filePrec, x_size*y_size, mythid )
          open( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         else
          write(msgbuf,'(2a)')
     &      ' MDSREADFIELD: filename: ',dataFName(1:IL)
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

c-- useSingleCpuIO
      else
C Only do I/O if I am the master thread

C Check first for global file with simple name (ie. fName)
       dataFName = fName
       inquire( file=dataFname, exist=exst )
       if (exst) then
        write(msgbuf,'(a,a)')
     &    ' MDSREADFIELD: opening global file: ',dataFName(1:IL)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
       endif

C If negative check for global file with MDS name (ie. fName.data)
       if (.NOT. globalFile) then
        write(dataFname,'(2a)') fName(1:IL),'.data'
        inquire( file=dataFname, exist=exst )
        if (exst) then
         write(msgbuf,'(a,a)')
     &     ' MDSREADFIELD_GL: opening global file: ',dataFName(1:IL+5)
         call print_message( msgbuf, standardmessageunit,
     &                       SQUEEZE_RIGHT , mythid)
         globalFile = .TRUE.
        endif
       endif

c-- useSingleCpuIO
      endif

      if ( .not. useSingleCpuIO ) then
cph      if ( .not. ( globalFile .and. useSingleCPUIO ) ) then
      if ( .not. ( globalFile ) ) then

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

C Loop over all processors
      do jp=1,nPy
      do ip=1,nPx
C Loop over all tiles
      do bj=1,nSy
      do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(ip-1)*nsx ! Kludge until unstructered tiles
         jG=bj+(jp-1)*nsy ! Kludge until unstructered tiles
         write(dataFname,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
         if (exst) then
          if ( debugLevel .GE. debLevB ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADFIELD_GL: opening file: ',dataFName(1:IL+13)
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
     &      ' MDSREADFIELD_GL: filename: ',dataFName(1:IL+13)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELD_GL: File does not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
         endif
        endif

        if (fileIsOpen) then
         do k=1,Nr
          do j=1,sNy
           if (globalFile) then
            iG=bi+(ip-1)*nsx
            jG=bj+(jp-1)*nsy
            irec=1 + INT(iG/sNx) + nSx*nPx*(jG+j-1) + nSx*nPx*Ny*(k-1)
     &             + nSx*nPx*Ny*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=j + sNy*(k-1) + sNy*Nr*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG4toRS( j,bi,bj,k,Nr, r4seg, .TRUE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,Nr, r4seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
            endif
           elseif (filePrec .eq. precFloat64) then
            read(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG8toRS( j,bi,bj,k,Nr, r8seg, .TRUE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,Nr, r8seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
            endif
           else
            write(msgbuf,'(a)')
     &        ' MDSREADFIELD_GL: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
           endif
       do ii=1,sNx
        arr_gl(ii,bi,ip,j,bj,jp,k)=arr(ii,j,k,bi,bj)
       enddo

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
C End of ip,jp loops
       enddo
      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

c      end of if ( .not. ( globalFile ) ) then
      endif

c      else of if ( .not. ( globalFile .and. useSingleCPUIO ) ) then
      else

       DO k=1,nNz

#ifdef ALLOW_USE_MPI
         IF( myProcId .EQ. 0 ) THEN
#else
         IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
          irec = k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
           read(dUnit,rec=irec) xy_buffer_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = xy_buffer_r4(I,J)
            ENDDO
           ENDDO
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) xy_buffer_r8
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( x_size*y_size, xy_buffer_r8 )
#endif
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = xy_buffer_r8(I,J)
            ENDDO
           ENDDO
          else
           write(msgbuf,'(a)')
     &            ' MDSREADFIELD: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELD'
          endif
         ENDIF
        DO jp=1,nPy
         DO ip=1,nPx
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO J=1,sNy
             JJ=((jp-1)*nSy+(bj-1))*sNy+J
             DO I=1,sNx
              II=((ip-1)*nSx+(bi-1))*sNx+I
              arr_gl(i,bi,ip,j,bj,jp,k) = global(II,JJ)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ENDDO
c      ENDDO k=1,nNz

        close( dUnit )

      endif
c      end of if ( .not. ( globalFile .and. useSingleCPUIO ) ) then

      _END_MASTER( myThid )

#else /* ALLOW_CTRL */
      STOP 'ABNORMAL END: S/R MDSREADFIELD_3D_GL is empty'
#endif /* ALLOW_CTRL */
C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELD_3D_GL(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     I   arr_gl,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C arrType   (char(2)) :: type of array "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr        (RS/RL)  :: array to write, arr(:,:,nNz,:,:)
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
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

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer nNz
cph(
cph      Real arr(*)
      _RL arr_gl(sNx,nSx,nPx,sNy,nSy,nPy,Nr)
cph)
      integer irecord
      integer myIter
      integer myThid

#ifdef ALLOW_CTRL

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,metaFName
      integer ip,jp,iG,jG,irec,bi,bj,i,j,k,dUnit,IL
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,Nr,nSx,nSy)
      INTEGER dimList(3,3), nDims, map2gl(2)
      _RL dummyRL(1)
      CHARACTER*8 blank8c
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
cph-usesingle(
#ifdef ALLOW_USE_MPI
      integer ii,jj
c     integer iG_IO,jG_IO,npe
      integer x_size,y_size
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 global(Nx,Ny)
#endif
cph-usesingle)
CMM(
      integer pIL
CMM)

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /

C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

#ifndef REAL4_IS_SLOW
      if (arrType .eq. 'RS') then
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELD_GL is wrong for arrType="RS" (=real*4)'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
      endif
#endif

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEFIELD_GL: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELD_GL: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
      endif

C Assume nothing
      fileIsOpen=.FALSE.
      IL=ILNBLNK( fName )
CMM(
      pIL = ILNBLNK( mdsioLocalDir )
CMM)
CMM(
C Assign special directory
      if ( pIL.NE.0 ) then
       write(fName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      endif
CMM)

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

cph-usesingle(
#ifdef ALLOW_USE_MPI
      _END_MASTER( myThid )
C If option globalFile is desired but does not work or if
C globalFile is too slow, then try using single-CPU I/O.
      if (useSingleCpuIO) then

C Master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
        IF( myProcId .EQ. 0 ) THEN
         write(dataFname,'(2a)') fName(1:IL),'.data'
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
C Loop over all processors
        do jp=1,nPy
        do ip=1,nPx
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO J=1,sNy
           JJ=((jp-1)*nSy+(bj-1))*sNy+J
           DO I=1,sNx
            II=((ip-1)*nSx+(bi-1))*sNx+I
            global(II,JJ) = arr_gl(i,bi,ip,j,bj,jp,k)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        enddo
        enddo
        _BEGIN_MASTER( myThid )
         IF( myProcId .EQ. 0 ) THEN
          irec=k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r4(I,J) = global(I,J)
            ENDDO
           ENDDO
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
           write(dUnit,rec=irec) xy_buffer_r4
          elseif (filePrec .eq. precFloat64) then
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r8(I,J) = global(I,J)
            ENDDO
           ENDDO
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
        IF( myProcId .EQ. 0 ) THEN
         close( dUnit )
         write(metaFName,'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=Nx
         dimList(2,1)=1
         dimList(3,1)=Nx
         dimList(1,2)=Ny
         dimList(2,2)=1
         dimList(3,2)=Ny
         dimList(1,3)=nNz
         dimList(2,3)=1
         dimList(3,3)=nNz
         nDims=3
         if (nNz .EQ. 1) nDims=2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
        ENDIF
       _END_MASTER( myThid )
C To be safe, make other processes wait for I/O completion
       _BARRIER

      elseif ( .NOT. useSingleCpuIO ) then
      _BEGIN_MASTER( myThid )
#endif /* ALLOW_USE_MPI */
cph-usesingle)

C Loop over all processors
      do jp=1,nPy
      do ip=1,nPx
C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
         iG=bi+(ip-1)*nsx ! Kludge until unstructered tiles
         jG=bj+(jp-1)*nsy ! Kludge until unstructered tiles
         write(dataFname,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
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
        if (fileIsOpen) then
         do k=1,Nr
          do j=1,sNy
             do i=1,sNx
                arr(i,j,k,bi,bj)=arr_gl(i,bi,ip,j,bj,jp,k)
             enddo
            iG = 0
            jG = 0
            irec=j + sNy*(k-1) + sNy*Nr*(irecord-1)
           if (filePrec .eq. precFloat32) then
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG4toRS( j,bi,bj,k,Nr, r4seg, .FALSE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,Nr, r4seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG8toRS( j,bi,bj,k,Nr, r8seg, .FALSE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,Nr, r8seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgbuf,'(a)')
     &        ' MDSWRITEFIELD_GL: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEFIELD_GL: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen) then
         close( dUnit )
         fileIsOpen = .FALSE.
        endif
C Create meta-file for each tile if we are tiling
         iG=bi+(ip-1)*nsx ! Kludge until unstructered tiles
         jG=bj+(jp-1)*nsy ! Kludge until unstructered tiles
         write(metaFname,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.meta'
         dimList(1,1)=Nx
         dimList(2,1)=((ip-1)*nSx+(bi-1))*sNx+1
         dimList(3,1)=((ip-1)*nSx+bi)*sNx
         dimList(1,2)=Ny
         dimList(2,2)=((jp-1)*nSy+(bj-1))*sNy+1
         dimList(3,2)=((jp-1)*nSy+bj)*sNy
         dimList(1,3)=Nr
         dimList(2,3)=1
         dimList(3,3)=Nr
         nDims=3
         if (Nr .EQ. 1) nDims=2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
C End of bi,bj loops
       enddo
      enddo
C End of ip,jp loops
       enddo
      enddo

      _END_MASTER( myThid )

cph-usesingle(
#ifdef ALLOW_USE_MPI
C endif useSingleCpuIO
      endif
#endif /* ALLOW_USE_MPI */
cph-usesingle)

#else /* ALLOW_CTRL */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELD_3D_GL is empty'
#endif /* ALLOW_CTRL */
C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELD_2D_GL(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     O   arr_gl,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to read
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C arrType   (char(2)) :: type of array "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr        (RS/RL)  :: array to read into, arr(:,:,nNz,:,:)
C irecord   (integer) :: record number to read
C myThid    (integer) :: thread identifier
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

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer nNz, nLocz
      parameter (nLocz = 1)
      _RL arr_gl(sNx,nSx,nPx,sNy,nSy,nPy,nLocz)
      integer irecord
      integer myThid

#ifdef ALLOW_CTRL

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName
      integer ip,jp,iG,jG,irec,bi,bj,i,j,k,dUnit,IL
      logical exst
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nLocz,nSx,nSy)
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
cph-usesingle(
      integer ii,jj
c     integer iG_IO,jG_IO,npe
      integer x_size,y_size
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 global(Nx,Ny)
c     _RL    local(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cph-usesingle)
CMM(
      integer pIL
CMM)

C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

#ifndef REAL4_IS_SLOW
      if (arrType .eq. 'RS') then
       write(msgbuf,'(a)')
     &   ' MDSREADFIELD_GL is wrong for arrType="RS" (=real*4)'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
      endif
#endif

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADFIELD_GL: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADFIELD_GL: Invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
      endif

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )
CMM(
      pIL = ILNBLNK( mdsioLocalDir )
CMM)
CMM(
C Assign special directory
      if ( pIL.NE.0 ) then
       write(fName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      endif
CMM)

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

      if ( useSingleCPUIO ) then

C master thread of process 0, only, opens a global file
#ifdef ALLOW_USE_MPI
        IF( myProcId .EQ. 0 ) THEN
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */

C Check first for global file with simple name (ie. fName)
         dataFName = fName
         inquire( file=dataFname, exist=exst )
         if (exst) globalFile = .TRUE.

C If negative check for global file with MDS name (ie. fName.data)
         if (.NOT. globalFile) then
          write(dataFname,'(2a)') fName(1:IL),'.data'
          inquire( file=dataFname, exist=exst )
          if (exst) globalFile = .TRUE.
         endif

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         if ( globalFile) then
          length_of_rec=MDS_RECLEN( filePrec, x_size*y_size, mythid )
          open( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         else
          write(msgbuf,'(2a)')
     &      ' MDSREADFIELD: filename: ',dataFName(1:IL)
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

c-- useSingleCpuIO
      else

C Check first for global file with simple name (ie. fName)
       dataFName = fName
       inquire( file=dataFname, exist=exst )
       if (exst) then
        write(msgbuf,'(a,a)')
     &    ' MDSREADFIELD: opening global file: ',dataFName(1:IL)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
       endif

C If negative check for global file with MDS name (ie. fName.data)
       if (.NOT. globalFile) then
        write(dataFname,'(2a)') fName(1:IL),'.data'
        inquire( file=dataFname, exist=exst )
        if (exst) then
         write(msgbuf,'(a,a)')
     &     ' MDSREADFIELD_GL: opening global file: ',dataFName(1:IL+5)
         call print_message( msgbuf, standardmessageunit,
     &                       SQUEEZE_RIGHT , mythid)
         globalFile = .TRUE.
        endif
       endif

c-- useSingleCpuIO
      endif

      if ( .not. useSingleCpuIO ) then
cph      if ( .not. ( globalFile .and. useSingleCPUIO ) ) then
      if ( .not. ( globalFile ) ) then

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

C Loop over all processors
      do jp=1,nPy
      do ip=1,nPx
C Loop over all tiles
      do bj=1,nSy
      do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(ip-1)*nsx ! Kludge until unstructered tiles
         jG=bj+(jp-1)*nsy ! Kludge until unstructered tiles
         write(dataFname,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
         if (exst) then
          if ( debugLevel .GE. debLevB ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADFIELD_GL: opening file: ',dataFName(1:IL+13)
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
     &      ' MDSREADFIELD_GL: filename: ',dataFName(1:IL+13)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELD_GL: File does not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
         endif
        endif

        if (fileIsOpen) then
         do k=1,nLocz
          do j=1,sNy
           if (globalFile) then
            iG=bi+(ip-1)*nsx
            jG=bj+(jp-1)*nsy
            irec=1 + INT(iG/sNx) + nSx*nPx*(jG+j-1) + nSx*nPx*Ny*(k-1)
     &             + nSx*nPx*Ny*nLocz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=j + sNy*(k-1) + sNy*nLocz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG4toRS( j,bi,bj,k,nLocz, r4seg, .TRUE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nLocz, r4seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
            endif
           elseif (filePrec .eq. precFloat64) then
            read(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG8toRS( j,bi,bj,k,nLocz, r8seg, .TRUE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nLocz, r8seg, .TRUE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSREADFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
            endif
           else
            write(msgbuf,'(a)')
     &        ' MDSREADFIELD_GL: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADFIELD_GL'
           endif
       do ii=1,sNx
        arr_gl(ii,bi,ip,j,bj,jp,k)=arr(ii,j,k,bi,bj)
       enddo

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
C End of ip,jp loops
       enddo
      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

c      end of if ( .not. ( globalFile ) ) then
      endif

c      else of if ( .not. ( globalFile .and. useSingleCPUIO ) ) then
      else

       DO k=1,nLocz

#ifdef ALLOW_USE_MPI
         IF( myProcId .EQ. 0 ) THEN
#else
         IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
          irec = k+nNz*(irecord-1)
          if (filePrec .eq. precFloat32) then
           read(dUnit,rec=irec) xy_buffer_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = xy_buffer_r4(I,J)
            ENDDO
           ENDDO
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) xy_buffer_r8
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( x_size*y_size, xy_buffer_r8 )
#endif
           DO J=1,Ny
            DO I=1,Nx
             global(I,J) = xy_buffer_r8(I,J)
            ENDDO
           ENDDO
          else
           write(msgbuf,'(a)')
     &            ' MDSREADFIELD: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELD'
          endif
         ENDIF
        DO jp=1,nPy
         DO ip=1,nPx
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO J=1,sNy
             JJ=((jp-1)*nSy+(bj-1))*sNy+J
             DO I=1,sNx
              II=((ip-1)*nSx+(bi-1))*sNx+I
              arr_gl(i,bi,ip,j,bj,jp,k) = global(II,JJ)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ENDDO
c      ENDDO k=1,nNz

        close( dUnit )

      endif
c      end of if ( .not. ( globalFile .and. useSingleCPUIO ) ) then

      _END_MASTER( myThid )

#else /* ALLOW_CTRL */
      STOP 'ABNORMAL END: S/R MDSREADFIELD_2D_GL is empty'
#endif /* ALLOW_CTRL */
C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELD_2D_GL(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     I   arr_gl,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C arrType   (char(2)) :: type of array "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr        (RS/RL)  :: array to write, arr(:,:,nNz,:,:)
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
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

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer nNz, nLocz
      parameter (nLocz = 1)
cph(
cph      Real arr(*)
      _RL arr_gl(sNx,nSx,nPx,sNy,nSy,nPy,nLocz)
cph)
      integer irecord
      integer myIter
      integer myThid

#ifdef ALLOW_CTRL

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,metaFName
      integer ip,jp,iG,jG,irec,bi,bj,i,j,k,dUnit,IL
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nLocz,nSx,nSy)
      INTEGER dimList(3,3), nDims, map2gl(2)
      _RL dummyRL(1)
      CHARACTER*8 blank8c
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
cph-usesingle(
#ifdef ALLOW_USE_MPI
      integer ii,jj
c     integer iG_IO,jG_IO,npe
      integer x_size,y_size
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 global(Nx,Ny)
#endif
cph-usesingle)
CMM(
      integer pIL
CMM)

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /

C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

#ifndef REAL4_IS_SLOW
      if (arrType .eq. 'RS') then
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELD_GL is wrong for arrType="RS" (=real*4)'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
      endif
#endif

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEFIELD_GL: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEFIELD_GL: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
      endif

C Assume nothing
      fileIsOpen=.FALSE.
      IL=ILNBLNK( fName )
CMM(
      pIL = ILNBLNK( mdsioLocalDir )
CMM)
CMM(
C Assign special directory
      if ( pIL.NE.0 ) then
       write(fName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
      endif
CMM)

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )


cph-usesingle(
#ifdef ALLOW_USE_MPI
      _END_MASTER( myThid )
C If option globalFile is desired but does not work or if
C globalFile is too slow, then try using single-CPU I/O.
      if (useSingleCpuIO) then

C Master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
        IF( myProcId .EQ. 0 ) THEN
         write(dataFname,'(2a)') fName(1:IL),'.data'
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
       DO k=1,nLocz
C Loop over all processors
        do jp=1,nPy
        do ip=1,nPx
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO J=1,sNy
           JJ=((jp-1)*nSy+(bj-1))*sNy+J
           DO I=1,sNx
            II=((ip-1)*nSx+(bi-1))*sNx+I
            global(II,JJ) = arr_gl(i,bi,ip,j,bj,jp,k)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        enddo
        enddo
        _BEGIN_MASTER( myThid )
         IF( myProcId .EQ. 0 ) THEN
          irec=k+nLocz*(irecord-1)
          if (filePrec .eq. precFloat32) then
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r4(I,J) = global(I,J)
            ENDDO
           ENDDO
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
           write(dUnit,rec=irec) xy_buffer_r4
          elseif (filePrec .eq. precFloat64) then
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r8(I,J) = global(I,J)
            ENDDO
           ENDDO
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
        IF( myProcId .EQ. 0 ) THEN
         close( dUnit )
         write(metaFName,'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=Nx
         dimList(2,1)=1
         dimList(3,1)=Nx
         dimList(1,2)=Ny
         dimList(2,2)=1
         dimList(3,2)=Ny
         dimList(1,3)=nLocz
         dimList(2,3)=1
         dimList(3,3)=nLocz
         nDims=3
         if (nLocz .EQ. 1) nDims=2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
        ENDIF
       _END_MASTER( myThid )
C To be safe, make other processes wait for I/O completion
       _BARRIER

      elseif ( .NOT. useSingleCpuIO ) then
      _BEGIN_MASTER( myThid )
#endif /* ALLOW_USE_MPI */
cph-usesingle)

C Loop over all processors
      do jp=1,nPy
      do ip=1,nPx
C Loop over all tiles
      do bj=1,nSy
       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
         iG=bi+(ip-1)*nsx ! Kludge until unstructered tiles
         jG=bj+(jp-1)*nsy ! Kludge until unstructered tiles
         write(dataFname,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
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
        if (fileIsOpen) then
         do k=1,nLocz
          do j=1,sNy
             do i=1,sNx
                arr(i,j,k,bi,bj)=arr_gl(i,bi,ip,j,bj,jp,k)
             enddo
            iG = 0
            jG = 0
            irec=j + sNy*(k-1) + sNy*nLocz*(irecord-1)
           if (filePrec .eq. precFloat32) then
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG4toRS( j,bi,bj,k,nLocz, r4seg, .FALSE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nLocz, r4seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4( sNx, r4seg )
#endif
            write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
#ifdef REAL4_IS_SLOW
             call MDS_SEG8toRS( j,bi,bj,k,nLocz, r8seg, .FALSE., arr )
#endif
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nLocz, r8seg, .FALSE., arr )
            else
             write(msgbuf,'(a)')
     &         ' MDSWRITEFIELD_GL: illegal value for arrType'
             call print_error( msgbuf, mythid )
             stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
            endif
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8( sNx, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgbuf,'(a)')
     &        ' MDSWRITEFIELD_GL: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEFIELD_GL: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEFIELD_GL'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen) then
         close( dUnit )
         fileIsOpen = .FALSE.
        endif
C Create meta-file for each tile if we are tiling
         iG=bi+(ip-1)*nsx ! Kludge until unstructered tiles
         jG=bj+(jp-1)*nsy ! Kludge until unstructered tiles
         write(metaFname,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.meta'
         dimList(1,1)=Nx
         dimList(2,1)=((ip-1)*nSx+(bi-1))*sNx+1
         dimList(3,1)=((ip-1)*nSx+bi)*sNx
         dimList(1,2)=Ny
         dimList(2,2)=((jp-1)*nSy+(bj-1))*sNy+1
         dimList(3,2)=((jp-1)*nSy+bj)*sNy
         dimList(1,3)=Nr
         dimList(2,3)=1
         dimList(3,3)=Nr
         nDims=3
         if (nLocz .EQ. 1) nDims=2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
C End of bi,bj loops
       enddo
      enddo
C End of ip,jp loops
       enddo
      enddo

      _END_MASTER( myThid )

#ifdef ALLOW_USE_MPI
C endif useSingleCpuIO
      endif
#endif /* ALLOW_USE_MPI */

#else /* ALLOW_CTRL */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELD_2D_GL is empty'
#endif /* ALLOW_CTRL */
C     ------------------------------------------------------------------
      RETURN
      END
