C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/mdsio.F,v 1.8 2000/11/13 16:18:21 heimbach Exp $

#include "CPP_OPTIONS.h"

C The five "public" routines supplied here are:
C
C MDSREADFIELD   - read model field from direct access global or tiled MDS file
C MDSWRITEFIELD  - write model field to direct access global or tiled MDS file
C MDSFINDUNIT    - returns an available (unused) I/O channel
C MDSREADVECTOR  - read vector from direct access global or tiled MDS file
C MDSWRITEVECTOR - write vector to direct access global or tiled MDS file
C
C all other routines are "private" to these utilities and ought
C not be accessed directly from the main code.
C
C Created:  03/16/99 adcroft@mit.edu
C Modified: 03/23/99 adcroft@mit.edu
C           To work with multiple records
C Modified: 03/29/99 eckert@mit.edu
C           Added arbitrary vector capability
C Modified: 07/27/99 eckert@mit.edu
C           Customized for state estimation (--> active_file_control.F)
C           this relates only to *mdsreadvector* and *mdswritevector*
C Modified: 07/28/99 eckert@mit.edu
C           inserted calls to *print_message* and *print_error*
C
C To be modified to work with MITgcmuv message routines.

#undef  SAFE_IO

#ifdef SAFE_IO
#define _NEW_STATUS 'new'
#else
#define _NEW_STATUS 'unknown'
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#define _OLD_STATUS 'unknown'
#else
#define _OLD_STATUS 'old'
#endif


C=======================================================================
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
      integer iG,jG,irec,bi,bj,j,k,dUnit,IL
      logical exst
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
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
      IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       write(msgbuf,'(a,a)')
     &   ' MDSREADFIELD: opening global file: ',dataFName
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       globalFile = .TRUE.
      endif

C If negative check for global file with MDS name (ie. fName.data)
      if (.NOT. globalFile) then
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       inquire( file=dataFname, exist=exst )
       if (exst) then
        write(msgbuf,'(a,a)')
     &    ' MDSREADFIELD: opening global file: ',dataFName
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
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
          write(msgbuf,'(a,a)')
     &      ' MDSREADFIELD: opening file: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          length_of_rec=MDS_RECLEN( filePrec, sNx, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(a,a)')
     &      ' MDSREADFIELD: filename: ',dataFName
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

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
       close( dUnit )
       fileIsOpen = .FALSE.
      endif

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSWRITEFIELD(
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
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNz		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to write, arr(:,:,nNz,:,:)
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

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      character*(2) arrType
      integer nNz
cph(
cph      Real arr(*)
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
cph)
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName,metaFName
      integer iG,jG,irec,bi,bj,j,k,dUnit,IL
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      integer dimList(3,3),ndims
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
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
      IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

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
        open( dUnit, file=dataFName, status=_OLD_STATUS,
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
             call MDS_SEG4toRS( j,bi,bj,k,nNz, r4seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG4toRL( j,bi,bj,k,nNz, r4seg, .FALSE., arr )
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
             call MDS_SEG8toRS( j,bi,bj,k,nNz, r8seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             call MDS_SEG8toRL( j,bi,bj,k,nNz, r8seg, .FALSE., arr )
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
         write(metaFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.meta'
         dimList(1,1)=Nx
         dimList(2,1)=myXGlobalLo+(bi-1)*sNx
         dimList(3,1)=myXGlobalLo+bi*sNx-1
         dimList(1,2)=Ny
         dimList(2,2)=myYGlobalLo+(bj-1)*sNy
         dimList(3,2)=myYGlobalLo+bj*sNy-1
         dimList(1,3)=Nr
         dimList(2,3)=1
         dimList(3,3)=Nr
         ndims=3
         if (nNz .EQ. 1) ndims=2
         call MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, mythid )
        endif
C End of bi,bj loops
       enddo
      enddo

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
       write(metaFName(1:80),'(2a)') fName(1:IL),'.meta'
       dimList(1,1)=Nx
       dimList(2,1)=1
       dimList(3,1)=Nx
       dimList(1,2)=Ny
       dimList(2,2)=1
       dimList(3,2)=Ny
       dimList(1,3)=Nr
       dimList(2,3)=1
       dimList(3,3)=Nr
       ndims=3
       if (nNz .EQ. 1) ndims=2
       call MDSWRITEMETA( metaFName, dataFName,
     &   filePrec, ndims, dimList, irecord, myIter, mythid )
       fileIsOpen=.TRUE.
      endif

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG4toRS( j,bi,bj,k,nNz, seg, copyTo, arr )
C IN:
C     j,bi,bj,k	integer - indices to array "arr"
C     nNz	integer - K dimension of array "arr"
C     seg	Real*4  - 1-D vector of length sNx
C OUT:
C     arr	_RS     - model tiled array
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer j,bi,bj,k,nNz
      _RS arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      logical copyTo
      Real*4 seg(sNx)
C Local
      integer ii

C     ------------------------------------------------------------------

      if (copyTo) then
       do ii=1,sNx
        arr(ii,j,k,bi,bj)=seg(ii)
       enddo
      else
       do ii=1,sNx
        seg(ii)=arr(ii,j,k,bi,bj)
       enddo
      endif

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG4toRL( j,bi,bj,k,nNz, seg, copyTo, arr )
C IN:
C     j,bi,bj,k	integer - indices to array "arr"
C     nNz	integer - K dimension of array "arr"
C     seg	Real*4  - 1-D vector of length sNx
C OUT:
C     arr	_RL     - model tiled array
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer j,bi,bj,k,nNz
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      logical copyTo
      Real*4 seg(sNx)
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1,sNx
        arr(ii,j,k,bi,bj)=seg(ii)
       enddo
      else
       do ii=1,sNx
        seg(ii)=arr(ii,j,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG8toRS( j,bi,bj,k,nNz, seg, copyTo, arr )
C IN:
C     j,bi,bj,k	integer - indices to array "arr"
C     nNz	integer - K dimension of array "arr"
C     seg	Real*8  - 1-D vector of length sNx
C OUT:
C     arr	_RS     - model tiled array
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer j,bi,bj,k,nNz
      _RS arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      logical copyTo
      Real*8 seg(sNx)
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1,sNx
        arr(ii,j,k,bi,bj)=seg(ii)
       enddo
      else
       do ii=1,sNx
        seg(ii)=arr(ii,j,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG8toRL( j,bi,bj,k,nNz, seg, copyTo, arr )
C IN:
C     j,bi,bj,k	integer - indices to array "arr"
C     nNz	integer - K dimension of array "arr"
C     seg	Real*8  - 1-D vector of length sNx
C OUT:
C     arr	_RL     - model tiled array
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer j,bi,bj,k,nNz
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      logical copyTo
      Real*8 seg(sNx)
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1,sNx
        arr(ii,j,k,bi,bj)=seg(ii)
       enddo
      else
       do ii=1,sNx
        seg(ii)=arr(ii,j,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDSWRITEMETA(
     I    mFileName,
     I    dFileName,
     I    filePrec,
     I    ndims,
     I    dimList,
     I    nrecords,
     I    myIter,
     I    mythid )

C IN:
C     mFileName	string  - complete name of meta-file
C     dFileName	string  - complete name of data-file
C     ndims	integer - number of dimensions
C     dimList	integer - array of dimensions, etc.
C     nrecords	integer - record number
C     myIter	integer - time-step number
C     mythid    integer - thread id
C OUT:
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Arguments
      character*(*) mFileName
      character*(*) dFileName
      integer filePrec
      integer ndims
      integer dimList(3,ndims)
      integer nrecords
      integer myIter
      integer mythid

C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Functions

C Local
      integer i,ii,mUnit
      logical ex
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C We should *read* the met-file if it exists to check
C that the information we are writing is consistent
C with the current contents
      inquire( file=mFileName, exist=ex )
C However, it is bloody difficult to parse files
C in fortran so someone else can do this.
C For now, we will assume everything is ok
C and that the last record is written to the
C last consecutive record in the file.

C Assign a free unit number as the I/O channel for this subroutine
      call MDSFINDUNIT( mUnit, mythid )

C Open meta-file
      open( mUnit, file=mFileName, status='unknown',
     &    form='formatted' )

C Write the number of dimensions
      write(mUnit,'(x,a,i3,a)') 'nDims = [ ',ndims,' ];'

C For each dimension, write the following:
C   1  global size  (ie. the size of the global dimension of all files)
C   2  global start (ie. the global position of the start of this file)
C   3  global end   (ie. the global position of the end   of this file)

      write(mUnit,'(x,a)') 'dimList = ['
      do ii=1,ndims
       if (ii.lt.ndims) then
        write(mUnit,'(10x,3(i5,","))') (dimList(i,ii),i=1,3)
       else
        write(mUnit,'(10x,i5,",",i5,",",i5)') (dimList(i,ii),i=1,3)
       endif
      enddo
      write(mUnit,'(10x,a)') '];'

C Record the precision of the file
      if (filePrec .EQ. precFloat32) then
       write(mUnit,'(x,a)') "format = [ 'float32' ];"
      elseif (filePrec .EQ. precFloat64) then
       write(mUnit,'(x,a)') "format = [ 'float64' ];"
      else
       write(msgbuf,'(a)')
     &   ' MDSWRITEMETA: invalid filePrec'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEMETA'
      endif

C Record the current record number
C This is a proxy for the actual number of records in the file.
C If we could read the file then we could do this properly.
      write(mUnit,'(x,a,i5,a)') 'nrecords = [ ',nrecords,' ];'

C Record the file-name for the binary data
Cveto ii=ILNBLNK( dFileName )
Cveto write(mUnit,'(x,3a)') 'binarydatafile = [ ',dFileName(1:ii),' ];'

C Write the integer time (integer iteration number) for later record
C keeping. If the timestep number is less than 0 then we assume
C that the information is superfluous and do not write it.
      if (myIter .ge. 0)
     & write(mUnit,'(x,a,i8,a)') 'timeStepNumber = [ ',myIter,' ];'

C Close meta-file
      close(mUnit)
      
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDSFINDUNIT( iounit, mythid )
C OUT:
C     iounit   integer - unit number
C
C MDSFINDUNIT returns a valid, unused unit number for f77 I/O
C The routine stops the program is an error occurs in the process
C of searching the I/O channels.
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer iounit
      integer mythid
C Local
      integer ii
      logical op
      integer ios
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C Sweep through a valid range of unit numbers
      iounit=-1
      do ii=9,99
        if (iounit.eq.-1) then
          inquire(unit=ii,iostat=ios,opened=op)
          if (ios.ne.0) then
            write(msgbuf,'(a,i2.2)')
     &        ' MDSFINDUNIT: inquiring unit number = ',ii
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a)')
     &        ' MDSFINDUNIT: inquire statement failed!'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSFINDUNIT'
          endif
          if (.NOT. op) then
            iounit=ii
          endif
        endif
      enddo

C Was there an available unit number
      if (iounit.eq.-1) then
        write(msgbuf,'(a)')
     &    ' MDSFINDUNIT: could not find an available unit number!'
        call print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MDSFINDUNIT'
      endif

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      integer function MDS_RECLEN( filePrec, nnn, mythid )
C IN:
C   filePrec	integer - precision of file in bits
C   nnn		integer - number of elements in record
C OUT:
C   MDS_RECLEN  integer - appropriate length of record in bytes or words
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none
C Arguments
      integer filePrec
      integer nnn
      integer mythid
C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

      if (filePrec .EQ. precFloat32) then
       MDS_RECLEN=nnn*WORDLENGTH
      elseif (filePrec .EQ. precFloat64) then
       MDS_RECLEN=nnn*WORDLENGTH*2
      else
       write(msgbuf,'(a,i2.2)')
     &   ' MDS_RECLEN: filePrec = ',filePrec
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDS_RECLEN: illegal value for filePrec'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDS_RECLEN'
      endif

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSREADVECTOR(
     I   fName,
     I   filePrec,
     I   arrType,
     I   narr,
     O   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to read
C filePrec	integer	number of bits per word in file (32 or 64)
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C narr		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to read into, arr(narr)
ce bi           integer x tile index
ce bj           integer y tile index
C irecord	integer	record number to read
C myThid	integer thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer narr
      Real arr(narr)
      integer irecord
      integer myThid
ce
      integer bi,bj
ce

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName
      integer iG,jG,irec,dUnit,IL
      logical exst
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADVECTOR: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADVECTOR: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADVECTOR'
      endif

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       write(msgbuf,'(a,a)')
     &   ' MDSREADVECTOR: opening global file: ',dataFName
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       globalFile = .TRUE.
      endif

C If negative check for global file with MDS name (ie. fName.data)
      if (.NOT. globalFile) then
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       inquire( file=dataFname, exist=exst )
       if (exst) then
         write(msgbuf,'(a,a)')
     &     ' MDSREADVECTOR: opening global file: ',dataFName
         call print_message( msgbuf, standardmessageunit,
     &                       SQUEEZE_RIGHT , mythid)
        globalFile = .TRUE.
       endif
      endif

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, narr, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

C Loop over all tiles
ce      do bj=1,nSy
ce       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism)
         if (exst) then
          write(msgbuf,'(a,a)')
     &      ' MDSREADVECTOR: opening file: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          length_of_rec=MDS_RECLEN( filePrec, narr, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(a)')
     &      ' MDSREADVECTOR: un-active tiles not implemented yet'
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADVECTOR'
         endif
        endif
        if (fileIsOpen) then
          if (globalFile) then
            iG   = myXGlobalLo-1+(bi-1)*sNx
            jG   = myYGlobalLo-1+(bj-1)*sNy
            irec = 1 + int(iG/sNx) + (jG/sNy)*nSx*nPx +
     &             (irecord-1)*nSx*nPx*nSy*nPy
          else
            iG   = 0
            jG   = 0
            irec = irecord
          endif
          if (filePrec .eq. precFloat32) then
           call MDS_READ_RS_VEC( dUnit, irec, narr, arr, myThid )
          elseif (filePrec .eq. precFloat64) then
           call MDS_READ_RL_VEC( dUnit, irec, narr, arr, myThid )
          else
            write(msgbuf,'(a)')
     &        ' MDSREADVECTOR: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADVECTOR'
          endif
          if (.NOT. globalFile) then
            close( dUnit )
            fileIsOpen = .FALSE.
          endif
        endif
C End of bi,bj loops
ce       enddo
ce      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
        close( dUnit )
        fileIsOpen = .FALSE.
      endif

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSWRITEVECTOR(
     I   fName,
     I   filePrec,
     I   globalfile,
     I   arrType,
     I   narr,
     I   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myIter,
     I   myThid )
C Arguments:
C
C fName		string	base name for file to written
C filePrec	integer	number of bits per word in file (32 or 64)
C globalFile	logical	selects between writing a global or tiled file
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C narr		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to write, arr(narr)
ce bi           integer x tile index
ce bj           integer y tile index
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)
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
      logical globalfile
      character*(2) arrType
      integer narr
      Real arr(narr)
      integer irecord
      integer myIter
      integer myThid
ce
      integer bi,bj
ce

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName,metaFName
      integer iG,jG,irec,dUnit,IL
      logical fileIsOpen
      integer dimList(3,3),ndims
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEVECTOR: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEVECTOR: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEVECTOR'
      endif

C Assume nothing
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

C If we are writing to a global file then we open it here
      if (globalFile) then
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
        open( dUnit, file=dataFName, status=_OLD_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       endif
      endif

C Loop over all tiles
ce      do bj=1,nSy
ce       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         if (irecord .EQ. 1) then
          length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
          open( dUnit, file=dataFName, status=_OLD_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         endif
        endif
        if (fileIsOpen) then
          if (globalFile) then
            iG   = myXGlobalLo-1+(bi-1)*sNx
            jG   = myYGlobalLo-1+(bj-1)*sNy
            irec = 1 + int(iG/sNx) + (jG/sNy)*nSx*nPx +
     &             (irecord-1)*nSx*nPx*nSy*nPy
          else
            iG   = 0
            jG   = 0
            irec = irecord
          endif
          if (filePrec .eq. precFloat32) then
           call MDS_WRITE_RS_VEC( dUnit, irec, narr, arr, myThid )
          elseif (filePrec .eq. precFloat64) then
           call MDS_WRITE_RL_VEC( dUnit, irec, narr, arr, myThid )
          else
           write(msgbuf,'(a)')
     &       ' MDSWRITEVECTOR: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSWRITEVECTOR'
          endif
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEVECTOR: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEVECTOR'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen .AND. (.NOT. globalFile)) then
          close( dUnit )
          fileIsOpen = .FALSE.
        endif
C Create meta-file for each tile file
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(metaFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.meta'
         dimList(1,1) = nPx*nSx*narr
         dimList(2,1) = ((myXGlobalLo-1)/sNx + (bi-1))*narr + 1
         dimList(3,1) = ((myXGlobalLo-1)/sNx +   bi  )*narr
         dimList(1,2) = nPy*nSy
         dimList(2,2) = (myYGlobalLo-1)/sNy + bj
         dimList(3,2) = (myYGlobalLo-1)/sNy + bj
         dimList(1,3) = 1
         dimList(2,3) = 1
         dimList(3,3) = 1
         ndims=1
         call MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, mythid )
        endif
C End of bi,bj loops
ce       enddo
ce      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
        close( dUnit )
        fileIsOpen = .FALSE.
      endif

C Create meta-file for global file
      if (globalFile) then
       write(metaFName(1:80),'(2a)') fName(1:IL),'.meta'
       dimList(1,1) = nPx*nSx*narr
       dimList(2,1) = 1
       dimList(3,1) = nPx*nSx*narr
       dimList(1,2) = nPy*nSy
       dimList(2,2) = 1
       dimList(3,2) = nPy*nSy
       dimList(1,3) = 1
       dimList(2,3) = 1
       dimList(3,3) = 1
       ndims=1
       call MDSWRITEMETA( metaFName, dataFName,
     &   filePrec, ndims, dimList, irecord, myIter, mythid )
      endif

      _END_MASTER( myThid )
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_WRITE_RS_VEC( dUnit, irec, narr, arr, mythid )
C IN:
C   dunit	integer - 'Opened' I/O channel
C   irec	integer - record number to write
C   narr	integer - dimension off array "arr"
C   arr		_RS     - model tiled vector
C   mythid      integer - thread id
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer dUnit
      integer irec
      integer narr
      integer mythid
      _RS arr(narr)
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_WRITE_RS_VEC: irec = ',irec,narr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_WRITE_RL_VEC( dUnit, irec, narr, arr, mythid )
C IN:
C   dunit	integer - 'Opened' I/O channel
C   irec	integer - record number to write
C   narr	integer - dimension off array "arr"
C   arr		_RL     - model tiled vector
C   mythid      integer - thread id
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer dUnit
      integer irec
      integer narr
      integer mythid
      _RL arr(narr)
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_WRITE_RL_VEC: irec = ',irec,narr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_READ_RS_VEC( dUnit, irec, narr, arr, mythid )
C IN:
C   dunit	integer - 'Opened' I/O channel
C   irec	integer - record number to write
C   narr	integer - dimension off array "arr"
C   mythid      integer - thread id
C OUT:
C   arr		_RS     - model tiled vector
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer dUnit
      integer irec
      integer narr
      _RS arr(narr)
      integer mythid
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_READ_RS_VEC: irec = ',irec,narr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      read(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_READ_RL_VEC( dUnit, irec, narr, arr, mythid )
C IN:
C   dunit	integer - 'Opened' I/O channel
C   irec	integer - record number to write
C   narr	integer - dimension off array "arr"
C   mythid      integer - thread id
C OUT:
C   arr		_RL     - model tiled vector
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      implicit none

#include "EEPARAMS.h"

C Arguments
      integer dUnit
      integer irec
      integer narr
      _RL arr(narr)
      integer mythid
C Local
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------
      write(msgbuf,'(a,i9.8,2x,i9.8)')
     &  ' MDS_READ_RL_VEC: irec = ',irec,narr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      read(dUnit,rec=irec) arr
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

#ifdef _BYTESWAPIO
C=======================================================================
      subroutine MDS_BYTESWAPR4( n, arr )
C IN:
C   n		integer - Number of 4-byte words in arr
C IN/OUT:
C   arr		real*4  - Array declared as real*4(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      implicit none
C Arguments
      integer n
      character*(*) arr
C Local
      integer i
      character*(1) cc
C     ------------------------------------------------------------------
      do i=1,4*n,4
       cc=arr(i:i)
       arr(i:i)=arr(i+3:i+3)
       arr(i+3:i+3)=cc
       cc=arr(i+1:i+1)
       arr(i+1:i+1)=arr(i+2:i+2)
       arr(i+2:i+2)=cc
      enddo
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_BYTESWAPR8( n, arr )
C IN:
C   n		integer - Number of 8-byte words in arr
C IN/OUT:
C   arr		real*8  - Array declared as real*4(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      implicit none
C Arguments
      integer n
      character*(*) arr
C Local
      integer i
      character*(1) cc
C     ------------------------------------------------------------------
      do i=1,8*n,8
       cc=arr(i:i)
       arr(i:i)=arr(i+7:i+7)
       arr(i+7:i+7)=cc
       cc=arr(i+1:i+1)
       arr(i+1:i+1)=arr(i+6:i+6)
       arr(i+6:i+6)=cc
       cc=arr(i+2:i+2)
       arr(i+2:i+2)=arr(i+5:i+5)
       arr(i+5:i+5)=cc
       cc=arr(i+3:i+3)
       arr(i+3:i+3)=arr(i+4:i+4)
       arr(i+4:i+4)=cc
      enddo
C     ------------------------------------------------------------------
      return
      end
C=======================================================================
#endif

