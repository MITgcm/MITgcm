C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/mdsio_slice.F,v 1.1 2001/03/25 22:31:53 heimbach Exp $

#include "CPP_OPTIONS.h"

#undef  SAFE_IO

#ifdef SAFE_IO
#define _NEW_STATUS 'new'
#else
#define _NEW_STATUS 'unknown'
#endif

C=======================================================================
      SUBROUTINE MDSREADFIELDXZ(
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
      Real*4 r4seg(sNx+2*oLx)
      Real*8 r8seg(sNx+2*oLx)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
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

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       write(msgbuf,'(a,a)')
     &   ' MDSREADFIELDXZ: opening global file: ',dataFName
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
     &    ' MDSREADFIELDXZ: opening global file: ',dataFName
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        globalFile = .TRUE.
       endif
      endif

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNx+2*oLx, mythid )
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
     &      ' MDSREADFIELDXZ: opening file: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          length_of_rec=MDS_RECLEN( filePrec, sNx+2*oLx, mythid )
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
            irec=1 + INT(iG/sNx) + nSx*nPx*jG + nSx*nPx*nSy*nPy*(k-1)
     &           + nSx*nPx*nSy*nPy*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=k + nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4(sNx+2*oLx,r4seg)
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
            call MDS_BYTESWAPR8( sNx+2*oLx, r8seg )
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

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSREADFIELDYZ(
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
      Real*4 r4seg(sNy+2*oLy)
      Real*8 r8seg(sNy+2*oLy)
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf
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

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       write(msgbuf,'(a,a)')
     &   ' MDSREADFIELDYZ: opening global file: ',dataFName
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
     &    ' MDSREADFIELDYZ: opening global file: ',dataFName
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        globalFile = .TRUE.
       endif
      endif

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, sNy+2*oLy, mythid )
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
     &      ' MDSREADFIELDYZ: opening file: ',dataFName
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          length_of_rec=MDS_RECLEN( filePrec, sNy+2*oLy, mythid )
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
            iG = myXGlobalLo-1 + (bi-1)*sNx
            jG = (myYGlobalLo-1)/sNy + (bj-1)
            irec=1 + INT(iG/sNx) + nSx*nPx*jG + nSx*nPx*nSy*nPy*(k-1)
     &           + nSx*nPx*nSy*nPy*nNz*(irecord-1)
           else
            iG = 0
            jG = 0
            irec=k + nNz*(irecord-1)
           endif
           if (filePrec .eq. precFloat32) then
            read(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR4(sNy+2*oLy,r4seg)
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
            call MDS_BYTESWAPR8( sNy+2*oLy, r8seg )
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

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSWRITEFIELDXZ(
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

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      character*(2) arrType
      integer nNz
      Real arr(*)
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName
      integer iG,jG,irec,bi,bj,k,dUnit,IL
      Real*4 r4seg(sNx+2*oLx)
      Real*8 r8seg(sNx+2*oLx)
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
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

C If we are writing to a global file then we open it here
      if (globalFile) then
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec=MDS_RECLEN( filePrec, sNx+2*oLx, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec=MDS_RECLEN( filePrec, sNx+2*oLx, mythid )
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
          length_of_rec=MDS_RECLEN( filePrec, sNx+2*oLx, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec=MDS_RECLEN( filePrec, sNx+2*oLx, mythid )
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
            irec=1 + INT(iG/sNx) + nSx*nPx*jG + nSx*nPx*nSy*nPy*(k-1)
     &           + nSx*nPx*nSy*nPy*nNz*(irecord-1)
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
            call MDS_BYTESWAPR4(sNx+2*oLx,r4seg)
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
            call MDS_BYTESWAPR8( sNx+2*oLx, r8seg )
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

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      SUBROUTINE MDSWRITEFIELDYZ(
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

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      character*(2) arrType
      integer nNz
      Real arr(*)
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(80) dataFName
      integer iG,jG,irec,bi,bj,k,dUnit,IL
      Real*4 r4seg(sNy+2*oLy)
      Real*8 r8seg(sNy+2*oLy)
      integer length_of_rec
      logical fileIsOpen
      character*(max_len_mbuf) msgbuf
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

C If we are writing to a global file then we open it here
      if (globalFile) then
       write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec=MDS_RECLEN( filePrec, sNy+2*oLy, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec=MDS_RECLEN( filePrec, sNy+2*oLy, mythid )
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
          length_of_rec=MDS_RECLEN( filePrec, sNy+2*oLy, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec=MDS_RECLEN( filePrec, sNy+2*oLy, mythid )
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
            irec=1 + INT(iG/sNx) + nSx*nPx*jG + nSx*nPx*nSy*nPy*(k-1)
     &           + nSx*nPx*nSy*nPy*nNz*(irecord-1)
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
            call MDS_BYTESWAPR4(sNy+2*oLy,r4seg)
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
            call MDS_BYTESWAPR8( sNy+2*oLy, r8seg )
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

C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG4toRL_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)
C IN:
C     sn,ol,nNz integer - size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,	integer - indices to array "arr"
C     copyTo    logical - flag to indicate tranfer direction.
C                         .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg	Real*4  - 1-D vector of length sn
C OUT:
C     arr	_RL     - model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer sn,ol,nNz,bi,bj,k
      logical copyTo
      Real*4 seg(sn+2*ol)
      _RL arr(1-ol:sn+ol,nNz,nSx,nSy)
     
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1-ol,sn+ol
        arr(ii,k,bi,bj)=seg(ii+ol)
       enddo
      else
       do ii=1-ol,sn+ol
        seg(ii+ol)=arr(ii,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG4toRS_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)
C IN:
C     sn,ol,nNz integer - size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,	integer - indices to array "arr"
C     copyTo    logical - flag to indicate tranfer direction.
C                         .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg	Real*4  - 1-D vector of length sn
C OUT:
C     arr	_RS     - model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer sn,ol,nNz,bi,bj,k
      logical copyTo
      Real*4 seg(sn+2*ol)
      _RS arr(1-ol:sn+ol,nNz,nSx,nSy)
     
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1-ol,sn+ol
        arr(ii,k,bi,bj)=seg(ii+ol)
       enddo
      else
       do ii=1-ol,sn+ol
        seg(ii+ol)=arr(ii,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG8toRL_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)
C IN:
C     sn,ol,nNz integer - size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,	integer - indices to array "arr"
C     copyTo    logical - flag to indicate tranfer direction.
C                         .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg	Real*8  - 1-D vector of length sn
C OUT:
C     arr	_RL     - model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer sn,ol,nNz,bi,bj,k
      logical copyTo
      Real*8 seg(sn+2*ol)
      _RL arr(1-ol:sn+ol,nNz,nSx,nSy)
     
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1-ol,sn+ol
        arr(ii,k,bi,bj)=seg(ii+ol)
       enddo
      else
       do ii=1-ol,sn+ol
        seg(ii+ol)=arr(ii,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

C=======================================================================
      subroutine MDS_SEG8toRS_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)
C IN:
C     sn,ol,nNz integer - size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,	integer - indices to array "arr"
C     copyTo    logical - flag to indicate tranfer direction.
C                         .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg	Real*8  - 1-D vector of length sn
C OUT:
C     arr	_RS     - model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      implicit none
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      integer sn,ol,nNz,bi,bj,k
      logical copyTo
      Real*8 seg(sn+2*ol)
      _RS arr(1-ol:sn+ol,nNz,nSx,nSy)
     
C Local
      integer ii
C     ------------------------------------------------------------------
      if (copyTo) then
       do ii=1-ol,sn+ol
        arr(ii,k,bi,bj)=seg(ii+ol)
       enddo
      else
       do ii=1-ol,sn+ol
        seg(ii+ol)=arr(ii,k,bi,bj)
       enddo
      endif
C     ------------------------------------------------------------------
      return
      end
C=======================================================================

