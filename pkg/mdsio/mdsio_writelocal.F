C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_writelocal.F,v 1.5 2006/08/04 21:52:25 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDS_WRITELOCAL(
     I   fName,
     I   filePrec,
     I   globFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   bi, bj,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to written
C filePrec	integer	number of bits per word in file (32 or 64)
C globFile	logical	selects between writing a global or tiled file
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNz		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to write, arr(:,:,nNz,:,:)
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C MDS_WRITELOCAL creates either a file of the form "fName.data" and
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
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globFile
      character*(2) arrType
      integer nNz
c     real arr(*)
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz)
      integer bi, bj
      integer irecord
      integer myIter
      integer myThid
C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,metaFName
      integer iG,jG,irec,j,k,dUnit,IL
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      integer dimList(3,3),ndims
      integer length_of_rec
      logical fileIsOpen
      logical globalFile, iAmDoingIO
      character*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_EXCH2
      integer tn
#endif /* ALLOW_EXCH2 */
      integer x_size,y_size
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      PARAMETER ( x_size = exch2_domain_nxt * sNx )
      PARAMETER ( y_size = exch2_domain_nyt * sNy )
#else
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
#endif
C     ------------------------------------------------------------------

      IL = ILNBLNK( fName )
      iAmDoingIO = .TRUE.
      globalFile = globFile

#ifndef LOCBIN_IO_THREAD_SAFE
      IF ( myThid.GT.1 ) THEN
C-    do not assume safe Muti-Threaded Binary IO (even to tiled file)
C      => stop here
        iAmDoingIO = .FALSE.
        WRITE(msgBuf,'(A,I10,A,2I5,2A)') 
     &    'MDS_WRITELOCAL (it=', myIter, ' ; bi,bj=', bi,bj, 
     &    ' ): try to write: ', fName(1:IL)
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'MDS_WRITELOCAL: no Multi-Threaded IO',
     &                  ' with #undef LOCBIN_IO_THREAD_SAFE'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITELOCAL'
      ENDIF
#endif /* ndef LOCBIN_IO_THREAD_SAFE */

      IF ( nThreads.GT.1 .AND. globFile ) THEN
C-    do not assume safe Muti-Threaded Binary IO to a single global file
C      => switch to tiled file
        globalFile = .FALSE.
        IF ( debugLevel.GE.debLevA .AND. IL.GT.0 ) THEN
         WRITE(msgBuf,'(A,I10,A,2I5,A)') 
     &    'MDS_WRITELOCAL (it=', myIter, ' ; bi,bj=', bi,bj, 
     &    ' ): No global-file multi-threaded IO'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(2A)') 
     &    'MDS_WRITELOCAL: => write tiled file: ', fName(1:IL)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        ENDIF
      ENDIF

C Only do I/O if I am the master thread
C jmc: No MASTER thread only within multi-threaded bi,bj loops
C      => will loose output from all non-master-thread tiles
c     _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgBuf,'(a,i9.8)')
     &   ' MDS_WRITELOCAL: argument irecord = ',irecord
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
       write(msgBuf,'(a)')
     &   ' MDS_WRITELOCAL: invalid value for irecord'
       CALL PRINT_ERROR( msgBuf, myThid )
       stop 'ABNORMAL END: S/R MDS_WRITELOCAL'
      endif

      IF ( iAmDoingIO ) THEN 

C Assume nothing
      fileIsOpen=.FALSE.

C Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

C If we are writing to a global file then we open it here
      if (globalFile) then
       write(dataFName,'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
        open( dUnit, file=dataFName, status='unknown',
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
        open( dUnit, file=dataFName, status=_OLD_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       endif
      endif

C Loop over all tiles
c     do bj=1,nSy
c      do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFName,'(2a,i3.3,a,i3.3,a)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         if (irecord .EQ. 1) then
          length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
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
             CALL MDS_SEG4toRS( j,1,1,k,nNz, r4seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             CALL MDS_SEG4toRL( j,1,1,k,nNz, r4seg, .FALSE., arr )
            else
             write(msgBuf,'(a)')
     &         ' MDS_WRITELOCAL: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             stop 'ABNORMAL END: S/R MDS_WRITELOCAL'
            endif
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR4( sNx, r4seg )
#endif
            write(dUnit,rec=irec) r4seg
           elseif (filePrec .eq. precFloat64) then
            if (arrType .eq. 'RS') then
             CALL MDS_SEG8toRS( j,1,1,k,nNz, r8seg, .FALSE., arr )
            elseif (arrType .eq. 'RL') then
             CALL MDS_SEG8toRL( j,1,1,k,nNz, r8seg, .FALSE., arr )
            else
             write(msgBuf,'(a)')
     &         ' MDS_WRITELOCAL: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             stop 'ABNORMAL END: S/R MDS_WRITELOCAL'
            endif
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( sNx, r8seg )
#endif
            write(dUnit,rec=irec) r8seg
           else
            write(msgBuf,'(a)')
     &        ' MDS_WRITELOCAL: illegal value for filePrec'
            CALL PRINT_ERROR( msgBuf, myThid )
            stop 'ABNORMAL END: S/R MDS_WRITELOCAL'
           endif
C End of j loop
          enddo
C End of k loop
         enddo
        else
         write(msgBuf,'(a)')
     &     ' MDS_WRITELOCAL: I should never get to this point'
         CALL PRINT_ERROR( msgBuf, myThid )
         stop 'ABNORMAL END: S/R MDS_WRITELOCAL'
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
     &              fName(1:IL),'.',iG,'.',jG,'.meta'
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
         dimList(1,3)=Nr
         dimList(2,3)=1
         dimList(3,3)=Nr
         ndims=3
         if (nNz .EQ. 1) ndims=2
         CALL MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, myThid )
        endif
C End of bi,bj loops
c      enddo
c     enddo

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
        write(msgBuf,'(a,a)')
     &    ' MDS_WRITELOCAL: A threads version of this routine',
     &    ' does not exist.'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        write(msgBuf,'(a)')
     &    ' MDS_WRITELOCAL: This needs to be fixed...'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        write(msgBuf,'(a,i3.2)')
     &    ' MDS_WRITELOCAL: nThreads = ',nThreads
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        write(msgBuf,'(a)')
     &    ' MDS_WRITELOCAL: Stopping because you are using threads'
        CALL PRINT_ERROR( msgBuf, myThid )
        stop 'ABNORMAL END: S/R MDS_WRITELOCAL'
       endif
C We put a barrier here to ensure that all processes have finished
C writing their data before we update the meta-file
c      _BARRIER
       write(metaFName,'(2a)') fName(1:IL),'.meta'
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
       CALL MDSWRITEMETA( metaFName, dataFName,
     &   filePrec, ndims, dimList, irecord, myIter, myThid )
       fileIsOpen=.TRUE.
      endif

c     _END_MASTER( myThid )
C-    end if iAmDoingIO
      ENDIF

C     ------------------------------------------------------------------
      RETURN
      END
