C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_writevector.F,v 1.11 2008/12/30 02:09:00 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

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
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C narr       integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(narr)
C bi         integer :: x tile index
C bj         integer :: y tile index
C irecord    integer :: record number to read
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)
C Changed: 05/31/00 heimbach@mit.edu
C          open(dUnit, ..., status='old', ... -> status='unknown'

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalfile
      CHARACTER*(2) arrType
      INTEGER narr
      _RL arr(narr)
      INTEGER bi,bj
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#if defined(ALLOW_AUTODIFF) || defined(ALLOW_FLT)

C Functions
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
C Local variables
      CHARACTER*(MAX_LEN_FNAM) dataFName,metaFName,pfName
      INTEGER iG,jG,irec,dUnit,IL,pIL
      LOGICAL fileIsOpen
      INTEGER dimList(3,3), nDims, map2gl(2)
      INTEGER length_of_rec
      CHARACTER*(MAX_LEN_MBUF) msgBuf

cph(
cph Deal with useSingleCpuIO
cph Not implemented here for EXCH2
      INTEGER vec_size
#ifdef ALLOW_USE_MPI
      LOGICAL lprint
      INTEGER K,L
c     INTEGER iG_IO,jG_IO,npe
      Real*8 global(narr*nPx*nPy)
      _RL    local(narr)
#endif
cph)
      Real*4 xy_buffer_r4(narr*nPx*nPy)
      Real*8 xy_buffer_r8(narr*nPx*nPy)

      DATA map2gl / 0, 1 /

C     ------------------------------------------------------------------

      vec_size = narr*nPx*nPy

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
       WRITE(msgBuf,'(A,I9.8)')
     &   ' MDSWRITEVECTOR: argument irecord = ',irecord
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDSWRITEVECTOR: invalid value for irecord'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDSWRITEVECTOR'
      ENDIF

C Assume nothing
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      IF ( mdsioLocalDir .NE. ' ' ) THEN
       WRITE(pfName,'(2A)')
     &  mdsioLocalDir(1:pIL), fName(1:IL)
      ELSE
       pfName= fName
      ENDIF
      pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

#ifdef ALLOW_USE_MPI
      _END_MASTER( myThid )
C If option globalFile is desired but does not work or if
C globalFile is too slow, then try using single-CPU I/O.
      IF (useSingleCpuIO) THEN

C Master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         WRITE(dataFName,'(2a)') fName(1:IL),'.data'
         length_of_rec=MDS_RECLEN(filePrec,vec_size,myThid)
         IF (irecord .EQ. 1) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &          access='direct', recl=length_of_rec )
         ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &          access='direct', recl=length_of_rec )
         ENDIF
        ENDIF
       _END_MASTER( myThid )

C Gather array and write it to file, one vertical level at a time
       DO k=1,1
        IF ( arrType.EQ.'RS' ) THEN
           CALL MDS_BUFFERtoRS( local, arr, narr, .FALSE., myThid )
        ELSEIF ( arrType.EQ.'RL' ) THEN
           CALL MDS_BUFFERtoRL( local, arr, narr, .FALSE., myThid )
        ELSE
           WRITE(msgBuf,'(A)')
     &          ' MDSWRITEVECTOR: illegal value for arrType'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDSWRITEVECTOR'
        ENDIF
cph(
cph        IF ( irecord .EQ. 1 .AND. fName(1:IL) .EQ.
cph     &       'tapelev2_7_the_main_loop_theta.it0000' ) THEN
cph           lprint = .TRUE.
cph        ELSE
           lprint = .FALSE.
cph        ENDIF
cph)
        CALL GATHER_VECTOR( lprint, narr, global, local, myThid )
        _BEGIN_MASTER( myThid )
         IF( mpiMyId .EQ. 0 ) THEN
          irec=irecord
          IF (filePrec .EQ. precFloat32) THEN
cph#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
cph#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            xy_buffer_r4(L) = global(L)
           ENDDO
cph#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( vec_size, xy_buffer_r4 )
#endif
           WRITE(dUnit,rec=irec) xy_buffer_r4
          ELSEIF (filePrec .EQ. precFloat64) THEN
cph#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
cph#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            xy_buffer_r8(L) = global(L)
           ENDDO
cph#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( vec_size, xy_buffer_r8 )
#endif
           WRITE(dUnit,rec=irec) xy_buffer_r8
          ELSE
           WRITE(msgBuf,'(A)')
     &       ' MDSWRITEVECTOR: illegal value for filePrec'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDSWRITEVECTOR'
          ENDIF
         ENDIF
        _END_MASTER( myThid )
C End k loop
       ENDDO

C Close data-file and create meta-file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         CLOSE( dUnit )
         WRITE(metaFName,'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=vec_size
         dimList(2,1)=1
         dimList(3,1)=vec_size
         dimList(1,2)=vec_size
         dimList(2,2)=1
         dimList(3,2)=vec_size
         dimList(1,3)=1
         dimList(2,3)=1
         dimList(3,3)=1
         nDims = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, ' ',
     I              0, UNSET_RL, irecord, myIter, myThid )
        ENDIF
       _END_MASTER( myThid )
C To be safe, make other processes wait for I/O completion
       _BARRIER

      ELSEIF ( .NOT. useSingleCpuIO ) THEN
      _BEGIN_MASTER( myThid )
#endif /* ALLOW_USE_MPI */

C If we are writing to a global file then we open it here
      IF (globalFile) THEN
       WRITE(dataFName,'(2A)') fName(1:IL),'.data'
       IF (irecord .EQ. 1) THEN
        length_of_rec = MDS_RECLEN( filePrec, narr, myThid )
        OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &        access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       ELSE
        length_of_rec = MDS_RECLEN( filePrec, narr, myThid )
        OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &        access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       ENDIF
      ENDIF

C Loop over all tiles
c     DO bj=1,nSy
c      DO bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         IF (irecord .EQ. 1) THEN
          length_of_rec = MDS_RECLEN( filePrec, narr, myThid )
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          length_of_rec = MDS_RECLEN( filePrec, narr, myThid )
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ENDIF
        ENDIF
        IF (fileIsOpen) THEN
          IF (globalFile) THEN
            iG   = myXGlobalLo-1+(bi-1)*sNx
            jG   = myYGlobalLo-1+(bj-1)*sNy
            irec = 1 + int(iG/sNx) + (jG/sNy)*nSx*nPx +
     &             (irecord-1)*nSx*nPx*nSy*nPy
          ELSE
            iG   = 0
            jG   = 0
            irec = irecord
          ENDIF
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_WR_REC_RS( arr, xy_buffer_r4, xy_buffer_r8,
     I                          filePrec, dUnit, irec, narr, myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_WR_REC_RL( arr, xy_buffer_r4, xy_buffer_r8,
     I                          filePrec, dUnit, irec, narr, myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &          ' MDSWRITEVECTOR: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDSWRITEVECTOR'
          ENDIF
        ELSE
         WRITE(msgBuf,'(A)')
     &     ' MDSWRITEVECTOR: I should never get to this point'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDSWRITEVECTOR'
        ENDIF
C If we were writing to a tiled MDS file then we close it here
        IF (fileIsOpen .AND. (.NOT. globalFile)) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF
C Create meta-file for each tile file
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         WRITE(metaFName,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.meta'
         dimList(1,1) = nPx*nSx*narr
         dimList(2,1) = ((myXGlobalLo-1)/sNx + (bi-1))*narr + 1
         dimList(3,1) = ((myXGlobalLo-1)/sNx +   bi  )*narr
         dimList(1,2) = nPy*nSy
         dimList(2,2) = (myYGlobalLo-1)/sNy + bj
         dimList(3,2) = (myYGlobalLo-1)/sNy + bj
         dimList(1,3) = 1
         dimList(2,3) = 1
         dimList(3,3) = 1
         nDims = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, ' ',
     I              0, UNSET_RL, irecord, myIter, myThid )
        ENDIF
C End of bi,bj loops
c      ENDDO
c     ENDDO

C If global file was opened then close it
      IF (fileIsOpen .AND. globalFile) THEN
        CLOSE( dUnit )
        fileIsOpen = .FALSE.
      ENDIF

C Create meta-file for global file
      IF (globalFile) THEN
       WRITE(metaFName,'(2A)') fName(1:IL),'.meta'
       dimList(1,1) = nPx*nSx*narr
       dimList(2,1) = 1
       dimList(3,1) = nPx*nSx*narr
       dimList(1,2) = nPy*nSy
       dimList(2,2) = 1
       dimList(3,2) = nPy*nSy
       dimList(1,3) = 1
       dimList(2,3) = 1
       dimList(3,3) = 1
       nDims = 1
       CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, ' ',
     I              0, UNSET_RL, irecord, myIter, myThid )
c    I              metaFName, dataFName, the_run_name, titleLine,
c    I              filePrec, nDims, dimList, map2gl, nFlds, fldList,
c    I              nTimRec, timList, irecord, myIter, myThid )
      ENDIF

      _END_MASTER( myThid )

#ifdef ALLOW_USE_MPI
C End-if useSingleCpuIO
      ENDIF
#endif /* ALLOW_USE_MPI */

#endif /* defined(ALLOW_AUTODIFF) || defined(ALLOW_FLT) */

C     ------------------------------------------------------------------
      RETURN
      END
