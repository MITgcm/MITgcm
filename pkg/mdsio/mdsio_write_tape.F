C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_write_tape.F,v 1.1 2013/10/17 00:30:46 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_WRITE_TAPE
C !INTERFACE:
      SUBROUTINE MDS_WRITE_TAPE(
     I   fName,
     I   filePrec,
     I   globalfile,
     I   arrType,
     I   nSize,
     I   fldR8, fldR4,
     I   singleCpuIO,
     I   iRec,
     I   myIter,
     I   myThid )

C !DESCRIPTION:
C MDS_WRITE_TAPE: write an array (treated as vector) to a tape-file
C  (renamed from MDSWRITEVECTOR with 2 explicit input array types)
C
C Arguments:
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: which array (fldR8/R4) to write, either "R8" or "R4"
C nSize      integer :: number of elements of input array "fldR8/R4" to write
C fldR8      ( R8 )  :: array to write if arrType="R8", fldR8(nSize)
C fldR4      ( R4 )  :: array to write if arrType="R4", fldR4(nSize)
C bi,bj      integer :: tile indices (if tiled array)
C singleCpuIO ( L )  :: only proc 0 do IO and collect data from other procs
C iRec       integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: my Thread Id number

C !USES:
      IMPLICIT NONE

C-- Global variables --
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalfile
      CHARACTER*(2) arrType
      INTEGER nSize
      _R8     fldR8(*)
      _R4     fldR4(*)
      LOGICAL singleCpuIO
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_AUTODIFF

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName, metaFName, pfName
      INTEGER iG, jG, jRec, dUnit, IL, pIL
      INTEGER dimList(3,1), nDims, map2gl(2)
      INTEGER length_of_rec
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C simple implementation of singleCpuIO without any specific EXCH2
C feature (should work as long as reading and writing match)
      INTEGER j
      INTEGER vec_size
C Note: would be better to use explicit (allocate/delocate) dynamical
C       allocation instead of this implicit form:
      _R8    gl_buffer_r8(nSize*nPx*nPy)
      _R4    gl_buffer_r4(nSize*nPx*nPy)
      _R8    local_r8    (nSize)
      _R4    local_r4    (nSize)
      _RL dummyRL(1)
      CHARACTER*8 blank8c
CEOP

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /
      DATA map2gl  / 0, 1 /

      vec_size = nSize*nPx*nPy

C--   Copy input array to local buffer
        IF ( arrType.EQ.'R4' ) THEN
          IF ( filePrec.EQ.precFloat32 ) THEN
            DO j=1,nSize
              local_r4(j) = fldR4(j)
            ENDDO
          ELSE
            DO j=1,nSize
              local_r8(j) = fldR4(j)
            ENDDO
          ENDIF
        ELSEIF ( arrType.EQ.'R8' ) THEN
          IF ( filePrec.EQ.precFloat32 ) THEN
            DO j=1,nSize
              local_r4(j) = fldR8(j)
            ENDDO
          ELSE
            DO j=1,nSize
              local_r8(j) = fldR8(j)
            ENDDO
          ENDIF
        ELSE
          WRITE(msgBuf,'(A)')
     &         ' MDS_WRITE_TAPE: illegal value for arrType'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_TAPE'
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C-    Record number must be >= 1
      IF ( iRec.LT.1 ) THEN
       WRITE(msgBuf,'(A,I10)')
     &   ' MDS_WRITE_TAPE: argument iRec =',iRec
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDS_WRITE_TAPE: invalid value for iRec'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDS_WRITE_TAPE'
      ENDIF

C-    Assume nothing
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C-    Assign special directory
      IF ( pIL.EQ.0 ) THEN
        pfName = fName
      ELSE
        WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
      ENDIF
      pIL = ILNBLNK( pfName )
      IF ( debugLevel.GE.debLevC .AND.
     &     ( .NOT.singleCpuIO .OR. myProcId.EQ.0 ) ) THEN
        WRITE(msgBuf,'(A,I8,2A)')
     &      ' MDS_WRITE_TAPE: iRec=', iRec, ', file=', pfName(1:pIL)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C-    Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

C     If option globalFile is desired but does not work or if
C     globalFile is too slow, then try using single-CPU I/O.
      IF ( singleCpuIO ) THEN

C-    Gather array from all procs
        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL GATHER_VEC_R4( gl_buffer_r4, local_r4, nSize, myThid )
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          CALL GATHER_VEC_R8( gl_buffer_r8, local_r8, nSize, myThid )
        ELSE
          WRITE(msgBuf,'(A)')
     &            ' MDS_WRITE_TAPE: illegal value for filePrec'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_TAPE'
        ENDIF

        IF ( myProcId .EQ. 0 ) THEN
C--   Master thread of process 0, only, opens a global file

         WRITE(dataFName,'(2a)') fName(1:IL),'.data'
         length_of_rec = MDS_RECLEN( filePrec, vec_size, myThid )
         IF (iRec .EQ. 1) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &          access='direct', recl=length_of_rec )
         ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &          access='direct', recl=length_of_rec )
         ENDIF

C-    Write global buffer to file:
         IF ( filePrec.EQ.precFloat32 ) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( vec_size, gl_buffer_r4 )
#endif
           WRITE(dUnit,rec=iRec) gl_buffer_r4
         ELSEIF ( filePrec.EQ.precFloat64 ) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( vec_size, gl_buffer_r8 )
#endif
           WRITE(dUnit,rec=iRec) gl_buffer_r8
         ENDIF

C-    Close data-file and create meta-file
         CLOSE( dUnit )
         WRITE(metaFName,'(2a)') fName(1:IL),'.meta'
         dimList(1,1) = vec_size
         dimList(2,1) = 1
         dimList(3,1) = vec_size
         nDims = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, iRec, myIter, myThid )

C-    end if myProcId=0
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     if ( singleCpuIO ), else
      ELSEIF ( .NOT. singleCpuIO ) THEN

        IF ( globalFile ) THEN
C-    If we are writing to a global file then we open it here
         WRITE(dataFName,'(2A)') fName(1:IL),'.data'
         length_of_rec = MDS_RECLEN( filePrec, nSize, myThid )
         IF ( iRec.EQ.1 ) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &          access='direct', recl=length_of_rec )
         ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &          access='direct', recl=length_of_rec )
         ENDIF

        ELSE
C-    If we are writing to a tiled MDS file then we open each one here
         iG = 1 + (myXGlobalLo-1)/sNx
         jG = 1 + (myYGlobalLo-1)/sNy
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         length_of_rec = MDS_RECLEN( filePrec, nSize, myThid )
         IF (iRec .EQ. 1) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &          access='direct', recl=length_of_rec )
         ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &          access='direct', recl=length_of_rec )
         ENDIF
        ENDIF

C-    Write local buffer to file:
        IF (globalFile) THEN
C-- Original: nPy=2, nSx=2 -> produces too large file (1.5 x normal size)
c          iG   = myXGlobalLo-1+(bi-1)*sNx
c          jG   = myYGlobalLo-1+(bj-1)*sNy
c          jRec = 1 + int(iG/sNx) + (jG/sNy)*nSx*nPx +
c    &            (iRec-1)*nSx*nPx*nSy*nPy
C-- Alternative: same layout as in scatter/gather_vector (for singleCpuIO)
C   problem: nPx=2, nSx=2, writing a global (i.e., with bi,bj dim);
C-         2nd proc get iG=3 -> badly placed data over nPx*nPy*nSize range
C                               that will be overwritten by next record
c          iG   = 1 + (myXGlobalLo-1)/sNx
c          jG   = 1 + (myYGlobalLo-1)/sNy
c          jRec = iG + (jG-1)*nPx + (iRec-1)*nPx*nPy
C-- Simpler: should work (but hard to interpret the sequence of data in file)
           jRec = 1 + myProcId + (iRec-1)*nPx*nPy
        ELSE
           jRec = iRec
        ENDIF
        IF ( filePrec.EQ.precFloat32 ) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( nSize, local_r4 )
#endif
           WRITE(dUnit,rec=jRec) local_r4
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( nSize, local_r8 )
#endif
           WRITE(dUnit,rec=jRec) local_r8
        ELSE
           WRITE(msgBuf,'(A)')
     &           ' MDS_WRITE_TAPE: illegal value for filePrec'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDS_WRITE_TAPE'
        ENDIF

C-    Close data-file and create meta-file
        CLOSE( dUnit )
        IF ( globalFile ) THEN
C     meta-file for global file
          WRITE(metaFName,'(2A)') fName(1:IL),'.meta'
          dimList(1,1) = vec_size
          dimList(2,1) = 1
          dimList(3,1) = vec_size
          nDims = 1
        ELSE
C     meta-file for tiled file
          iG = 1 + (myXGlobalLo-1)/sNx
          jG = 1 + (myYGlobalLo-1)/sNy
          WRITE(metaFName,'(2A,I3.3,A,I3.3,A)')
     &             pfName(1:pIL),'.',iG,'.',jG,'.meta'
          dimList(1,1) = nPx*nPy*nSize
          dimList(2,1) = 1 + myProcId*nSize
          dimList(3,1) = (1+myProcId)*nSize
          nDims = 1
        ENDIF
C-    write meta-file
        CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, iRec, myIter, myThid )
c    I              metaFName, dataFName, the_run_name, titleLine,
c    I              filePrec, nDims, dimList, map2gl, nFlds, fldList,
c    I              nTimRec, timList, misVal, iRec, myIter, myThid )

C     end-if ( .not. singleCpuIO )
      ENDIF

      _END_MASTER( myThid )

#else /* ALLOW_AUTODIFF */
      STOP 'ABNORMAL END: S/R MDS_WRITE_TAPE is empty'
#endif /* ALLOW_AUTODIFF */

      RETURN
      END
