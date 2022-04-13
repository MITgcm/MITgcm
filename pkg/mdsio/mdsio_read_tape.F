#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_READ_TAPE
C !INTERFACE:
      SUBROUTINE MDS_READ_TAPE(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nSize,
     O   fldR8, fldR4,
     I   singleCpuIO,
     I   iRec,
     I   myThid )

C !DESCRIPTION:
C MDS_READ_TAPE: load an array (treated as vector) for a tape-file
C  (renamed from MDSREADVECTOR with 2 explicit output array typest)
C
C Arguments:
C fName      string  :: base name for file to read
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: which array (fldR8/R4) to read, either "R8" or "R4"
C nSize      integer :: number of elements of input array "fldR8/R4" to read
C fldR8      ( R8 )  :: array to read if arrType="R8", fldR8(nSize)
C fldR4      ( R4 )  :: array to read if arrType="R4", fldR4(nSize)
C singleCpuIO ( L )  :: only proc 0 do IO and send data to other procs
C iRec       integer :: record number to read
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
      CHARACTER*(2) arrType
      INTEGER nSize
      _R8     fldR8(*)
      _R4     fldR4(*)
      LOGICAL singleCpuIO
      INTEGER iRec
      INTEGER myThid

#ifdef ALLOW_AUTODIFF

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName, pfName
      INTEGER iG, jG, jRec, dUnit, IL, pIL
      LOGICAL exst
      LOGICAL globalFile, fileIsOpen
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
CEOP

      vec_size = nSize*nPx*nPy

C--   Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C-    Record number must be >= 1
      IF ( iRec.LT.1 ) THEN
       WRITE(msgBuf,'(A,I10)')
     &   ' MDS_READ_TAPE: argument iRec =',iRec
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDS_READ_TAPE: invalid value for iRec'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDS_READ_TAPE'
      ENDIF

C-    Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C-    Assign special directory
      IF ( mdsioLocalDir .NE. ' ' ) THEN
       WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
      ELSE
       pfName = fName
      ENDIF
      pIL = ILNBLNK( pfName )

C-    Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( singleCpuIO ) THEN

        IF ( myProcId .EQ. 0 ) THEN
C--   Master thread of process 0, only, opens a global file

C-    Check first for global file with with MDS name (ie. fName.data)
         WRITE(dataFName,'(2A)') fName(1:IL),'.data'
         INQUIRE( file=dataFName, exist=exst )
         IF (exst) globalFile = .TRUE.

C-    If global file is visible to process 0, then open it here.
         IF ( globalFile ) THEN
          IF ( debugLevel .GE. debLevB ) THEN
           WRITE(msgBuf,'(A,A)')
     &     ' MDS_READ_TAPE: opening global file: ',dataFName(1:IL+5)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          length_of_rec = MDS_RECLEN( filePrec, vec_size, myThid )
          OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &          access='direct', recl=length_of_rec )
         ELSE
C     Otherwise stop program.
          WRITE(msgBuf,'(2A)')
     &      ' MDS_READ_TAPE: filename: ',dataFName(1:IL)
C-jmc: why double print (stdout + stderr) ?
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDS_READ_TAPE: File does not exist'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_TAPE'
         ENDIF

C-    Read into global buffer:
         IF ( filePrec.EQ.precFloat32 ) THEN
           READ(dUnit,rec=iRec) gl_buffer_r4
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( vec_size, gl_buffer_r4 )
#endif
         ELSEIF ( filePrec.EQ.precFloat64 ) THEN
           READ(dUnit,rec=iRec) gl_buffer_r8
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( vec_size, gl_buffer_r8 )
#endif
         ENDIF

C-    Close data-file
         CLOSE( dUnit )

C--   end if myProcId=0
        ENDIF

        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL SCATTER_VEC_R4( gl_buffer_r4, local_r4, nSize, myThid )
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          CALL SCATTER_VEC_R8( gl_buffer_r8, local_r8, nSize, myThid )
        ELSE
          WRITE(msgBuf,'(A)')
     &            ' MDS_READ_TAPE: illegal value for filePrec'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_TAPE'
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     if ( singleCpuIO ), else
      ELSEIF ( .NOT. singleCpuIO ) THEN

C-    Check first for global file with with MDS name (ie. fName.data)
        WRITE(dataFName,'(2A)') fName(1:IL),'.data'
        INQUIRE( file=dataFName, exist=exst )
        IF (exst) THEN
          IF ( debugLevel .GE. debLevB ) THEN
           WRITE(msgBuf,'(A,A)')
     &     ' MDS_READ_TAPE: opening global file: ',dataFName(1:IL+5)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          globalFile = .TRUE.
C-    And open it here
          length_of_rec = MDS_RECLEN( filePrec, nSize, myThid )
          OPEN( dUnit, file=dataFName, status='old',_READONLY_ACTION 
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
        ENDIF

C-    If we are reading from a tiled MDS file then we open each one here
        IF ( .NOT.globalFile ) THEN
         iG = 1 + (myXGlobalLo-1)/sNx
         jG = 1 + (myYGlobalLo-1)/sNy
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         INQUIRE( file=dataFName, exist=exst )
         IF (exst) THEN
          IF ( debugLevel .GE. debLevB ) THEN
           WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_TAPE: opening file: ',dataFName(1:pIL+13)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          length_of_rec = MDS_RECLEN( filePrec, nSize, myThid )
          OPEN( dUnit, file=dataFName, status='old',_READONLY_ACTION 
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          fileIsOpen=.FALSE.
          WRITE(msgBuf,'(4A)')
     &      ' MDS_READ_TAPE: missing file: ',fName(1:IL),
     &                                 ' , ',dataFName(1:pIL+13)
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_TAPE'
         ENDIF
        ENDIF

        IF ( fileIsOpen ) THEN
          IF ( globalFile ) THEN
C-    read the same way it was written:
            jRec = 1 + myProcId + (iRec-1)*nPx*nPy
          ELSE
            jRec = iRec
          ENDIF
          IF ( filePrec.EQ.precFloat32 ) THEN
           READ(dUnit,rec=jRec) local_r4
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( nSize, local_r4 )
#endif
          ELSEIF ( filePrec.EQ.precFloat64 ) THEN
           READ(dUnit,rec=jRec) local_r8
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( nSize, local_r8 )
#endif
          ELSE
           WRITE(msgBuf,'(A)')
     &            ' MDS_READ_TAPE: illegal value for filePrec'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDS_READ_TAPE'
          ENDIF
C--   If file was opened then close it
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF

C     end-if ( .not. singleCpuIO )
      ENDIF

      _END_MASTER( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Copy local buffer into output array
        IF ( arrType.EQ.'R4' ) THEN
          IF ( filePrec.EQ.precFloat32 ) THEN
            DO j=1,nSize
              fldR4(j) = local_r4(j)
            ENDDO
          ELSE
            DO j=1,nSize
              fldR4(j) = local_r8(j)
            ENDDO
          ENDIF
        ELSEIF ( arrType.EQ.'R8' ) THEN
          IF ( filePrec.EQ.precFloat32 ) THEN
            DO j=1,nSize
              fldR8(j) = local_r4(j)
            ENDDO
          ELSE
            DO j=1,nSize
              fldR8(j) = local_r8(j)
            ENDDO
          ENDIF
        ELSE
          WRITE(msgBuf,'(A)')
     &          ' MDS_READ_TAPE: illegal value for arrType'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_TAPE'
        ENDIF

#else /* ALLOW_AUTODIFF */
      STOP 'ABNORMAL END: S/R MDS_READ_TAPE is empty'
#endif /* ALLOW_AUTODIFF */

      RETURN
      END
