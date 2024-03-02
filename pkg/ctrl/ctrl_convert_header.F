#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_CONVERT_HEADER
C     !INTERFACE:
      SUBROUTINE CTRL_CONVERT_HEADER(
     I              maxLocal, numVar, errMsgUnit,
     U              varIndex, varRecs,
     U              varNxMax, varNyMax, varNrMax,
     U              varGrid, varType,
     I              myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_CONVERT_HEADER
C     | o Convert header of old ctrl-pack file to current one
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
c#include "SIZE.h"
#include "CTRL_SIZE.h"
c#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid     :: my Thread Id number
      INTEGER maxLocal, numVar, errMsgUnit
      INTEGER varIndex(maxLocal)
      INTEGER varRecs (maxLocal)
      INTEGER varNxMax(maxLocal)
      INTEGER varNyMax(maxLocal)
      INTEGER varNrMax(maxLocal)
      CHARACTER*(1) varGrid(maxLocal)
      CHARACTER*(5) varType(maxLocal)
      INTEGER myThid

#ifdef READ_OLD_CTRL_PACK_FILE
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      INTEGER i, j, k
      INTEGER iarr, jarr
      CHARACTER*(5) allType(5)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      errCount = 0
      allType(1) = 'SecXZ'
      allType(2) = 'SecYZ'
      allType(3) = 'Arr2D'
      allType(4) = 'Arr3D'
      allType(5) = 'Tim2D'

      j = 0
      DO i=1,maxLocal
        iarr = varIndex(i)
        IF ( iarr.NE. -1 ) THEN
          IF ( ( iarr.GE.111 .AND. iarr.LE.114 ).OR.
     &         ( iarr.GE.201 .AND. iarr.LE.200+maxCtrlArr2D ) .OR.
     &         ( iarr.GE.301 .AND. iarr.LE.300+maxCtrlArr3D ) .OR.
     &         ( iarr.GE.401 .AND. iarr.LE.400+maxCtrlTim2D ) ) THEN
            j = j + 1
            k = ( iarr - 1 ) / 100 + 1
            IF ( k.LE.2 ) THEN
              jarr = iarr - 110
              IF ( iarr.LE.112 ) THEN
                k = 1
              ELSE
C             switch 3 <-> 4 ( obE was 114 now #3 ; obW was 113 now #4 )
                jarr = 7 - jarr
              ENDIF
            ELSE
              jarr = MOD( iarr, 100 )
            ENDIF
C- copy i -> j :
            varIndex(j) = jarr
            varRecs(j)  = varRecs(i)
            varNxMax(j) = varNxMax(i)
            varNyMax(j) = varNyMax(i)
            varNrMax(j) = varNrMax(i)
            varGrid(j)  = varGrid(i)
c           varType(j)  = varType(i)
            varType(j)  = allType(k)
          ELSE
            WRITE(msgBuf,'(A,2(A,I4))') '*** ERROR *** ',
     &      'CTRL_CONVERT_HEADER: invalid varIndex(i=', i, ')=', iarr
            WRITE(errMsgUnit,'(A)') msgBuf
c           CALL PRINT_ERROR( msgBuf, myThid )
            errCount = errCount + 1
          ENDIF
        ENDIF
      ENDDO

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(2A,I3,A)') '*** ERROR *** ',
     &      'CTRL_CONVERT_HEADER: detected', errCount,' fatal error(s)'
        WRITE(errMsgUnit,'(A)') msgBuf
c       CALL PRINT_ERROR( msgBuf, myThid )
c       CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R CTRL_CONVERT_HEADER'
      ENDIF
      IF ( j .NE. numVar ) THEN
        WRITE(msgBuf,'(2A,I4)') '*** ERROR *** ',
     &      'CTRL_CONVERT_HEADER: number of ctrl-vars counted =', j
        WRITE(errMsgUnit,'(A)') msgBuf
        WRITE(msgBuf,'(2A,I4)') '*** ERROR *** ',
     &      'CTRL_CONVERT_HEADER: inconsitent with numVar=', numVar
        WRITE(errMsgUnit,'(A)') msgBuf
        STOP 'ABNORMAL END: S/R CTRL_CONVERT_HEADER'
      ENDIF

#endif /* READ_OLD_CTRL_PACK_FILE */
      RETURN
      END
