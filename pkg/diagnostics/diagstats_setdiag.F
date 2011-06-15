C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_setdiag.F,v 1.4 2011/06/15 13:44:42 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_SETDIAG
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_SETDIAG(
     O                      mate,
     U                      ndiagmx,
     I                      mId, listId, ndId, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGSTATS_SETDIAG
C     | o activate statistics diagnostic "ndId":
C     |   set pointer locations for this diagnostic ;
C     |   look for a counter mate and set it
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     mate    :: counter-mate number in available diagnostics list
C     ndiagmx :: current space allocated in storage array
C     mId    :: current field index in list "listId"
C     listId :: current list number that contains field "mId"
C     ndId   :: diagnostic number in available diagnostics list
C     myThid :: Thread number for this instance of the routine.
      INTEGER mate
      INTEGER ndiagmx
      INTEGER mId, listId, ndId
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER stdUnit, errUnit
      INTEGER k, l
      LOGICAL flag

      CHARACTER*10 gcode
      CHARACTER*(MAX_LEN_MBUF) msgBuf


C **********************************************************************
C ****                SET POINTERS FOR DIAGNOSTIC ndId              ****
C **********************************************************************

      gcode   = gdiag(ndId)(1:8)
      stdUnit = standardMessageUnit
      errUnit = errorMessageUnit

C--   Seach for the same diag (with same freq) to see if already set
      flag = .TRUE.
      DO l=1,listId
       IF (flag .AND. diagSt_freq(l) .EQ. diagSt_freq(listId)
     &          .AND. diagSt_phase(l).EQ.diagSt_phase(listId) ) THEN
        DO k=1,MIN(diagSt_nbActv(l),numperlist)
         IF (flag .AND. jSdiag(k,l).GT.0) THEN
          IF (cdiag(ndId).EQ.cdiag(jSdiag(k,l)) ) THEN
C-    diagnostics already set ; use the same slot:
           flag = .FALSE.
           iSdiag(mId,listId) = -ABS(iSdiag(k,l))
           mSdiag(mId,listId) = mSdiag(k,l)
          ENDIF
         ENDIF
        ENDDO
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( flag ) THEN
        IF (ndiagmx+kdiag(ndId).GT.diagSt_size) THEN
         WRITE(msgBuf,'(A,I6,1X,A)')
     &    'SETDIAG: Not enough space for Stats-Diag #',ndId,cdiag(ndId)
         CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
        ELSE
         WRITE(msgBuf,'(A,I3,A,I6,1X,A)') 'SETDIAG: Allocate',
     &    kdiag(ndId), ' Levels for Stats-Diag #', ndId, cdiag(ndId)
         CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
        ENDIF
        iSdiag(mId,listId) = ndiagmx + 1
        ndiagmx = ndiagmx + kdiag(ndId)
      ELSE
        WRITE(msgBuf,'(A,I6,1X,2A)')
     &    '- NOTE - SETDIAG: Stats-Diag #', ndId, cdiag(ndId),
     &    ' has already been set'
        CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
      ENDIF

C---  Check for Counter Diagnostic

      mate = 0
      IF ( gcode(5:5).EQ.'C') THEN
        mate = hdiag(ndId)

C--     Seach for the same diag (with same freq) to see if already set
        flag = .TRUE.
        DO l=1,listId
         IF (flag .AND. diagSt_freq(l) .EQ. diagSt_freq(listId)
     &            .AND. diagSt_phase(l).EQ.diagSt_phase(listId) ) THEN
          DO k=1,MIN(diagSt_nbActv(l),numperlist)
           IF (flag .AND. jSdiag(k,l).GT.0) THEN
            IF (cdiag(mate).EQ.cdiag(jSdiag(k,l)) ) THEN
C-    diagnostics already set ; use the same slot:
             flag = .FALSE.
             mSdiag(mId,listId) = ABS(iSdiag(k,l))
            ENDIF
           ENDIF
          ENDDO
         ENDIF
        ENDDO

        IF ( flag ) THEN
          IF (ndiagmx+kdiag(mate).GT.diagSt_size) THEN
           WRITE(msgBuf,'(A,I6,1X,A)')
     &      'SETDIAG: Not enough space for Counter Diagnostic #',
     &      mate, cdiag(mate)
           CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
          ELSE
           WRITE(msgBuf,'(A,I3,A,I6,1X,A)') 'SETDIAG: Allocate',
     &     kdiag(mate), ' Levels for Count.Diag #', mate, cdiag(mate)
           CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
          ENDIF
          mSdiag(mId,listId) = ndiagmx + 1
          ndiagmx = ndiagmx + kdiag(mate)
        ELSE
          WRITE(msgBuf,'(A,I6,1X,2A)')
     &    '- NOTE - SETDIAG: Counter Diagnostic #', mate, cdiag(mate),
     &    ' has already been set'
          CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
          mate = -mate
        ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
