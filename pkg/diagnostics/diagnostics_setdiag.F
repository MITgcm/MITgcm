C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_setdiag.F,v 1.9 2011/07/01 18:26:54 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGNOSTICS_SETDIAG
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SETDIAG(
     O                      mate,
     U                      ndiagmx,
     I                      mId, listId, ndId, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGNOSTICS_SETDIAG
C     | o activate diagnostic "ndId":
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
C     mId     :: current field index in list "listId"
C     listId  :: current list number that contains field "mId"
C     ndId    :: diagnostic number in available diagnostics list
C     myThid  :: Thread number for this instance of the routine.
      INTEGER mate
      INTEGER ndiagmx
      INTEGER mId, listId, ndId
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER stdUnit, errUnit
      INTEGER nn, k, l
      LOGICAL diagIsPP, flagD, flagP, flagM

      CHARACTER*10 gcode
      CHARACTER*12 tmpMsg
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C **********************************************************************
C ****                SET POINTERS FOR DIAGNOSTIC ndId              ****
C **********************************************************************

      stdUnit = standardMessageUnit
      errUnit = errorMessageUnit

C-    Case of Post-Procesed diag, not filled up but computed from other diags:
      diagIsPP = gdiag(ndId)(5:5).EQ.'P'

C---  Seach for the same diag (with same freq) to see if already set
C     do it recursively on Post-Processed diag dependance (=mate)
C     until we find either one already set or a non Post-Processed diag
      flagD = .TRUE.
      flagP = .TRUE.
      nn = ndId
      DO WHILE ( flagP )
        DO l=1,listId
         IF (flagD .AND. freq(l) .EQ. freq(listId)
     &             .AND. phase(l).EQ.phase(listId)
     &             .AND. averageFreq(l) .EQ.averageFreq(listId)
     &             .AND. averagePhase(l).EQ.averagePhase(listId)
     &             .AND. averageCycle(l).EQ.averageCycle(listId) ) THEN
          DO k=1,MIN(nActive(l),numperList)
           IF (flagD .AND. jdiag(k,l).GT.0) THEN
            IF ( cdiag(nn).EQ.cdiag(jdiag(k,l)) ) THEN
C-    diagnostics already set ; use the same slot:
             flagD = .FALSE.
             idiag(mId,listId) = -ABS(idiag(k,l))
             mdiag(mId,listId) = mdiag(k,l)
            ENDIF
           ENDIF
          ENDDO
         ENDIF
        ENDDO
        flagP = flagD .AND. gdiag(nn)(5:5).EQ.'P'
        IF ( flagP ) nn = hdiag(nn)
      ENDDO

C---  Set pointer if not already set, otherwise just print a message

      IF ( diagIsPP ) THEN
        WRITE(msgBuf,'(2(A,I6,1X,A))')
     &    'SETDIAG: Diag #', ndId, cdiag(ndId),
     &    ' processed from Diag #',nn,cdiag(nn)
        CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
      ENDIF
      gcode   = gdiag(nn)(1:10)
      IF ( flagD ) THEN
        IF ( diagIsPP ) THEN
C-      Add this diag with negative idiag pointer (since those 2 diags
C        share the same pointer and "nn" will get the positive pointer).
          idiag(mId,listId) = -(ndiagmx+1)
C-      Also add "nn" to the Active list
          k = nActive(listId) + 1
          IF ( k.LE.numperList ) THEN
            jdiag(k,listId) = nn
            idiag(k,listId) = ndiagmx + 1
            flds (k,listId) = cdiag(nn)
          ENDIF
          nActive(listId) = k
        ELSE
          idiag(mId,listId) = ndiagmx + 1
        ENDIF
        ndiagmx = ndiagmx + kdiag(nn)*averageCycle(listId)
        IF ( ndiagmx.GT.numDiags ) THEN
         WRITE(msgBuf,'(A,I6,1X,A)')
     &    'SETDIAG: Not enough space for Diagnostic #',nn,cdiag(nn)
         CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
        ELSE
         WRITE(msgBuf,'(A,2(I3,A),I6,1X,A)') 'SETDIAG: Allocate',
     &                   kdiag(nn), ' x', averageCycle(listId),
     &                ' Levels for Diagnostic #', nn, cdiag(nn)
         CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
        ENDIF
      ELSE
        tmpMsg = ' Diagnostic '
        WRITE(msgBuf,'(3A,I6,1X,2A)') '- NOTE - SETDIAG: ',tmpMsg,
     &           ' #', nn, cdiag(nn), ' is already set'
        CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  Check for Counter Diagnostic
      mate = 0

C-    if Post-Processed diag, activate 2nd components of vector field
      tmpMsg = ' Vector-mate'
      IF ( diagIsPP .AND. gcode(5:5).NE.'P' .AND.
     &    (gcode(1:1).EQ.'U' .OR. gcode(1:1).EQ.'V') ) mate = hdiag(nn)
C-    activate mate if this is a Counter Diagnostic
      IF ( gcode(5:5).EQ.'C') THEN
        mate = hdiag(nn)
        tmpMsg = 'Counter-mate'
      ENDIF

      IF ( mate.GT.0 ) THEN
C--     Seach for the same diag (with same freq) to see if already set
        flagM = .TRUE.
        DO l=1,listId
         IF (flagM .AND. freq(l) .EQ.freq(listId)
     &             .AND. phase(l).EQ.phase(listId)
     &             .AND. averageFreq(l) .EQ.averageFreq(listId)
     &             .AND. averagePhase(l).EQ.averagePhase(listId)
     &             .AND. averageCycle(l).EQ.averageCycle(listId) ) THEN
          DO k=1,MIN(nActive(l),numperList)
           IF (flagM .AND. jdiag(k,l).GT.0) THEN
            IF (cdiag(mate).EQ.cdiag(jdiag(k,l)) ) THEN
C-    diagnostics already set ; use the same slot:
             flagM = .FALSE.
             mdiag(mId,listId) = ABS(idiag(k,l))
            ENDIF
           ENDIF
          ENDDO
         ENDIF
        ENDDO

C---  Set pointer if not already set, otherwise just print a message
        IF ( flagM ) THEN
          mdiag(mId,listId) = ndiagmx + 1
          k = nActive(listId) + 1
          IF ( k.LE.numperList ) THEN
C-      Also add mate to the Active list
            jdiag(k,listId) = mate
            idiag(k,listId) = ndiagmx + 1
            flds (k,listId) = cdiag(mate)
          ENDIF
          nActive(listId) = k
          ndiagmx = ndiagmx + kdiag(mate)*averageCycle(listId)
          IF ( ndiagmx.GT.numDiags ) THEN
           WRITE(msgBuf,'(3A,I6,1X,A)')
     &      'SETDIAG: Not enough space for ',tmpMsg,' #',
     &      mate, cdiag(mate)
           CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
          ELSE
           WRITE(msgBuf,'(A,2(I3,A),I6,1X,A)') 'SETDIAG: Allocate',
     &                     kdiag(mate), ' x', averageCycle(listId),
     &                  ' Levels for Mate Diag. #', mate, cdiag(mate)
           CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
          ENDIF
        ELSE
          WRITE(msgBuf,'(3A,I6,1X,2A)') '- NOTE - SETDIAG: ',tmpMsg,
     &    ' #', mate, cdiag(mate), ' is already set'
          CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
        ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
