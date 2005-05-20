C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_setdiag.F,v 1.1 2005/05/20 07:28:52 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_SETDIAG
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_SETDIAG( 
     O                      mate,
     U                      ndiagmx, 
     I                      num, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGSTATS_SETDIAG
C     | o activate statistics diagnostic "num":
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
C     myThid - Thread number for this instance of the routine.
      INTEGER mate
      INTEGER ndiagmx
      INTEGER num
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER stdUnit, errUnit

      CHARACTER*8 parms1
      CHARACTER*3 mate_index
      CHARACTER*(MAX_LEN_MBUF) msgBuf


C **********************************************************************
C ****                SET POINTERS FOR DIAGNOSTIC NUM               ****
C **********************************************************************

      parms1 = gdiag(num)(1:8)
      stdUnit = standardMessageUnit
      errUnit = errorMessageUnit

      IF ( iSdiag(num).EQ.0 ) THEN
        IF (ndiagmx+kdiag(num).GT.diagSt_size) THEN
         WRITE(msgBuf,'(A,I4,1X,A)') 
     &    'SETDIAG: Not enough space for Stats-Diag #', num, cdiag(num)
         CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
        ELSE
         WRITE(msgBuf,'(A,I3,A,I4,1X,A)') 'SETDIAG: Allocate', 
     &    kdiag(num), ' Levels for Stats-Diag #', num, cdiag(num)
         CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
        ENDIF
        iSdiag(num) = ndiagmx + 1
        ndiagmx    = ndiagmx + kdiag(num)
      ELSE
        WRITE(msgBuf,'(A,I4,1X,2A)') 
     &    '- WARNING - SETDIAG: Stats-Diag #', num, cdiag(num),
     &    ' has already been set'
        CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
      ENDIF

c Check for Counter Diagnostic
c ----------------------------
      mate = 0
      IF ( parms1(5:5).EQ.'C') THEN
        mate_index = parms1(6:8)
        READ (mate_index,'(I3)') mate

        IF ( iSdiag(mate).EQ.0 ) THEN
          IF (ndiagmx+kdiag(mate).GT.diagSt_size) THEN
           WRITE(msgBuf,'(A,I4,1X,A)') 
     &      'SETDIAG: Not enough space for Counter Diagnostic #', 
     &      num, cdiag(num)
           CALL PRINT_MESSAGE(msgBuf, errUnit, SQUEEZE_RIGHT, myThid)
          ELSE
           WRITE(msgBuf,'(A,I3,A,I4,1X,A)') 'SETDIAG: Allocate', 
     &     kdiag(mate), ' Levels for Count.Diag #', mate, cdiag(mate)
           CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
          ENDIF
          iSdiag(mate) = ndiagmx + 1
          ndiagmx     = ndiagmx + kdiag(mate)
        ELSE
          WRITE(msgBuf,'(A,I4,1X,2A)') 
     &    '- NOTE - SETDIAG: Counter Diagnostic #', mate, cdiag(mate),
     &    ' has already been set'
          CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid)
          mate = -mate
        ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
