C $Header: /u/gcmpack/MITgcm/model/src/state_summary.F,v 1.6 2001/02/04 14:38:48 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE STATE_SUMMARY( myThid )
C     /==========================================================\
C     | SUBROUTINE STATE_SUMMARY                                 |
C     | o Summarize model prognostic variables.                  |
C     |==========================================================|
C     | This routine can be called at any time during an         |
C     | integration to provide a summary of the model state.     |
C     | Note                                                     |
C     |  1. Under multi-process parallelism the state summary    |
C     |     is only given for the per-process data.              |
C     |  2. Under multi-threading the summary is produced by     |
C     |     the master thread. This threads reads data managed by|
C     |     other threads.                                       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of STATE_SUMMARY
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BARRIER

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Model current state'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      _END_MASTER(myThid)

      _BARRIER


      RETURN
      END

