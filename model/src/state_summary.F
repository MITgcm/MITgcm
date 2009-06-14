C $Header: /u/gcmpack/MITgcm/model/src/state_summary.F,v 1.8 2009/06/14 21:45:12 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: STATE_SUMMARY
C     !INTERFACE:
      SUBROUTINE STATE_SUMMARY( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE STATE_SUMMARY                                  
C     | o Summarize model prognostic variables.                   
C     *==========================================================*
C     | This routine can be called at any time during an          
C     | integration to provide a summary of the model state.      
C     | Note                                                      
C     |  1. Under multi-process parallelism the state summary     
C     |     is only given for the per-process data.               
C     |  2. Under multi-threading the summary is produced by      
C     |     the master thread. This threads reads data managed by 
C     |     other threads.                                        
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of STATE_SUMMARY
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') '// Model current state'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      _END_MASTER(myThid)

      RETURN
      END
