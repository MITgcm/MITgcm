C $Header: /u/gcmpack/MITgcm/pkg/admtlm/admtlm_driver.F,v 1.5 2005/11/01 04:09:46 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP

C     !ROUTINE: ADMTLM_DRIVER

C     !INTERFACE:
      SUBROUTINE ADMTLM_DRIVER( myThid )
      IMPLICIT NONE

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ADMTLM_DRIVER                                 
C     | o Master controlling routine for model using the MITgcm   
C     |   UV parallel wrapper.                                    
C     *==========================================================*
C     \ev

C     !USES:
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

#ifdef ALLOW_ADMTLM
# include "tamc.h"
# include "ctrl.h"
# include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     == Local ==
      INTEGER myCurrentIter
      _RL     myCurrentTime
C
CEOP

#ifndef DISABLE_DEBUGMODE
      IF (debugMode) CALL DEBUG_ENTER('ADMTLM_DRIVER',myThid)
#endif

#if ( defined (ALLOW_ADMTLM) )

      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'g_'

      CALL CTRL_INIT( mythid )
c
      CALL TIMER_START('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
      CALL G_THE_MAIN_LOOP ( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
c
      CALL ADMTLM_METRIC ( mythid )
      CALL ADMTLM_MAP ( mythid )
c
      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'ad'
c
      CALL CTRL_INIT( mythid )
c
      CALL TIMER_START('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
      CALL ADTHE_MAIN_LOOP (  myCurrentTime, myCurrentIter, mythid )
      CALL TIMER_STOP ('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
c
#endif

#ifndef DISABLE_DEBUGMODE
      IF (debugMode) CALL DEBUG_LEAVE('ADMTLM_DRIVER',myThid)
#endif

      RETURN
      END
