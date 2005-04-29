C $Header: /u/gcmpack/MITgcm/pkg/admtlm/admtlm_driver.F,v 1.4 2005/04/29 10:36:45 heimbach Exp $
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

      IF (optimcycle .GT. 0 ) THEN
cph         _BEGIN_MASTER( mythid )
cph         IF ( myProcId .eq. 0 ) THEN
cph            CALL CTRL_UNPACK( .false., mythid )
cph         ENDIF
cph         _END_MASTER( mythid )
         CALL ADMTLM_UPXX( mythid )
      ENDIF
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
cph      _BEGIN_MASTER( mythid )
cph      IF ( myProcId .eq. 0 ) THEN
cph         call CTRL_PACK( .FALSE., mythid )
cph      ENDIF
cph      _END_MASTER( mythid )
c
#endif

#ifndef DISABLE_DEBUGMODE
      IF (debugMode) CALL DEBUG_LEAVE('ADMTLM_DRIVER',myThid)
#endif

      RETURN
      END
