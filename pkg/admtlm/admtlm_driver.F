C $Header: /u/gcmpack/MITgcm/pkg/admtlm/admtlm_driver.F,v 1.3 2004/11/17 03:06:11 heimbach Exp $
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
         _BEGIN_MASTER( mythid )
         IF ( myProcId .eq. 0 ) THEN
            CALL CTRL_UNPACK( .false., mythid )
         ENDIF
         _END_MASTER( mythid )
         CALL ADMTLM_UPXX( mythid )
      ENDIF
c
      CALL TIMER_START('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
      CALL G_THE_MAIN_LOOP ( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
c
c      _BEGIN_MASTER( mythid )
c      IF ( myProcId .eq. 0) THEN
c         call CTRL_PACK( mycurrentiter, mycurrenttime, mythid )
c      ENDIF
c      _END_MASTER( mythid )
c
      CALL ADMTLM_METRIC ( mythid )
      CALL ADMTLM_MAP ( mythid )
c
      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'ad'
c
c      _BEGIN_MASTER( mythid )
c      IF (myProcId .eq. 0) THEN
c         CALL CTRL_UNPACK( mycurrentiter, mycurrenttime, mythid )
c      ENDIF
c      _END_MASTER( mythid )
c
      CALL CTRL_INIT( mythid )
c
      CALL TIMER_START('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
      CALL ADTHE_MAIN_LOOP (  myCurrentTime, myCurrentIter, mythid )
      CALL TIMER_STOP ('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
c
      _BEGIN_MASTER( mythid )
      IF ( myProcId .eq. 0 ) THEN
         call CTRL_PACK( .FALSE., mythid )
      ENDIF
      _END_MASTER( mythid )
c
#endif

#ifndef DISABLE_DEBUGMODE
      IF (debugMode) CALL DEBUG_LEAVE('ADMTLM_DRIVER',myThid)
#endif

      RETURN
      END
