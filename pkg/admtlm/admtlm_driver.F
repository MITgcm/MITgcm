C $Header: /u/gcmpack/MITgcm/pkg/admtlm/admtlm_driver.F,v 1.1 2004/10/26 20:11:47 heimbach Exp $
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

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "ctrl.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     == Local ==
      INTEGER myCurrentIter
      _RL     myCurrentTime

CEOP

#ifndef DISABLE_DEBUGMODE
      IF (debugMode) CALL DEBUG_ENTER('ADMTLM_DRIVER',myThid)
#endif

#if ( defined (ALLOW_ADMTLM_RUN) )

      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'g_'

      _BEGIN_MASTER( mythid )
      IF (myProcId .eq. 0) THEN
         CALL CTRL_UNPACK( mycurrentiter, mycurrenttime, mythid )
      ENDIF
      _END_MASTER( mythid )
c
      CALL TIMER_START('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
      CALL G_THE_MAIN_LOOP ( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
c
      _BEGIN_MASTER( mythid )
      IF ( myProcId .eq. 0) THEN
         call CTRL_PACK( mycurrentiter, mycurrenttime, mythid )
      ENDIF
      _END_MASTER( mythid )
c
      CALL ADMTLM_METRIC ( mythid )
      CALL ADMTLM_MAP ( mythid )
c
      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'ad'
c
      _BEGIN_MASTER( mythid )
      IF (myProcId .eq. 0) THEN
         CALL CTRL_UNPACK( mycurrentiter, mycurrenttime, mythid )
      ENDIF
      _END_MASTER( mythid )
c
      CALL TIMER_START('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
      CALL ADTHE_MAIN_LOOP ( mythid )
      CALL TIMER_STOP ('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
c
      _BEGIN_MASTER( mythid )
      IF ( myProcId .eq. 0) THEN
         call CTRL_PACK( mycurrentiter, mycurrenttime, mythid )
      ENDIF
      _END_MASTER( mythid )
c
# ifdef ALLOW_GRADIENT_CHECK
      IF ( useGrdchk ) THEN
         yadprefix = 'g_'
         CALL TIMER_START('GRDCHK_MAIN         [THE_MODEL_MAIN]',mythid)
         CALL GRDCHK_MAIN( mythid )
         CALL TIMER_STOP ('GRDCHK_MAIN         [THE_MODEL_MAIN]',mythid)
         _BARRIER
      ENDIF
# endif
c
#endif

#ifndef DISABLE_DEBUGMODE
      IF (debugMode) CALL DEBUG_LEAVE('ADMTLM_DRIVER',myThid)
#endif

      RETURN
      END
