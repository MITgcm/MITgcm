#include "ADMTLM_OPTIONS.h"

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
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
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

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('ADMTLM_DRIVER',myThid)
#endif

#if ( defined (ALLOW_ADMTLM) )

      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'g_'

      CALL CTRL_INIT( mythid )
c
      CALL TIMER_START('ADMTLM_DRIVER             [TANGENT RUN]',mythid)
      CALL G_THE_MAIN_LOOP ( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('ADMTLM_DRIVER             [TANGENT RUN]',mythid)
c
      myCurrentTime = startTime
      myCurrentIter = nIter0
      yadprefix = 'ad'
c
      CALL CTRL_INIT( mythid )
c
      if ( doAdmtlmBypassAD ) then
c
#ifdef ALLOW_BYPASSAD
         CALL TIMER_START('ADMTLM_DRIVER             [BYPASSAD]',mythid)
         CALL ADMTLM_BYPASSAD( mythid )
         CALL TIMER_STOP ('ADMTLM_DRIVER             [BYPASSAD]',mythid)
#endif
c
      else
c
         CALL ADMTLM_METRIC ( mythid )
         CALL ADMTLM_MAP ( mythid )
c
         CALL TIMER_START('ADMTLM_DRIVER          [ADJOINT RUN]',mythid)
         CALL ADTHE_MAIN_LOOP (  myCurrentTime, myCurrentIter, mythid )
         CALL TIMER_STOP ('ADMTLM_DRIVER          [ADJOINT RUN]',mythid)
c
      endif
c
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('ADMTLM_DRIVER',myThid)
#endif

      RETURN
      END
