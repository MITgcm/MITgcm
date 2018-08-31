#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_GGL90
# include "GGL90_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif
#ifdef ALLOW_DIC
# include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
c#ifdef ALLOW_ECCO
c# include "ECCO_OPTIONS.h"
c#endif

CBOP
C     !ROUTINE: MAIN_DO_LOOP
C     !INTERFACE:
      SUBROUTINE MAIN_DO_LOOP( iloop, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE the_loop_body
C     | o Run the ocean model and evaluate the specified cost function.
C     *================================================================*
C     |
C     | MAIN_DO_LOOP is the toplevel routine for the Tangent Linear and
C     | Adjoint Model Compiler (TAMC).
C     | For this purpose the initialization
C     | of the model was split into two parts. Those parameters that do
C     | not depend on a specific model run are set in INITIALISE_FIXED,
C     | whereas those that do depend on the specific realization are
C     | initialized in INITIALISE_VARIA.
C     | This routine is to be used in conjuction with the MITgcmuv
C     | checkpoint 37.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c**************************************
#ifdef ALLOW_OPENAD

# ifdef ALLOW_CTRL
#  include "CTRL_SIZE.h"
#  include "ctrl.h"
#  include "ctrl_dummy.h"
#  include "CTRL_GENARR.h"
#  include "CTRL_OBCS.h"
# endif
# ifdef ALLOW_COST
#  include "cost.h"
# endif

# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_FIELDS.h"
#  include "PTRACERS_START.h"
# endif
# ifdef ALLOW_GCHEM
#  include "GCHEM_SIZE.h"
#  include "GCHEM_FIELDS.h"
# endif
# ifdef ALLOW_CFC
#  include "CFC.h"
# endif
# ifdef ALLOW_DIC
#  include "DIC_VARS.h"
#  include "DIC_LOAD.h"
#  include "DIC_ATMOS.h"
#  include "DIC_CTRL.h"
#  include "DIC_COST.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS_PARAMS.h"
#  include "OBCS_FIELDS.h"
#  include "OBCS_SEAICE.h"
#  ifdef ALLOW_PTRACERS
#   include "OBCS_PTRACERS.h"
#  endif
# endif
# ifdef ALLOW_SHELFICE
#  include "SHELFICE.h"
#  include "SHELFICE_COST.h"
# endif
# ifdef ALLOW_STREAMICE
#  include "STREAMICE.h"
#  include "STREAMICE_ADV.h"
#  include "STREAMICE_BDRY.h"
#  include "STREAMICE_CG.h"
# endif

#endif /* ALLOW_OPENAD */
c**************************************

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     note: under the multi-threaded model myIter and
C           myTime are local variables passed around as routine
C           arguments. Although this is fiddly it saves the need to
C           impose additional synchronisation points when they are
C           updated.
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
C     == Functions ==

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER iloop
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('MAIN_DO_LOOP',myThid)
#endif

#ifdef ALLOW_OPENAD
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ifdef ALLOW_OPENAD_DIVA
      DO iloop = 1, nTimeSteps_l2
# else
      DO iloop = 1, nTimeSteps
# endif

#endif /* ALLOW_OPENAD */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('FORWARD_STEP',myThid)
#endif

#ifndef ALLOW_OPENAD
# ifdef ALLOW_ATM2D
        CALL TIMER_START('FORWARD_STEP_ATM2D  [MAIN_DO_LOOP]',myThid)
        CALL FORWARD_STEP_ATM2D( iloop, myTime, myIter, myThid )
        CALL TIMER_STOP ('FORWARD_STEP_ATM2D  [MAIN_DO_LOOP]',myThid)
# else
        CALL TIMER_START('FORWARD_STEP        [MAIN_DO_LOOP]',myThid)
        CALL FORWARD_STEP( iloop, myTime, myIter, myThid )
        CALL TIMER_STOP ('FORWARD_STEP        [MAIN_DO_LOOP]',myThid)
# endif
#else
# ifdef ALLOW_OPENAD_DIVA
       CALL TIMER_START('INNER_DO_LOOP       [MAIN_DO_LOOP]',myThid)
       nTimeSteps_l2 = 2
       CALL INNER_DO_LOOP( iloop, myTime, myIter, myThid )
       CALL TIMER_STOP ('INNER_DO_LOOP       [MAIN_DO_LOOP]',myThid)
# else
       CALL TIMER_START('FORWARD_STEP        [MAIN_DO_LOOP]',myThid)
       nTimeSteps_l2 = 2
       CALL FORWARD_STEP( iloop, myTime, myIter, myThid )
       CALL TIMER_STOP ('FORWARD_STEP        [MAIN_DO_LOOP]',myThid)
# endif
#endif /* ndef ALLOW_OPENAD */

#ifdef ALLOW_OPENAD
      ENDDO
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STOPS   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
#endif /* ALLOW_OPENAD */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('MAIN_DO_LOOP',myThid)
#endif

      RETURN
      END
