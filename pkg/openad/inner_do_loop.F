#include "OPENAD_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_DIC
# include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif
#ifdef ALLOW_GGL90
# include "GGL90_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
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
C     !ROUTINE: INNER_DO_LOOP
C     !INTERFACE:
      SUBROUTINE INNER_DO_LOOP( iloop, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE INNER_DO_LOOP
C     | o Inner time-stepping "do-loop" for OPENAD_DIVA
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#ifdef ALLOW_OPENAD_DIVA
C-- Header files that are included in the_main_loop.F for TAF (e.g., DYNVARS.h)
C    might be needed here (where the iloop is done) for OpenAD

# ifdef ALLOW_CTRL
#  include "CTRL_SIZE.h"
#  include "CTRL.h"
#  include "CTRL_DUMMY.h"
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

#endif /* ALLOW_OPENAD_DIVA */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      INTEGER iloop
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OPENAD_DIVA
C     !FUNCTIONS:

C     !LOCAL VARIABLES:
      INTEGER iloop1
      INTEGER prod
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INNER_DO_LOOP',myThid)
#endif

C--   Start Inner time-stepping Loop:
      DO iloop1 = 1, nTimeSteps
        prod = (iloop-1)*nTimeSteps_l2+iloop1

#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('FORWARD_STEP',myThid)
#endif
        CALL TIMER_START('FORWARD_STEP        [INNER_DO_LOOP]',myThid)
        CALL FORWARD_STEP( prod, myTime, myIter, myThid )
        CALL TIMER_STOP ('FORWARD_STEP        [INNER_DO_LOOP]',myThid)

      ENDDO
C--   end of Inner time-stepping Loop

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INNER_DO_LOOP',myThid)
#endif

#endif /* ALLOW_OPENAD_DIVA */
      RETURN
      END
