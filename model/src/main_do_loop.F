C $Header: /u/gcmpack/MITgcm/model/src/main_do_loop.F,v 1.18 2014/10/20 03:09:03 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
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

C-- The folowing header files are included in the_main_loop.F for TAF
C    and might be needed here (where the iloop is done) for OpenAD
c#  include "AUTODIFF_MYFIELDS.h"
c#  include "GRID.h"
c#  include "DYNVARS.h"
c#  include "SURFACE.h"
c#  include "FFIELDS.h"
c#  include "EOS.h"
c#  include "AUTODIFF.h"
c#  ifdef ALLOW_GENERIC_ADVDIFF
c#   include "GAD.h"
c#   include "GAD_SOM_VARS.h"
c#  endif
c#  ifdef ALLOW_MOM_FLUXFORM
c#   include "MOM_FLUXFORM.h"
c#  endif
c#  ifdef ALLOW_CD_CODE
c#   include "CD_CODE_VARS.h"
c#  endif
c#  ifdef ALLOW_EXF
c#   include "EXF_FIELDS.h"
c#   ifdef ALLOW_BULKFORMULAE
c#    include "EXF_CONSTANTS.h"
c#   endif
c#  endif /* ALLOW_EXF */
c#  ifdef ALLOW_SEAICE
c#   include "SEAICE_SIZE.h"
c#   include "SEAICE.h"
c#   include "SEAICE_PARAMS.h"
c#   include "SEAICE_COST.h"
c#   include "SEAICE_TRACER.h"
c#  endif
c#  ifdef ALLOW_SALT_PLUME
c#   include "SALT_PLUME.h"
c#  endif
c#  ifdef ALLOW_THSICE
c#   include "THSICE_SIZE.h"
c#   include "THSICE_VARS.h"
c#   include "THSICE_COST.h"
c#  endif
c#  ifdef ALLOW_EBM
c#   include "EBM.h"
c#  endif
c#  ifdef ALLOW_RBCS
c#   include "RBCS_SIZE.h"
c#   include "RBCS_FIELDS.h"
c#  endif
c#  ifdef ALLOW_OFFLINE
c#   include "OFFLINE.h"
c#  endif
c#  ifdef ALLOW_CG2D_NSA
c#   include "CG2D.h"
c#  endif
c# ifdef ALLOW_GGL90
c#  include "GGL90.h"
c# endif
c# ifdef ALLOW_PROFILES
c#  include "profiles.h"
c# endif
c# ifdef ALLOW_ECCO_EVOLUTION
c#  ifdef ALLOW_ECCO
c#   include "ecco_cost.h"
c#  endif
c# endif

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
      DO iloop = 1, nTimeSteps
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('FORWARD_STEP',myThid)
#endif

#ifdef ALLOW_ATM2D
        CALL TIMER_START('FORWARD_STEP_ATM2D  [MAIN_DO_LOOP]',myThid)
        CALL FORWARD_STEP_ATM2D( iloop, myTime, myIter, myThid )
        CALL TIMER_STOP ('FORWARD_STEP_ATM2D  [MAIN_DO_LOOP]',myThid)
#else
        CALL TIMER_START('FORWARD_STEP        [MAIN_DO_LOOP]',myThid)
        CALL FORWARD_STEP( iloop, myTime, myIter, myThid )
        CALL TIMER_STOP ('FORWARD_STEP        [MAIN_DO_LOOP]',myThid)
#endif

#ifdef ALLOW_OPENAD
      ENDDO
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STOPS   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('MAIN_DO_LOOP',myThid)
#endif

      RETURN
      END
