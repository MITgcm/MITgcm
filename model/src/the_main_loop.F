C $Header: /u/gcmpack/MITgcm/model/src/the_main_loop.F,v 1.150 2013/03/30 13:25:46 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
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
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THE_MAIN_LOOP
C     !INTERFACE:
      SUBROUTINE THE_MAIN_LOOP( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE the_main_loop
C     | o Run the ocean model and evaluate the specified cost function.
C     *================================================================*
C     |
C     | THE_MAIN_LOOP is the toplevel routine for the Tangent Linear and
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
#ifdef ALLOW_AUTODIFF
# ifndef ALLOW_AUTODIFF_OPENAD

c These includes are needed for
c AD-checkpointing.
c They provide the fields to be stored.

#  include "AUTODIFF_MYFIELDS.h"
#  include "GRID.h"
#  include "DYNVARS.h"
#  include "SURFACE.h"
#  include "FFIELDS.h"
#  include "EOS.h"
#  include "AUTODIFF.h"

#  ifdef ALLOW_GENERIC_ADVDIFF
#   include "GAD.h"
#   include "GAD_SOM_VARS.h"
#  endif
#  ifdef ALLOW_MOM_FLUXFORM
#   include "MOM_FLUXFORM.h"
#  endif
#  ifdef ALLOW_CD_CODE
#   include "CD_CODE_VARS.h"
#  endif
#  ifdef ALLOW_PTRACERS
#   include "PTRACERS_SIZE.h"
#   include "PTRACERS_FIELDS.h"
#   include "PTRACERS_START.h"
#  endif
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
#  ifdef ALLOW_OBCS
#   include "OBCS_PARAMS.h"
#   include "OBCS_FIELDS.h"
#   include "OBCS_SEAICE.h"
#   ifdef ALLOW_PTRACERS
#    include "OBCS_PTRACERS.h"
#   endif
#  endif
#  ifdef ALLOW_EXF
#   include "EXF_FIELDS.h"
#   ifdef ALLOW_BULKFORMULAE
#    include "EXF_CONSTANTS.h"
#   endif
#  endif /* ALLOW_EXF */
#  ifdef ALLOW_SEAICE
#   include "SEAICE_SIZE.h"
#   include "SEAICE.h"
#   include "SEAICE_PARAMS.h"
#   include "SEAICE_COST.h"
#   include "SEAICE_TRACER.h"
#  endif
#  ifdef ALLOW_SALT_PLUME
#   include "SALT_PLUME.h"
#  endif
#  ifdef ALLOW_THSICE
#   include "THSICE_SIZE.h"
#   include "THSICE_VARS.h"
#  endif
#  ifdef ALLOW_SHELFICE
#   include "SHELFICE.h"
#   include "SHELFICE_COST.h"
#  endif
#  ifdef ALLOW_STREAMICE
#   include "STREAMICE.h"
#   include "STREAMICE_ADV.h"
#   include "STREAMICE_BDRY.h"
#   include "STREAMICE_CG.h"
#  endif
#  ifdef ALLOW_EBM
#   include "EBM.h"
#  endif
#  ifdef ALLOW_RBCS
#   include "RBCS_SIZE.h"
#   include "RBCS_FIELDS.h"
#  endif
#  ifdef ALLOW_OFFLINE
#   include "OFFLINE.h"
#  endif
#  ifdef ALLOW_CG2D_NSA
#   include "CG2D.h"
#  endif
#  ifdef ALLOW_DIVIDED_ADJOINT_MPI
#   include "mpif.h"
#  endif

#  include "tamc.h"

# ifdef ALLOW_GGL90
#  include "GGL90.h"
# endif
# ifdef ALLOW_PROFILES
#  include "profiles.h"
# endif

# ifdef ALLOW_ECCO_EVOLUTION
#  ifdef ALLOW_ECCO
#   include "ecco_cost.h"
#  endif
# endif

# endif /* undef ALLOW_AUTODIFF_OPENAD */

# ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "CTRL_GENARR.h"
# endif
# ifdef ALLOW_COST
#  include "cost.h"
# endif

#endif /* ALLOW_AUTODIFF */
c**************************************

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     note: under the multi-threaded model myiter and
C           mytime are local variables passed around as routine
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
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MAIN_LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Initialize storage for the cost function evaluation.
CADJ  INIT dummytape = common, 1
c--   Initialize storage for the outermost loop.
# ifndef ALLOW_AUTODIFF_WHTAPEIO
CADJ  INIT tapelev_init   = USER
CADJ  INIT tapelev_ini_bibj_k   = USER
# else
CADJ  INIT tapelev_init   = common, 1
# endif
c
# ifdef ALLOW_TAMC_CHECKPOINTING
#  if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
CADJ  INIT tapelev2 = USER
#  elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
CADJ  INIT tapelev4 = USER
#  else
CADJ  INIT tapelev3 = USER
#  endif
# endif
#endif

#ifdef ALLOW_AUTODIFF
      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
      ikey_dynamics = 1
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE hFacC = tapelev_init, key = 1
# endif
#endif

#ifdef ALLOW_AUTODIFF_OPENAD
# ifdef ALLOW_THETA0_CONTROL
c$openad INDEPENDENT(xx_theta)
# endif
# ifdef ALLOW_SALT0_CONTROL
c$openad INDEPENDENT(xx_salt)
# endif
# ifdef ALLOW_HFLUX0_CONTROL
c$openad INDEPENDENT(xx_hflux0)
# endif
# ifdef ALLOW_SFLUX0_CONTROL
c$openad INDEPENDENT(xx_sflux0)
# endif
# ifdef ALLOW_TAUU0_CONTROL
c$openad INDEPENDENT(xx_tauu0)
# endif
# ifdef ALLOW_TAUV0_CONTROL
c$openad INDEPENDENT(xx_tauv0)
# endif
# ifdef ALLOW_DIFFKR_CONTROL
c$openad INDEPENDENT(xx_diffkr)
# endif
# ifdef ALLOW_KAPGM_CONTROL
c$openad INDEPENDENT(xx_kapgm)
# endif
# ifdef ALLOW_TR10_CONTROL
c$openad INDEPENDENT(xx_tr1)
# endif
# ifdef ALLOW_GENARR2D_CONTROL
c$openad INDEPENDENT(xx_genarr2d)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
c$openad INDEPENDENT(xx_genarr3d)
# endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_VARIA',myThid)
#endif
C--   Set initial conditions (variable arrays)
      CALL TIMER_START('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
      CALL INITIALISE_VARIA( mythid )
      CALL TIMER_STOP ('INITIALISE_VARIA    [THE_MAIN_LOOP]', mythid)
#ifdef ALLOW_ECCO_EVOLUTION
#ifdef ALLOW_ECCO
cph: avoid renewed call of initialise_varia in recomputation loop
cph: in some circumstances
CADJ STORE sbar_gen,tbar_gen  = onetape
#endif
#endif

#ifdef ALLOW_SHOWFLOPS
      CALL TIMER_START('SHOWFLOPS_INIT      [THE_MAIN_LOOP]', mythid)
      CALL SHOWFLOPS_INIT( myThid )
      CALL TIMER_STOP ('SHOWFLOPS_INIT      [THE_MAIN_LOOP]', mythid)
#endif

c--   Do the model integration.
      CALL TIMER_START('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STARTS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<

      CALL TIMER_START('MAIN_DO_LOOP        [THE_MAIN_LOOP]', mythid)
      CALL MAIN_DO_LOOP( myTime, myIter, myThid )
      CALL TIMER_STOP ('MAIN_DO_LOOP        [THE_MAIN_LOOP]', mythid)

c     >>>>>>>>>>>>>>>>>>>>>>>>>>>   LOOP   <<<<<<<<<<<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>>>>>>>>>  STOPS   <<<<<<<<<<<<<<<<<<<<<<<<<<<<

#ifdef ALLOW_ECCO
#ifdef ALLOW_ECCO_EVOLUTION
c--     Accumulate time averages of temperature, salinity
      CALL TIMER_START('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',mythid)
      CALL COST_AVERAGESFIELDS( endtime, mythid )
      CALL TIMER_STOP ('COST_AVERAGESFIELDS [THE_MAIN_LOOP]',mythid)
#endif /* ALLOW_ECCO_EVOLUTION */
#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
      IF (usePROFILES) THEN
c--   Accumulate in-situ time averages of temperature, salinity, SSH.
        CALL TIMER_START('PROFILES_INLOOP    [THE_MAIN_LOOP]', mythid)
        CALL PROFILES_INLOOP( endtime, mythid )
        CALL TIMER_STOP ('PROFILES_INLOOP    [THE_MAIN_LOOP]', mythid)
      ENDIF
#endif

#ifdef ALLOW_ECCO
#ifdef ALLOW_ECCO_EVOLUTION
c--   Compute all ecco cost function contributions.
      CALL TIMER_START('ECCO_COST_DRIVER   [THE_MAIN_LOOP]', mythid)
      CALL ECCO_COST_DRIVER ( endTime, myIter, mythid )
      CALL TIMER_STOP ('ECCO_COST_DRIVER   [THE_MAIN_LOOP]', mythid)
#endif /* ALLOW_ECCO_EVOLUTION */
#endif /* ALLOW_ECCO */

#ifdef ALLOW_COST
c--   Sum all cost function contributions.
      CALL TIMER_START('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)
      CALL COST_FINAL ( mythid )
      CALL TIMER_STOP ('COST_FINAL         [ADJOINT SPIN-DOWN]', mythid)

# ifdef ALLOW_AUTODIFF_OPENAD
c$openad DEPENDENT(fc)
# endif /* ALLOW_AUTODIFF_OPENAD */

#endif /* ALLOW_COST */

      _BARRIER
      CALL TIMER_STOP ('MAIN LOOP           [THE_MAIN_LOOP]', mythid)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MAIN_LOOP',myThid)
#endif

      RETURN
      END
