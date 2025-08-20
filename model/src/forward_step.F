#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
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
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif
#ifdef ALLOW_DIC
# include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_RBCS
# include "RBCS_OPTIONS.h"
#endif

#define ALLOW_MOM_STEPPING
#if ( defined (ALLOW_AUTODIFF) && defined (ALLOW_OFFLINE) )
# undef ALLOW_MOM_STEPPING
#endif

CBOP
C     !ROUTINE: FORWARD_STEP
C     !INTERFACE:
      SUBROUTINE FORWARD_STEP( iloop, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE forward_step
C     | o Step forward in time the model variables for one time-step
C     *=================================================================
C     | The algorithm...
C     |
C     | "Calculation of Gs"
C     | ===================
C     | This is where all the accelerations and tendencies (ie.
C     | physics, parameterizations etc...) are calculated
C     |   rho = rho ( theta[n], salt[n] )
C     |   b   = b(rho, theta)
C     |   K31 = K31 ( rho )
C     |   Gu[n] = Gu( u[n], v[n], wVel, b, ... )
C     |   Gv[n] = Gv( u[n], v[n], wVel, b, ... )
C     |   Gt[n] = Gt( theta[n], u[n], v[n], wVel, K31, ... )
C     |   Gs[n] = Gs( salt[n], u[n], v[n], wVel, K31, ... )
C     |
C     | "Time-stepping" or "Prediction"
C     | ================================
C     | The models variables are stepped forward with the appropriate
C     | time-stepping scheme (currently we use Adams-Bashforth II)
C     | - For momentum, the result is always *only* a "prediction"
C     | in that the flow may be divergent and will be "corrected"
C     | later with a surface pressure gradient.
C     | - Normally for tracers the result is the new field at time
C     | level [n+1} *BUT* in the case of implicit diffusion the result
C     | is also *only* a prediction.
C     | - We denote "predictors" with an asterisk (*).
C     |   U* = U[n] + dt x ( 3/2 Gu[n] - 1/2 Gu[n-1] )
C     |   V* = V[n] + dt x ( 3/2 Gv[n] - 1/2 Gv[n-1] )
C     |   theta[n+1] = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   salt[n+1]  = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     | With implicit diffusion:
C     |   theta* = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   salt*  = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   (1 + dt * K * d_zz) theta[n+1] = theta*
C     |   (1 + dt * K * d_zz) salt[n+1]  = salt*
C     |
C     | "Correction Step"
C     | =================
C     | Here we update the horizontal velocities with the surface
C     | pressure such that the resulting flow is either consistent
C     | with the free-surface evolution or the rigid-lid:
C     |   U[n] = U* + dt x d/dx P
C     |   V[n] = V* + dt x d/dy P
C     |   W[n] = W* + dt x d/dz P  (NH mode)
C     *=================================================================
C     \ev

C     !CALLING SEQUENCE:
C     FORWARD_STEP
C       |
C       |-- AUTODIFF_INADMODE_UNSET
C       |
C       |-- SHELFICE_REMESHING
C       |
C       |-- RESET_NLFS_VARS
C       |-- UPDATE_R_STAR
C       |-- UPDATE_SURF_DR
C       |
C       |-- PTRACERS_SWITCH_ONOFF
C       |
C       |-- DIAGNOSTICS_SWITCH_ONOFF
C       |-- DO_STATEVARS_DIAGS
C       |
C       |-- NEST_CHILD_SETMEMO
C       |-- NEST_PARENT_IO_1
C       |
C       |-- LOAD_FIELDS_DRIVER
C       |
C       |-- BULKF_FORCING
C       |
C       |-- CHEAPAML
C       |
C       |-- CTRL_MAP_FORCING
C       |-- DUMMY_IN_STEPPING
C       |
C       |-- CPL_EXPORT_IMPORT_DATA
C       |
C       |-- OASIS_PUT
C       |-- OASIS_GET
C       |
C       |-- EBM_DRIVER
C       |
C       |-- DO_ATMOSPHERIC_PHYS
C       |
C       |-- DO_OCEANIC_PHYS
C       |
C       |-- STREAMICE_TIMESTEP
C       |
C       |-- GCHEM_CALC_TENDENCY
C       |
C       |-- LONGSTEP_AVERAGE
C       |-- LONGSTEP_THERMODYNAMICS
C       |
C       |-- THERMODYNAMICS
C       |
C       |-- LONGSTEP_AVERAGE
C       |-- LONGSTEP_THERMODYNAMICS
C       |
C       |-- DO_STAGGER_FIELDS_EXCHANGES
C       |
C       |-- DYNAMICS
C       |
C       |-- MNC_UPDATE_TIME
C       |
C       |-- OFFLINE_FIELDS_LOAD
C       |
C       |-- UPDATE_R_STAR
C       |-- UPDATE_SIGMA
C       |-- UPDATE_SURF_DR
C       |-- UPDATE_CG2D
C       |
C       |-- SHAP_FILT_APPLY_UV
C       |-- ZONAL_FILT_APPLY_UV
C       |
C       |-- SOLVE_FOR_PRESSURE
C       |
C       |-- MOMENTUM_CORRECTION_STEP
C       |
C       |-- INTEGR_CONTINUITY
C       |
C       |-- CALC_R_STAR
C       |-- CALC_SURF_DR
C       |
C       |-- DO_STAGGER_FIELDS_EXCHANGES
C       |
C       |-- DO_STATEVARS_DIAGS
C       |
C       |-- THERMODYNAMICS
C       |
C       |-- TRACERS_CORRECTION_STEP
C       |
C       |-- LONGSTEP_AVERAGE
C       |-- LONGSTEP_THERMODYNAMICS
C       |
C       |-- GCHEM_FORCING_SEP
C       |
C       |-- DO_FIELDS_BLOCKING_EXCHANGES
C       |
C       |-- DO_STATEVARS_DIAGS
C       |
C       |-- GRIDALT_UPDATE
C       |-- STEP_FIZHI_CORR
C       |
C       |-- FLT_MAIN
C       |
C       |-- DO_STATEVARS_TAVE
C       |
C       |-- NEST_PARENT_IO_2
C       |-- NEST_CHILD_TRANSP
C       |
C       |-- MONITOR
C       |
C       |-- COST_TILE
C       |
C       |-- DO_THE_MODEL_IO
C       |
C       |-- PTRACERS_RESET
C       |
C       |-- DO_WRITE_PICKUP
C       |
C       |-- AUTODIFF_INADMODE_SET
C       |
C       |-- SHOWFLOPS_INLOOP

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

#ifdef HAVE_SIGREG
#include "SIGREG.h"
#endif

#ifdef ALLOW_SHAP_FILT
# include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
# include "ZONAL_FILT.h"
#endif

#ifdef ALLOW_LONGSTEP
# include "LONGSTEP_PARAMS.h"
# include "LONGSTEP.h"
#endif

#ifdef ALLOW_AUTODIFF
# include "FFIELDS.h"
# include "SURFACE.h"

# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
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
# ifdef ALLOW_ECCO
#  include "ecco_cost.h"
# endif
# include "EOS.h"
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
#  include "GRID.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_EXF
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
#  include "EXF_INTERP_SIZE.h"
#  include "EXF_FIELDS.h"
# endif
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_GENERIC_ADVDIFF
#  include "GAD.h"
#  include "GAD_SOM_VARS.h"
# endif
# ifdef ALLOW_GGL90
#  include "GGL90.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_FIELDS.h"
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
#  include "DIC_COST.h"
# endif
# ifdef ALLOW_BLING
#  include "BLING_VARS.h"
#  include "BLING_LOAD.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS_PARAMS.h"
#  include "OBCS_FIELDS.h"
#  include "OBCS_SEAICE.h"
#  ifdef ALLOW_PTRACERS
#   include "OBCS_PTRACERS.h"
#  endif
# endif
# ifdef ALLOW_THSICE
#  include "THSICE_PARAMS.h"
#  include "THSICE_SIZE.h"
#  include "THSICE_VARS.h"
#  include "THSICE_COST.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
#  include "SEAICE_COST.h"
# endif
# ifdef ALLOW_SALT_PLUME
#  include "SALT_PLUME.h"
# endif
# ifdef ALLOW_DOWN_SLOPE
#  include "DWNSLP_SIZE.h"
#  include "DWNSLP_VARS.h"
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
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_RBCS
#  include "RBCS_SIZE.h"
#  include "RBCS_FIELDS.h"
# endif
# ifdef ALLOW_OFFLINE
#  include "OFFLINE.h"
# endif
# if (defined ALLOW_CG2D_NSA || defined NONLIN_FRSURF || \
      defined ALLOW_DEPTH_CONTROL)
#  include "CG2D.h"
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_TAPENADE
c# ifdef ALLOW_KPP
c#  include "KPP_PARAMS.h"
c# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI_TAVE.h"
# endif
# ifdef ALLOW_TIMEAVE
#  include "TIMEAVE_STATV.h"
# endif
# ifdef ALLOW_MOM_FLUXFORM
#  include "MOM_FLUXFORM.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_PARAM.h"
#  include "EXF_INTERP_PARAM.h"
# endif
#endif /* ALLOW_TAPENADE */

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
      INTEGER iloop
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     modelEnd  :: true if reaching the end of the run
C     myTimeBeg :: time at beginning of time step (needed by longstep)
C     myIterBeg :: iteration number at beginning of time step
      LOGICAL modelEnd
#ifdef ALLOW_LONGSTEP
      INTEGER myIterBeg
      _RL myTimeBeg
#endif /* ALLOW_LONGSTEP */
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('FORWARD_STEP',myThid)
#endif

#ifdef ALLOW_AUTODIFF
C--   Reset the model iteration counter and the model time.
      myIter = nIter0 + (iloop-1)
      myTime = startTime + deltaTClock*(iLoop-1)
#endif

#ifdef ALLOW_AUTODIFF
      CALL AUTODIFF_INADMODE_UNSET( myTime, myIter, myThid )
#endif

#ifdef ALLOW_LONGSTEP
C     store this for longstep_average with staggerTimeStep
C     which is called after myIter and myTime are incremented
C     but needs iter/time at beginning of time step
      myIterBeg = myIter
      myTimeBeg = myTime
#endif /* ALLOW_LONGSTEP */

#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "checkpoint_lev1_directives.h"
#include "checkpoint_lev1_template.h"
c**************************************
#endif

#ifdef ALLOW_SHELFICE_REMESHING
      IF ( useShelfIce ) THEN
C--   Remesh shelfice
        CALL TIMER_START('SHELFICE_REMESHING     [FORWARD_STEP]',myThid)
        CALL SHELFICE_REMESHING ( myTime, myIter, myThid )
        CALL TIMER_STOP('SHELFICE_REMESHING     [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_SHELFICE_REMESHING */

C--   Reset geometric factors (hFacC,W,S & recip_hFac) to their current values:
C     added to simplify adjoint derivation - no effect in forward run
#ifdef NONLIN_FRSURF
#ifndef ALLOW_AUTODIFF
      IF ( doResetHFactors ) THEN
#endif
       CALL RESET_NLFS_VARS( myTime, myIter, myThid )
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rStarFacC, rStarFacS, rStarFacW =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
# endif
        CALL TIMER_START('UPDATE_R_STAR       [FORWARD_STEP]',myThid)
        CALL UPDATE_R_STAR( .FALSE., myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_R_STAR       [FORWARD_STEP]',myThid)
# endif /* DISABLE_RSTAR_CODE */
       ELSE
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFac_surfC, hFac_surfS, hFac_surfW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
#endif
        CALL TIMER_START('UPDATE_SURF_DR      [FORWARD_STEP]',myThid)
        CALL UPDATE_SURF_DR( .FALSE., myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_SURF_DR      [FORWARD_STEP]',myThid)
       ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFacC, hFacS, hFacW =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW  =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
#endif
#ifndef ALLOW_AUTODIFF
      ENDIF
#endif
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_PTRACERS
C--   Switch on/off individual tracer time-stepping
      IF ( usePTRACERS ) THEN
        CALL PTRACERS_SWITCH_ONOFF( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_PTRACERS */

C--   Switch on/off diagnostics for snap-shot output:
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_SWITCH_ONOFF( 1, myTime, myIter, myThid )
C--   State-variables diagnostics
        CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
        CALL DO_STATEVARS_DIAGS( myTime, 0, myIter, myThid )
        CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_NEST_CHILD
      IF ( useNEST_CHILD) THEN
        CALL NEST_CHILD_SETMEMO( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_NEST_CHILD */

#ifdef ALLOW_NEST_PARENT
      IF ( useNEST_PARENT) THEN
        CALL NEST_PARENT_IO_1( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_NEST_PARENT */

C--   Call driver to load external forcing fields from file
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('LOAD_FIELDS_DRIVER',myThid)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
cph Important STORE that avoids hidden recomp. of load_fields_driver
CADJ STORE theta      = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE uVel, vVel = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif
      CALL TIMER_START('LOAD_FIELDS_DRIVER  [FORWARD_STEP]',myThid)
      CALL LOAD_FIELDS_DRIVER( myTime, myIter, myThid )
      CALL TIMER_STOP ('LOAD_FIELDS_DRIVER  [FORWARD_STEP]',myThid)

C--   Call Bulk-Formulae forcing package
#ifdef ALLOW_BULK_FORCE
      IF ( useBulkForce ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('BULKF_FORCING',myThid)
#endif
        CALL TIMER_START('BULKF_FORCING       [FORWARD_STEP]',myThid)
C-    calculate qnet and empmr (and wind stress)
        CALL BULKF_FORCING( myTime, myIter, myThid )
        CALL TIMER_STOP ('BULKF_FORCING       [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_BULK_FORCE */

C--   Call external chepaml forcing package
#ifdef ALLOW_CHEAPAML
      IF ( useCheapAML  ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CHEAPAML',myThid)
#endif
        CALL TIMER_START('CHEAPAML      [FORWARD_STEP]',myThid)
C-    calculate qnet (and wind stress)
        CALL CHEAPAML( myTime, myIter,myThid )
        CALL TIMER_STOP ('CHEAPAML      [FORWARD_STEP]',myThid)
      ENDIF
#endif /*ALLOW_CHEAPAML */

#ifdef ALLOW_CTRL
C--   Add control vector for forcing and parameter fields
      IF ( useCTRL ) THEN
       CALL TIMER_START('CTRL_MAP_FORCING  [FORWARD_STEP]',myThid)
       CALL CTRL_MAP_FORCING( myTime, myIter, myThid )
       CALL TIMER_STOP ('CTRL_MAP_FORCING  [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_MONITOR
      CALL DUMMY_IN_STEPPING( myTime, myIter, myThid )
#endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
C      Post coupling data that I export.
C      Read in coupling data that I import.
         CALL TIMER_START('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
         CALL CPL_EXPORT_IMPORT_DATA( myTime, myIter, myThid )
         CALL TIMER_STOP ('CPL_EXPORT-IMPORT  [FORWARD_STEP]',myThid)
      ENDIF
#endif /* COMPONENT_MODULE */
#ifdef ALLOW_OASIS
      IF ( useOASIS ) THEN
       CALL TIMER_START('OASIS_PUT-GET  [FORWARD_STEP]',myThid)
C      Post coupling data that I export.
       CALL OASIS_PUT( myTime, myIter, myThid )
C      Read in coupling data that I import.
       CALL OASIS_GET( myTime, myIter, myThid )
       CALL TIMER_STOP ('OASIS_PUT-GET  [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_OASIS */

#ifdef ALLOW_EBM
      IF ( useEBM ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EBM',myThid)
# endif
        CALL TIMER_START('EBM                [FORWARD_STEP]',myThid)
        CALL EBM_DRIVER ( myTime, myIter, myThid )
        CALL TIMER_STOP ('EBM                [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_EBM */

#ifdef ALLOW_AUTODIFF_TAMC
C******************************************************
C     Store surface forcing fields if required to avoid
C     calling load_fields_driver in forward_step_ad
# include "check_lev1_dir_forcing.h"
C******************************************************
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Step forward fields and calculate time tendency terms.

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DO_ATMOSPHERIC_PHYS',myThid)
#endif
      CALL TIMER_START('DO_ATMOSPHERIC_PHYS [FORWARD_STEP]',myThid)
      CALL DO_ATMOSPHERIC_PHYS( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_ATMOSPHERIC_PHYS [FORWARD_STEP]',myThid)

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_KPP
CADJ STORE uVel, vVel = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_KPP */
# ifdef EXACT_CONSERV
CADJ STORE EmPmR      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE PmEpR      = comlev1, key = ikey_dynamics, kind = isbyte
# endif
# ifdef ALLOW_OBCS
CADJ STORE salt       = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE totphihyd  = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_OBCS */
# ifdef ALLOW_PTRACERS
CADJ STORE pTracer    = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_PTRACERS */
# ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacC      = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('DO_OCEANIC_PHYS',myThid)
#endif
       CALL TIMER_START('DO_OCEANIC_PHYS     [FORWARD_STEP]',myThid)
       CALL DO_OCEANIC_PHYS( myTime, myIter, myThid )
       CALL TIMER_STOP ('DO_OCEANIC_PHYS     [FORWARD_STEP]',myThid)

#ifdef ALLOW_STREAMICE
       IF (useStreamIce) THEN
         CALL STREAMICE_TIMESTEP ( myThid, myIter,
     &                             iLoop, myTime )
       ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C***********************************************************************
C     This block of store directives is for variables that have been
C     computed in do_oceanics_phys. Storing them here avoids calling
C     do_oceanic_phys again in forward_step_ad, which should improve
C     performance because expensive parts of the model are not
C     recomputed (e.g. seaice).
# include "check_lev1_dir_ocephys.h"
C***********************************************************************
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef GCHEM_ADD2TR_TENDENCY
#ifndef ALLOW_AUTODIFF
      IF ( useGCHEM ) THEN
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE pTracer     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE theta, salt = comlev1, key=ikey_dynamics, kind=isbyte
#endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GCHEM_CALC_TENDENCY',myThid)
#endif
        CALL TIMER_START('GCHEM_CALC_TENDENCY [FORWARD_STEP]',myThid)
        CALL GCHEM_CALC_TENDENCY( myTime, myIter, myThid )
        CALL TIMER_STOP ('GCHEM_CALC_TENDENCY [FORWARD_STEP]',myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gchemTendency = comlev1, key=ikey_dynamics, kind=isbyte
#endif
#ifndef ALLOW_AUTODIFF
      ENDIF
#endif
#endif /* GCHEM_ADD2TR_TENDENCY */

#ifdef ALLOW_LONGSTEP
      IF ( usePTRACERS .AND. LS_whenToSample .EQ. 0 ) THEN
C       Average all variables before advection (but after do_oceanic_phys
C       where Qsw, KPP and GMRedi stuff is computed).
C       This is like diagnostics package and will reproduce offline
C       results.
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('LONGSTEP_AVERAGE',myThid)
#endif
        CALL TIMER_START('LONGSTEP_AVERAGE    [FORWARD_STEP]',myThid)
        CALL LONGSTEP_AVERAGE( myTime, myIter, myThid )
        CALL TIMER_STOP ('LONGSTEP_AVERAGE    [FORWARD_STEP]',myThid)

#ifdef ALLOW_DEBUG
        IF (debugMode)
     &    CALL DEBUG_CALL('LONGSTEP_THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('LONGSTEP_THERMODYNAMICS      [FORWARD_STEP]',
     &                   myThid)
        CALL LONGSTEP_THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('LONGSTEP_THERMODYNAMICS      [FORWARD_STEP]',
     &                    myThid)
      ENDIF
#endif /* ALLOW_LONGSTEP */

      IF ( .NOT.staggerTimeStep ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wVel = comlev1, key = ikey_dynamics, kind = isbyte
#endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',myThid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',myThid)
C--     if not staggerTimeStep: end
      ENDIF
#if (defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_OBCS)
CADJ STORE salt, theta = comlev1, key = ikey_dynamics, kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC && ALLOW_OBCS */

#ifdef ALLOW_LONGSTEP
      IF ( usePTRACERS .AND. LS_whenToSample .EQ. 1 ) THEN
C       Average T and S after thermodynamics, but U,V,W before dynamics.
C       This will reproduce online results with staggerTimeStep=.FALSE.
C       for LS_nIter=1
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('LONGSTEP_AVERAGE',myThid)
#endif
        CALL TIMER_START('LONGSTEP_AVERAGE    [FORWARD_STEP]',myThid)
        CALL LONGSTEP_AVERAGE( myTime, myIter, myThid )
        CALL TIMER_STOP ('LONGSTEP_AVERAGE    [FORWARD_STEP]',myThid)

#ifdef ALLOW_DEBUG
        IF (debugMode)
     &    CALL DEBUG_CALL('LONGSTEP_THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('LONGSTEP_THERMODYNAMICS      [FORWARD_STEP]',
     &                   myThid)
        CALL LONGSTEP_THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('LONGSTEP_THERMODYNAMICS      [FORWARD_STEP]',
     &                   myThid)
      ENDIF
#endif /* ALLOW_LONGSTEP */

c #ifdef ALLOW_NONHYDROSTATIC
      IF ( implicitIntGravWave ) THEN
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
      ENDIF
c #endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etaN = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacC, hFacS, hFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_DEPTH_CONTROL */
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Step forward fields and calculate time tendency terms.
#ifdef ALLOW_MOM_STEPPING
#ifndef ALLOW_AUTODIFF
      IF ( momStepping ) THEN
#endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('DYNAMICS',myThid)
#endif
        CALL TIMER_START('DYNAMICS            [FORWARD_STEP]',myThid)
        CALL DYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('DYNAMICS            [FORWARD_STEP]',myThid)
#ifndef ALLOW_AUTODIFF
      ENDIF
#endif
#endif /* ALLOW_MOM_STEPPING */

#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE gU, gV  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif
#endif

C--   Update time-counter
      myIter = nIter0 + iLoop
      myTime = startTime + deltaTClock*iLoop

#ifdef ALLOW_MNC
C     Update MNC time information
      IF ( useMNC ) THEN
         CALL MNC_UPDATE_TIME( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_OFFLINE
C     Load new Offline fields and update state-variable
      IF ( useOffLine ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OFFLINE_FIELDS_LOAD',myThid)
#endif /* ALLOW_DEBUG */
        CALL TIMER_START('OFFLINE_FLDS_LOAD   [FORWARD_STEP]',myThid)
        CALL OFFLINE_FIELDS_LOAD( myTime, myIter, myThid )
        CALL TIMER_STOP ('OFFLINE_FLDS_LOAD   [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_OFFLINE */

C--   Update geometric factors:
#ifdef NONLIN_FRSURF
C-    update hfacC,W,S and recip_hFac according to etaH(n+1) :
      IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rStarFacC, rStarFacS, rStarFacW =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
# endif
        CALL TIMER_START('UPDATE_R_STAR       [FORWARD_STEP]',myThid)
        CALL UPDATE_R_STAR( .TRUE., myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_R_STAR       [FORWARD_STEP]',myThid)
# endif /* DISABLE_RSTAR_CODE */
      ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
        CALL UPDATE_SIGMA( etaH, myTime, myIter, myThid )
# endif /* DISABLE_RSTAR_CODE */
      ELSE
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFac_surfC, hFac_surfS, hFac_surfW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
# endif
        CALL TIMER_START('UPDATE_SURF_DR      [FORWARD_STEP]',myThid)
        CALL UPDATE_SURF_DR( .TRUE., myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_SURF_DR      [FORWARD_STEP]',myThid)
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFacC, hFacS, hFacW =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW  =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif /* NONLIN_FRSURF */

#if ( defined NONLIN_FRSURF || defined ALLOW_SOLVE4_PS_AND_DRAG || \
      defined ALLOW_CG2D_NSA || defined ALLOW_DEPTH_CONTROL )
C-    update CG2D matrix (and preconditioner)
      IF ( momStepping .AND.
     &     ( nonlinFreeSurf.GT.2 .OR. selectImplicitDrag.EQ.2 ) ) THEN
        CALL TIMER_START('UPDATE_CG2D         [FORWARD_STEP]',myThid)
        CALL UPDATE_CG2D( myTime, myIter, myThid )
        CALL TIMER_STOP ('UPDATE_CG2D         [FORWARD_STEP]',myThid)
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE aW2d, aS2d, aC2d, pC, pS, pW =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif /* NONLIN_FRSURF or ALLOW_SOLVE4_PS_AND_DRAG */

C--   Apply Filters to u*,v* before SOLVE_FOR_PRESSURE
#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT .AND. shap_filt_uvStar) THEN
        CALL TIMER_START('SHAP_FILT_UV        [FORWARD_STEP]',myThid)
        CALL SHAP_FILT_APPLY_UV( gU, gV, myTime, myIter, myThid )
        CALL TIMER_STOP ('SHAP_FILT_UV        [FORWARD_STEP]',myThid)
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT .AND. zonal_filt_uvStar) THEN
        CALL TIMER_START('ZONAL_FILT_UV       [FORWARD_STEP]',myThid)
        CALL ZONAL_FILT_APPLY_UV( gU, gV, myThid )
        CALL TIMER_STOP ('ZONAL_FILT_UV       [FORWARD_STEP]',myThid)
      ENDIF
#endif

C--   Solve elliptic equation(s).
C     Two-dimensional only for conventional hydrostatic or
C     three-dimensional for non-hydrostatic and/or IGW scheme.
      IF ( momStepping ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE hFacS, hFacW = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif
        CALL TIMER_START('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
        CALL SOLVE_FOR_PRESSURE( myTime, myIter, myThid )
        CALL TIMER_STOP ('SOLVE_FOR_PRESSURE  [FORWARD_STEP]',myThid)
      ENDIF

C--   Correct divergence in flow field and cycle time-stepping momentum
#ifdef ALLOW_MOM_STEPPING
#ifndef ALLOW_AUTODIFF
      IF ( momStepping ) THEN
#endif
        CALL TIMER_START('MOM_CORRECTION_STEP [FORWARD_STEP]',myThid)
        CALL MOMENTUM_CORRECTION_STEP( myTime, myIter, myThid )
        CALL TIMER_STOP ('MOM_CORRECTION_STEP [FORWARD_STEP]',myThid)
#ifndef ALLOW_AUTODIFF
      ENDIF
#endif
#endif /* ALLOW_MOM_STEPPING */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uVel, vVel = comlev1, key = ikey_dynamics, kind = isbyte
#endif

      IF ( calc_wVelocity ) THEN
C--     Integrate continuity vertically for vertical velocity
C       (+ update "etaN" & "etaH", exact volume conservation):
        CALL TIMER_START('INTEGR_CONTINUITY   [FORWARD_STEP]',myThid)
        CALL INTEGR_CONTINUITY( uVel, vVel, myTime, myIter, myThid)
        CALL TIMER_STOP ('INTEGR_CONTINUITY   [FORWARD_STEP]',myThid)
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
C--   These store directives avoid an (expensive) extra
C     "call solve_for_pressure" in forward_step_ad
CADJ STORE wVel = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE etaN = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
#  ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etaH
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE rStarFacC,rStarFacS,rStarFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
C--   r* : compute the future level thickness according to etaH(n+1)
        CALL TIMER_START('CALC_R_STAR         [FORWARD_STEP]',myThid)
        CALL CALC_R_STAR(etaH, myTime, myIter, myThid )
        CALL TIMER_STOP ('CALC_R_STAR         [FORWARD_STEP]',myThid)
# endif /* DISABLE_RSTAR_CODE */
      ELSEIF ( nonlinFreeSurf.GT.0 .AND. selectSigmaCoord.EQ.0 ) THEN
C--   compute the future surface level thickness according to etaH(n+1)
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE etaH          = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif
        CALL TIMER_START('CALC_SURF_DR      [FORWARD_STEP]',myThid)
        CALL CALC_SURF_DR(etaH, myTime, myIter, myThid )
        CALL TIMER_STOP ('CALC_SURF_DR      [FORWARD_STEP]',myThid)
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rStarExpC
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hFac_surfC       = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE salt,theta = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  ifdef ALLOW_PTRACERS
CADJ STORE pTracer    = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_PTRACERS */
# endif
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( staggerTimeStep ) THEN
C--   do exchanges of U,V (needed for multiDim) when using stagger time-step :
#ifdef ALLOW_DEBUG
        IF (debugMode)
     &   CALL DEBUG_CALL('DO_STAGGER_FIELDS_EXCH.',myThid)
#endif
        CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
        CALL DO_STAGGER_FIELDS_EXCHANGES( myTime, myIter, myThid )
        CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

#ifdef ALLOW_DIAGNOSTICS
C--   State-variables diagnostics
        IF ( useDiagnostics ) THEN
          CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
          CALL DO_STATEVARS_DIAGS( myTime, 1, myIter, myThid )
          CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C     these store directives prevent calling do_stagger_fields_exchanges
C     again in the AD-part of forward_step_ad
CADJ STORE salt, theta = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE wVel        = comlev1, key = ikey_dynamics, kind = isbyte
#endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('THERMODYNAMICS      [FORWARD_STEP]',myThid)
        CALL THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('THERMODYNAMICS      [FORWARD_STEP]',myThid)

C--    if staggerTimeStep: end
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
C     this may not be necessary, but it does not do any harm (i.e. it is
C     ignored by TAF if not needed)
CADJ STORE salt, theta = comlev1, key = ikey_dynamics, kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C---+--------+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
cph This is needed because convective_adjustment calls
cph find_rho which may use pressure()
CADJ STORE totPhiHyd  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif
C--   Apply adjustments to Tracers arrays (T,S,+pTracers)
      CALL TIMER_START('TRC_CORRECTION_STEP [FORWARD_STEP]',myThid)
      CALL TRACERS_CORRECTION_STEP(myTime, myIter, myThid)
      CALL TIMER_STOP ('TRC_CORRECTION_STEP [FORWARD_STEP]',myThid)

#ifdef ALLOW_LONGSTEP
      IF ( usePTRACERS ) THEN
       IF ( LS_whenToSample .EQ. 2 ) THEN
C       Average everything at the end of the timestep.  This will
C       reproduce online results with staggerTimeStep=.TRUE.
C       when LS_nIter=1
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('LONGSTEP_AVERAGE',myThid)
#endif
        CALL TIMER_START('LONGSTEP_AVERAGE    [FORWARD_STEP]',myThid)
C       myIter has been update after dynamics, but the averaging window
C       should be determined by myIter at beginning of timestep
        CALL LONGSTEP_AVERAGE( myTimeBeg, myIterBeg, myThid )
        CALL TIMER_STOP ('LONGSTEP_AVERAGE    [FORWARD_STEP]',myThid)

#ifdef ALLOW_DEBUG
        IF (debugMode)
     &    CALL DEBUG_CALL('LONGSTEP_THERMODYNAMICS',myThid)
#endif
        CALL TIMER_START('LONGSTEP_THERMODYNAMICS      [FORWARD_STEP]',
     &                   myThid)
        CALL LONGSTEP_THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('LONGSTEP_THERMODYNAMICS      [FORWARD_STEP]',
     &                   myThid)
C--    if LS_whenToSample.EQ.2: end
       ENDIF

C--   Apply adjustments to passive Tracers arrays (pTracers)
c      CALL TIMER_START('LS_CORRECTION_STEP  [FORWARD_STEP]',myThid)
c      CALL LONGSTEP_CORRECTION_STEP(myTime, myIter, myThid)
c      CALL TIMER_STOP ('LS_CORRECTION_STEP  [FORWARD_STEP]',myThid)
C--    if usePTRACERS: end
      ENDIF
#endif /* ALLOW_LONGSTEP */

#ifdef ALLOW_GCHEM
C     Add separate timestepping of chemical/biological/forcing
C     of ptracers here in GCHEM_FORCING_SEP
#ifdef ALLOW_LONGSTEP
      IF ( useGCHEM .AND. LS_doTimeStep ) THEN
#else
      IF ( useGCHEM ) THEN
#endif
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta, salt = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_PTRACERS
CADJ STORE pTracer     = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_PTRACERS */
# endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GCHEM_FORCING_SEP',myThid)
#endif /* ALLOW_DEBUG */
        CALL TIMER_START('GCHEM_FORCING_SEP  [FORWARD_STEP]',myThid)
        CALL GCHEM_FORCING_SEP( myTime,myIter,myThid )
        CALL TIMER_STOP ('GCHEM_FORCING_SEP  [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_GCHEM */

C--   Do "blocking" sends and receives for tendency "overlap" terms
c     CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
c     CALL DO_GTERM_BLOCKING_EXCHANGES( myThid )
c     CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)

C--   Do "blocking" sends and receives for field "overlap" terms
      CALL TIMER_START('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
      CALL DO_FIELDS_BLOCKING_EXCHANGES( myThid )
      CALL TIMER_STOP ('BLOCKING_EXCHANGES  [FORWARD_STEP]',myThid)
#ifdef ALLOW_AUTODIFF_TAMC
C     These store directives avoid extra subroutine calls, for example,
C     thermodynamics, tracers_correction_step, gchem_forcing_sep, and
C     do_fields_blocking_exchages
CADJ STORE theta, salt = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE uvel,  vvel = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE totPhiHyd   = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ALLOW_PTRACERS
CADJ STORE pTracer     = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL TIMER_START('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
       CALL DO_STATEVARS_DIAGS( myTime, 2, myIter, myThid )
       CALL TIMER_STOP ('DO_STATEVARS_DIAGS  [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_GRIDALT
      IF (useGRIDALT) THEN
         CALL GRIDALT_UPDATE(myThid)
      ENDIF
#endif

#ifdef ALLOW_FIZHI
      IF (useFIZHI) THEN
         CALL TIMER_START('FIZHI               [FORWARD_STEP]',myThid)
         CALL STEP_FIZHI_CORR ( myTime, myIter, myThid, dTtracerLev(1) )
         CALL TIMER_STOP ('FIZHI               [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_FLT
C--   Calculate float trajectories
      IF (useFLT) THEN
        CALL TIMER_START('FLOATS            [FORWARD_STEP]',myThid)
        CALL FLT_MAIN( myTime, myIter, myThid )
        CALL TIMER_STOP ('FLOATS            [FORWARD_STEP]',myThid)
      ENDIF
#endif

#ifdef ALLOW_TIMEAVE
C--   State-variables time-averaging
      CALL TIMER_START('DO_STATEVARS_TAVE   [FORWARD_STEP]',myThid)
      CALL DO_STATEVARS_TAVE( myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_STATEVARS_TAVE   [FORWARD_STEP]',myThid)
#endif

#ifdef ALLOW_NEST_PARENT
      IF ( useNEST_PARENT) THEN
        CALL NEST_PARENT_IO_2( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_NEST_PARENT */

#ifdef ALLOW_NEST_CHILD
      IF ( useNEST_CHILD) THEN
            CALL NEST_CHILD_TRANSP( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_NEST_CHILD */

#ifdef ALLOW_MONITOR
      IF ( monitorFreq.GT.0. .OR. adjMonitorFreq.GT.0. ) THEN
C--   Check status of solution (statistics, cfl, etc...)
        CALL TIMER_START('MONITOR             [FORWARD_STEP]',myThid)
        CALL MONITOR( myTime, myIter, myThid )
        CALL TIMER_STOP ('MONITOR             [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_MONITOR */

#ifdef ALLOW_COST
C--     compare model with data and compute cost function
C--     this is done after exchanges to allow interpolation
      CALL TIMER_START('COST_TILE           [FORWARD_STEP]',myThid)
      CALL COST_TILE  ( myTime, myIter, myThid )
      CALL TIMER_STOP ('COST_TILE           [FORWARD_STEP]',myThid)
#endif

#ifdef ALLOW_ECCO
C--   Diagnose variables for pkg/ecco averaging and cost function purposes
      IF ( useECCO ) CALL ECCO_PHYS( myTime, myIter, myThid )
#endif

C--   Check if it has reached the end of simulation
      modelEnd = myTime.EQ.endTime .OR. myIter.EQ.nEndIter
#ifdef HAVE_SIGREG
      IF ( useSIGREG ) THEN
        modelEnd = modelEnd .OR. ( i_got_signal.GT.0 )
      ENDIF
#endif /* HAVE_SIGREG */

C--   Do IO if needed.
      CALL TIMER_START('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)
      CALL DO_THE_MODEL_IO( modelEnd, myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_THE_MODEL_IO     [FORWARD_STEP]',myThid)

#ifdef ALLOW_PTRACERS
C     Reset the ptracers (but after the io is done)
      IF ( usePTRACERS ) THEN
        CALL TIMER_START('PTRACERS_RESET      [FORWARD_STEP]',myThid)
        CALL PTRACERS_RESET( myTime, myIter, myThid )
        CALL TIMER_STOP ('PTRACERS_RESET      [FORWARD_STEP]',myThid)
      ENDIF
#endif /* ALLOW_PTRACERS */

C--   Save state for restarts
      CALL TIMER_START('DO_WRITE_PICKUP     [FORWARD_STEP]',myThid)
      CALL DO_WRITE_PICKUP( modelEnd, myTime, myIter, myThid )
      CALL TIMER_STOP ('DO_WRITE_PICKUP     [FORWARD_STEP]',myThid)

#ifdef HAVE_SIGREG
      IF ( useSIGREG ) THEN
        IF ( modelEnd .AND. i_got_signal.GT.0 ) THEN
          STOP 'Checkpoint completed -- killed by signal handler'
        ENDIF
      ENDIF
#endif /* HAVE_SIGREG */

#ifdef ALLOW_AUTODIFF
      CALL AUTODIFF_INADMODE_SET( myTime, myIter, myThid )
#endif

#ifdef ALLOW_SHOWFLOPS
      CALL TIMER_START('SHOWFLOPS_INLOOP   [THE_MAIN_LOOP]', myThid)
      CALL SHOWFLOPS_INLOOP( iloop, myThid )
      CALL TIMER_STOP ('SHOWFLOPS_INLOOP   [THE_MAIN_LOOP]', myThid)
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('FORWARD_STEP',myThid)
#endif

      RETURN
      END
