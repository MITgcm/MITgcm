#include "ECCO_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_cost_driver(  myTime, myIter, myThid  )

c     ==================================================================
c     SUBROUTINE ecco_cost_driver
c     ==================================================================
c
c     o Compute all ecco cost function contributions.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_driver
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_SSH_COST_CONTRIBUTION
#  ifdef ALLOW_PROFILES
#   include "PROFILES_SIZE.h"
#   include "profiles.h"
#  endif
# endif
#endif

c     == routine arguments ==
      integer myThid
      integer myIter
      _RL     myTime

#ifdef ECCO_CTRL_DEPRECATED

c--   Compute the cost function contribution of the boundary forcing,
c--   i.e. heat flux, salt flux, zonal and meridional wind stress.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_forcing',myThid)
#endif
      call timer_start('cost_forcing       [ECCO SPIN-DOWN]', myThid)
      call cost_forcing( myThid )
      call timer_stop ('cost_forcing       [ECCO SPIN-DOWN]', myThid)
cph(
c--   Compute cost function contribution of wind stress observations.
c#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
c      call cost_mean_heatflux( myIter, myTime, myThid )
c# ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE objf_hfluxmm = tapelev_init, key=1
c# endif
c#endif

c--   Compute cost function contribution of wind stress observations.
c#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
c      call cost_mean_saltflux( myIter, myTime, myThid )
c# ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE objf_sfluxmm = tapelev_init, key=1
c# endif
c#endif
cph)

c--   Compute cost function contribution of SSH.
#ifdef ALLOW_SSH_COST_CONTRIBUTION
      IF (using_cost_altim) THEN
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_ssh',myThid)
# endif
c# if (defined(ALLOW_NEW_SSH_COST))
c      call timer_start('cost_ssh_new       [ECCO SPIN-DOWN]', myThid)
c      call cost_ssh_new( myIter, myTime, myThid )
c      call timer_stop ('cost_ssh_new       [ECCO SPIN-DOWN]', myThid)
c# elif (!defined (ALLOW_GENCOST_SSHV4))
      call timer_start('cost_ssh           [ECCO SPIN-DOWN]', myThid)
      call cost_ssh( myIter, myTime, myThid )
      call timer_stop ('cost_ssh           [ECCO SPIN-DOWN]', myThid)
c# endif
      ENDIF !IF (using_cost_altim) THEN
# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef ALLOW_PROFILES
CADJ STORE prof_etan_mean = tapelev_init, key=1
#  endif
# endif
#endif

#ifdef ALLOW_BP_COST_CONTRIBUTION
c--   Compute bottom pressure cost
      IF (using_cost_bp) THEN
      call timer_start('cost_bp    [ECCO SPIN-DOWN]', myThid)
      call cost_generic(
     &     1, bpfile, xx_bpbar_mean_dummy,
     &     1, bpdatfile, bperrfile, mult_bp,
     &     nmonsrec, nmonsrec, bpstartdate, 0. _d 0,
     &     'C', -1. _d 2, 1. _d 2, 0. _d 0,
     &     anom_preproc, no_preproc_c, no_preproc_i, no_preproc_r,
     &     no_posproc, no_posproc_c, no_posproc_i, no_posproc_r,
     &     0, bpdatfile, objf_bp, num_bp,
     &     myIter, myTime, myThid )
      call timer_stop ('cost_bp    [ECCO SPIN-DOWN]', myThid)
      no_preproc(1)=' '
      ENDIF
#endif

c--   Compute cost function contribution of Temperature and Salinity.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_hyd',myThid)
#endif
      call timer_start('cost_hyd           [ECCO SPIN-DOWN]', myThid)
      call cost_hyd( myIter, myTime, myThid )
      call timer_stop ('cost_hyd           [ECCO SPIN-DOWN]', myThid)

#ifdef ALLOW_SEAICE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('seaice_cost_driver',myThid)
#endif
      IF ( useSeaice) THEN
      call timer_start('seaice_cost_driver [ECCO SPIN-DOWN]', myThid)
      call seaice_cost_driver( myIter, myTime, myThid )
      call timer_stop ('seaice_cost_driver [ECCO SPIN-DOWN]', myThid)
      ENDIF
#endif

c#ifdef ALLOW_CURMTR_COST_CONTRIBUTION
c#ifdef ALLOW_DEBUG
c      IF (debugMode) CALL DEBUG_CALL('cost_curmtr',myThid)
c#endif
c      call timer_start('cost_curmtr        [ECCO SPIN-DOWN]', myThid)
c      call cost_curmtr( myIter, myTime, myThid )
c      call timer_stop ('cost_curmtr        [ECCO SPIN-DOWN]', myThid)
c#endif

c--   Compute cost function contribution of drifter velocities.
c#ifdef ALLOW_DRIFTER_COST_CONTRIBUTION
c#ifndef ALLOW_AUTODIFF_WHTAPEIO
c#ifdef ALLOW_DEBUG
c      IF (debugMode) CALL DEBUG_CALL('cost_drifter',myThid)
c#endif
c      call timer_start('cost_drifter         [ECCO SPIN-DOWN]', myThid)
c      call cost_drifter( myIter, myTime, myThid )
c      call timer_stop ('cost_drifter         [ECCO SPIN-DOWN]', myThid)
c#endif
c#endif

c--   Compute cost function contribution of wind stress observations.
c#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
c     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
c#ifdef ALLOW_DEBUG
c      IF (debugMode) CALL DEBUG_CALL('cost_scat',myThid)
c#endif
c      IF (using_cost_scat) THEN
c      call timer_start('cost_scat            [ECCO SPIN-DOWN]', myThid)
c      call cost_scat( myIter, myTime, myThid )
c      call timer_stop ('cost_scat            [ECCO SPIN-DOWN]', myThid)
c      ENDIF
c#endif

c--   Compute cost function contribution of drift between the first
c     and the last year.
c#ifdef ALLOW_DRIFT_COST_CONTRIBUTION
c#ifdef ALLOW_DEBUG
c      IF (debugMode) CALL DEBUG_CALL('cost_drift',myThid)
c#endif
c      call timer_start('cost_drift         [ECCO SPIN-DOWN]', myThid)
c      call cost_drift( myIter, myTime, myThid )
c      call timer_stop ('cost_drift         [ECCO SPIN-DOWN]', myThid)
c#endif
c#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
c#ifdef ALLOW_DEBUG
c      IF (debugMode) CALL DEBUG_CALL('cost_driftw',myThid)
c#endif
c      call timer_start('cost_driftw        [ECCO SPIN-DOWN]', myThid)
c      call cost_driftw( myIter, myTime, myThid )
c      call timer_stop ('cost_driftw        [ECCO SPIN-DOWN]', myThid)
c#endif
c      _BARRIER

c--   Compute initial vs. final T/S deviation
c#ifdef ALLOW_COST_INI_FIN
c      call timer_start('cost_ini_fin       [ECCO SPIN-DOWN]', myThid)
c      call cost_theta_ini_fin( myIter, myTime, myThid )
c      call cost_salt_ini_fin( myIter, myTime, myThid )
c      call timer_stop ('cost_ini_fin       [ECCO SPIN-DOWN]', myThid)
c#endif
c      _BARRIER

c--   Internal Parameter controls cost terms:
      call timer_start('cost_internal_params  [ECCO SPIN-DOWN]', myThid)
      call cost_internal_params( myIter, myTime, myThid )
      call timer_stop ('cost_internal_params  [ECCO SPIN-DOWN]', myThid)
      _BARRIER

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_OBCS_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_obcs',myThid)
#endif
      call timer_start('cost_obcs          [ECCO SPIN-DOWN]', myThid)
      call cost_obcs( myIter, myTime, myThid )
      call timer_stop ('cost_obcs          [ECCO SPIN-DOWN]', myThid)
#endif

c--   Compute user defined cost function contributions (elaborate)
      call timer_start('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)
      call cost_gencost_all( myIter, myTime, myThid )
      call timer_stop ('cost_gencost_all    [ECCO SPIN-DOWN]', myThid)

#ifdef ALLOW_CTRL
      IF (useCTRL) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('ctrl_cost_driver',myThid)
#endif
      call timer_start('ctrl_cost_driver [ECCO SPIN-DOWN]', myThid)
      call ctrl_cost_driver( myThid )
      call timer_stop ('ctrl_cost_driver [ECCO SPIN-DOWN]', myThid)
      ENDIF
#endif

      RETURN
      END
