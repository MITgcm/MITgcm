C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_cost_driver.F,v 1.7 2012/09/30 20:33:55 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine ecco_cost_driver(  myTime, myIter, mythid  )

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
#include "ecco_cost.h"

c     == routine arguments ==
      integer mythid
      integer myiter
      _RL     mytime

#ifdef ALLOW_ECCO_EVOLUTION

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_SSH_COST_CONTRIBUTION
#  ifdef ALLOW_PROFILES
#   include "profiles.h"
#  endif
# endif
#endif

c--   Compute the cost function contribution of the boundary forcing,
c--   i.e. heat flux, salt flux, zonal and meridional wind stress.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_forcing',myThid)
#endif
      call timer_start('cost_forcing       [ECCO SPIN-DOWN]', mythid)
      call cost_forcing( myiter, mytime, mythid )
      call timer_stop ('cost_forcing       [ECCO SPIN-DOWN]', mythid)
cph(
c--   Compute cost function contribution of wind stress observations.
#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
      call cost_mean_heatflux( myiter, mytime, mythid )
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE objf_hfluxmm = tapelev_init, key=1
# endif
#endif

c--   Compute cost function contribution of wind stress observations.
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
      call cost_mean_saltflux( myiter, mytime, mythid )
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE objf_sfluxmm = tapelev_init, key=1
# endif
#endif
cph)

c--   Compute cost function contribution of SSH.
#ifdef ALLOW_SSH_COST_CONTRIBUTION
      IF (using_cost_altim) THEN
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_ssh',myThid)
# endif
# if (defined(ALLOW_SSHV4_COST))
      call timer_start('cost_sshv4         [ECCO SPIN-DOWN]', mythid)
      call cost_sshv4( myiter, mytime, mythid )
      call timer_stop ('cost_sshv4         [ECCO SPIN-DOWN]', mythid)
# elif (defined(ALLOW_NEW_SSH_COST))
      call timer_start('cost_ssh_new       [ECCO SPIN-DOWN]', mythid)
      call cost_ssh_new( myiter, mytime, mythid )
      call timer_stop ('cost_ssh_new       [ECCO SPIN-DOWN]', mythid)
# elif (!defined (ALLOW_GENCOST_SSHV4))
      call timer_start('cost_ssh           [ECCO SPIN-DOWN]', mythid)
      call cost_ssh( myiter, mytime, mythid )
      call timer_stop ('cost_ssh           [ECCO SPIN-DOWN]', mythid)
# endif
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
      call timer_start('cost_bp    [ECCO SPIN-DOWN]', mythid)
      call cost_bp( myiter, mytime, mythid )
      call timer_stop ('cost_bp    [ECCO SPIN-DOWN]', mythid)
      ENDIF
#endif

c--   Compute cost function contribution of Temperature and Salinity.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_hyd',myThid)
#endif
      call timer_start('cost_hyd           [ECCO SPIN-DOWN]', mythid)
      call cost_hyd( myiter, mytime, mythid )
      call timer_stop ('cost_hyd           [ECCO SPIN-DOWN]', mythid)

#ifdef ALLOW_SEAICE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('seaice_cost_driver',myThid)
#endif
      IF ( useSeaice) THEN
      call timer_start('seaice_cost_driver [ECCO SPIN-DOWN]', mythid)
      call seaice_cost_driver( myiter, mytime, mythid )
      call timer_stop ('seaice_cost_driver [ECCO SPIN-DOWN]', mythid)
      ENDIF
#endif

#ifdef ALLOW_OBCS_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_obcs',myThid)
#endif
      call timer_start('cost_obcs          [ECCO SPIN-DOWN]', mythid)
      call cost_obcs( myiter, mytime, mythid )
      call timer_stop ('cost_obcs          [ECCO SPIN-DOWN]', mythid)
#endif

#ifdef ALLOW_CURMTR_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_curmtr',myThid)
#endif
      call timer_start('cost_curmtr        [ECCO SPIN-DOWN]', mythid)
      call cost_curmtr( myiter, mytime, mythid )
      call timer_stop ('cost_curmtr        [ECCO SPIN-DOWN]', mythid)
#endif

c--   Compute cost function contribution of drifter velocities.
#ifdef ALLOW_DRIFTER_COST_CONTRIBUTION
#ifndef ALLOW_AUTODIFF_WHTAPEIO
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_drifter',myThid)
#endif
      call timer_start('cost_drifter         [ECCO SPIN-DOWN]', mythid)
      call cost_drifter( myiter, mytime, mythid )
      call timer_stop ('cost_drifter         [ECCO SPIN-DOWN]', mythid)
#endif
#endif

c--   Compute cost function contribution of wind stress observations.
#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_scat',myThid)
#endif
      IF (using_cost_scat) THEN
      call timer_start('cost_scat            [ECCO SPIN-DOWN]', mythid)
      call cost_scat( myiter, mytime, mythid )
      call timer_stop ('cost_scat            [ECCO SPIN-DOWN]', mythid)
      ENDIF
#endif

c--   Compute cost function contribution of drift between the first
c     and the last year.
#ifdef ALLOW_DRIFT_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_drift',myThid)
#endif
      call timer_start('cost_drift         [ECCO SPIN-DOWN]', mythid)
      call cost_drift( myiter, mytime, mythid )
      call timer_stop ('cost_drift         [ECCO SPIN-DOWN]', mythid)
#endif
#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('cost_driftw',myThid)
#endif
      call timer_start('cost_driftw        [ECCO SPIN-DOWN]', mythid)
      call cost_driftw( myiter, mytime, mythid )
      call timer_stop ('cost_driftw        [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   Compute initial vs. final T/S deviation
#ifdef ALLOW_COST_INI_FIN
      call timer_start('cost_ini_fin       [ECCO SPIN-DOWN]', mythid)
      call cost_theta_ini_fin( myiter, mytime, mythid )
      call cost_salt_ini_fin( myiter, mytime, mythid )
      call timer_stop ('cost_ini_fin       [ECCO SPIN-DOWN]', mythid)
#endif
      _BARRIER

c--   Internal Parameter controls cost terms:
      call timer_start('cost_internal_params  [ECCO SPIN-DOWN]', mythid)
      call cost_internal_params( myiter, mytime, mythid )
      call timer_stop ('cost_internal_params  [ECCO SPIN-DOWN]', mythid)
      _BARRIER

c--   Compute user defined cost function contributions (elaborate)
      call timer_start('cost_gencost_all    [ECCO SPIN-DOWN]', mythid)
      call cost_gencost_all( myiter, mytime, mythid )
      call timer_stop ('cost_gencost_all    [ECCO SPIN-DOWN]', mythid)

c--   Compute user defined cost function contributions (basic)
      call timer_start('cost_usercost_all    [ECCO SPIN-DOWN]', mythid)
      call cost_usercost_all( myiter, mytime, mythid )
      call timer_stop ('cost_usercost_all    [ECCO SPIN-DOWN]', mythid)

#endif /* ALLOW_ECCO_EVOLUTION */

      return
      end
