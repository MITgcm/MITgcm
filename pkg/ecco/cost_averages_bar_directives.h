C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_averages_bar_directives.h,v 1.8 2014/08/15 09:27:03 atn Exp $
C $Name:  $

c
c     store directives for cost_averagesfields
c     xx_..._mean_dummy
c
c     created: heimbach@mit.edu 4-Mar-2003
c
CADJ STORE xx_psbar_mean_dummy = onetape
CADJ STORE xx_tbar_mean_dummy  = onetape
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
CADJ STORE xx_sigmaRbar_mean_dummy  = onetape
#endif
CADJ STORE xx_sbar_mean_dummy  = onetape
CADJ STORE xx_tbar_daily_mean_dummy  = onetape
CADJ STORE xx_sbar_daily_mean_dummy  = onetape
CADJ STORE xx_sstbar_mean_dummy  = onetape
CADJ STORE xx_ubar_mean_dummy  = onetape
CADJ STORE xx_vbar_mean_dummy  = onetape
CADJ STORE xx_wbar_mean_dummy  = onetape
CADJ STORE xx_taux_mean_dummy  = onetape
CADJ STORE xx_tauy_mean_dummy  = onetape
CADJ STORE xx_hflux_mean_dummy = onetape
CADJ STORE xx_sflux_mean_dummy = onetape
CADJ STORE xx_atemp_mean_dummy = onetape
CADJ STORE xx_aqh_mean_dummy = onetape
CADJ STORE xx_precip_mean_dummy = onetape
CADJ STORE xx_swflux_mean_dummy = onetape
CADJ STORE xx_swdown_mean_dummy = onetape
CADJ STORE xx_uwind_mean_dummy = onetape
CADJ STORE xx_vwind_mean_dummy = onetape
CADJ STORE xx_smrareabar_mean_dummy = onetape
CADJ STORE xx_smrsstbar_mean_dummy = onetape
CADJ STORE xx_smrsssbar_mean_dummy = onetape
CADJ STORE xx_iestaubar_mean_dummy = onetape
#ifdef ALLOW_PROFILES
CADJ STORE profiles_dummy = tapelev_init, key = 1
#endif

