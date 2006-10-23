c
c     store directives for cost_averagesfields
c     xx_..._mean_dummy
c
c     created: heimbach@mit.edu 4-Mar-2003
c
CADJ STORE xx_psbar_mean_dummy = onetape
CADJ STORE xx_tbar_mean_dummy  = onetape
CADJ STORE xx_sbar_mean_dummy  = onetape
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
#ifdef ALLOW_PROFILES
CADJ STORE profiles_dummy = tapelev_init, key = 1
#endif

