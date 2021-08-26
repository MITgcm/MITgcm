c
c     store directives for checkpoint level 4
c

#ifdef ALLOW_COST_ATLANTIC
CADJ STORE theta = tapelev4, key=ilev_4
CADJ STORE vVel  = tapelev4, key=ilev_4
# ifdef NONLIN_FRSURF
CADJ STORE hFacS  = tapelev4, key=ilev_4
# endif
#endif

#ifdef ALLOW_ECCO_BARSTORES

#ifdef ALLOW_ECCO

CADJ STORE tbar  = tapelev4, key=ilev_4
cccCADJ STORE tbar_daily  = tapelev4, key=ilev_4
CADJ STORE sbar  = tapelev4, key=ilev_4
cccCADJ STORE sbar_daily  = tapelev4, key=ilev_4
CADJ STORE ubar  = tapelev4, key=ilev_4
CADJ STORE vbar  = tapelev4, key=ilev_4
CADJ STORE wbar  = tapelev4, key=ilev_4
CADJ STORE psbar  = tapelev4, key=ilev_4
CADJ STORE bpbar  = tapelev4, key=ilev_4
CADJ STORE sstbar  = tapelev4, key=ilev_4

CADJ STORE xx_tbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_sbar_mean_dummy  = dummytape, key=1, kind=isbyte
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
CADJ STORE xx_sigmaRbar_mean_dummy  = dummytape, key=1, kind=isbyte
#endif
CADJ STORE xx_ubar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_vbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_wbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_psbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_bpbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_sstbar_mean_dummy  = dummytape, key=1, kind=isbyte
cccCADJ STORE xx_sssbar_mean_dummy  = dummytape, key=1, kind=isbyte

#ifdef ALLOW_GENCOST_CONTRIBUTION
CADJ STORE gencost_barfld  = tapelev4, key=ilev_4
CADJ STORE gencost_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
CADJ STORE profiles_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_PROFILES */

#ifdef ALLOW_SEAICE
CADJ STORE smrareabar  = tapelev4, key=ilev_4
CADJ STORE smrsstbar  = tapelev4, key=ilev_4
CADJ STORE smrsssbar  = tapelev4, key=ilev_4

CADJ STORE xx_smrareabar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsstbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsssbar_mean_dummy  = dummytape, key=1, kind=isbyte
#endif /* ALLOW_SEAICE */

#endif /* ALLOW_ECCO_BARSTORES */

