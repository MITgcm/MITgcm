C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_ad_check_lev3_dir.h,v 1.5 2014/09/29 16:45:45 gforget Exp $
C $Name:  $
c
c     store directives for checkpoint level 3
c

#ifdef ALLOW_ECCO_EVOLUTION
#ifdef ALLOW_COST_ATLANTIC
CADJ STORE theta = tapelev3, key=ilev_3
CADJ STORE vVel  = tapelev3, key=ilev_3
# ifdef NONLIN_FRSURF
CADJ STORE hFacS  = tapelev3, key=ilev_3
# endif
#endif
#endif /* ALLOW_ECCO_EVOLUTION */

#ifdef ALLOW_ECCO_BARSTORES

#ifdef ALLOW_ECCO

CADJ STORE tbar  = tapelev3, key=ilev_3
cccCADJ STORE tbar_daily  = tapelev3, key=ilev_3
CADJ STORE sbar  = tapelev3, key=ilev_3
cccCADJ STORE sbar_daily  = tapelev3, key=ilev_3
CADJ STORE ubar  = tapelev3, key=ilev_3
CADJ STORE vbar  = tapelev3, key=ilev_3
CADJ STORE wbar  = tapelev3, key=ilev_3
CADJ STORE psbar  = tapelev3, key=ilev_3
CADJ STORE bpbar  = tapelev3, key=ilev_3
CADJ STORE sstbar  = tapelev3, key=ilev_3

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
CADJ STORE gencost_barfld  = tapelev3, key=ilev_3
CADJ STORE gencost_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
CADJ STORE profiles_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_PROFILES */

#ifdef ALLOW_SEAICE
CADJ STORE smrareabar  = tapelev3, key=ilev_3
CADJ STORE smrsstbar  = tapelev3, key=ilev_3
CADJ STORE smrsssbar  = tapelev3, key=ilev_3

CADJ STORE xx_smrareabar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsstbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsssbar_mean_dummy  = dummytape, key=1, kind=isbyte
#endif /* ALLOW_SEAICE */

#endif /* ALLOW_ECCO_BARSTORES */

