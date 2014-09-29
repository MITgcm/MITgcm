C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_ad_check_lev2_dir.h,v 1.5 2014/09/29 16:45:45 gforget Exp $
C $Name:  $
c
c     store directives for checkpoint level 2
c

#ifdef ALLOW_ECCO_EVOLUTION
#ifdef ALLOW_COST_ATLANTIC
CADJ STORE theta = tapelev2, key=ilev_2
CADJ STORE vVel  = tapelev2, key=ilev_2
# ifdef NONLIN_FRSURF
CADJ STORE hFacS  = tapelev2, key=ilev_2
# endif
#endif
#endif /* ALLOW_ECCO_EVOLUTION */

#ifdef ALLOW_ECCO_BARSTORES

#ifdef ALLOW_ECCO

CADJ STORE tbar  = tapelev2, key=ilev_2
cccCADJ STORE tbar_daily  = tapelev2, key=ilev_2
CADJ STORE sbar  = tapelev2, key=ilev_2
cccCADJ STORE sbar_daily  = tapelev2, key=ilev_2
CADJ STORE ubar  = tapelev2, key=ilev_2
CADJ STORE vbar  = tapelev2, key=ilev_2
CADJ STORE wbar  = tapelev2, key=ilev_2
CADJ STORE psbar  = tapelev2, key=ilev_2
CADJ STORE bpbar  = tapelev2, key=ilev_2
CADJ STORE sstbar  = tapelev2, key=ilev_2

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
CADJ STORE gencost_barfld  = tapelev2, key=ilev_2
CADJ STORE gencost_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
CADJ STORE profiles_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_PROFILES */

#ifdef ALLOW_SEAICE
CADJ STORE smrareabar  = tapelev2, key=ilev_2
CADJ STORE smrsstbar  = tapelev2, key=ilev_2
CADJ STORE smrsssbar  = tapelev2, key=ilev_2

CADJ STORE xx_smrareabar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsstbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsssbar_mean_dummy  = dummytape, key=1, kind=isbyte
#endif /* ALLOW_SEAICE */

#endif /* ALLOW_ECCO_BARSTORES */

