C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_ad_check_lev1_dir.h,v 1.5 2014/09/29 16:45:45 gforget Exp $
C $Name:  $
c
c     store directives for checkpoint level 1
c

#ifdef ALLOW_ECCO_BARSTORES

#ifdef ALLOW_ECCO

CADJ STORE tbar  = comlev1, key=ikey_dynamics, kind=isbyte
cccCADJ STORE tbar_daily  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sbar  = comlev1, key=ikey_dynamics, kind=isbyte
cccCADJ STORE sbar_daily  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE ubar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vbar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wbar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE psbar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE bpbar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sstbar  = comlev1, key=ikey_dynamics, kind=isbyte

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
CADJ STORE gencost_barfld  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE gencost_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#endif /* ALLOW_ECCO */

#ifdef ALLOW_PROFILES
CADJ STORE profiles_dummy = dummytape, key=1, kind=isbyte
#endif /* ALLOW_PROFILES */

#ifdef ALLOW_SEAICE
CADJ STORE smrareabar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE smrsstbar  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE smrsssbar  = comlev1, key=ikey_dynamics, kind=isbyte

CADJ STORE xx_smrareabar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsstbar_mean_dummy  = dummytape, key=1, kind=isbyte
CADJ STORE xx_smrsssbar_mean_dummy  = dummytape, key=1, kind=isbyte
#endif /* ALLOW_SEAICE */

#endif /* ALLOW_ECCO_BARSTORES */

