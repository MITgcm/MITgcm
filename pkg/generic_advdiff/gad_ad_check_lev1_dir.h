C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_ad_check_lev1_dir.h,v 1.1 2012/07/02 22:58:07 heimbach Exp $
C $Name:  $

#ifdef GAD_ALLOW_TS_SOM_ADV
CADJ STORE som_S      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE som_T      = comlev1, key = ikey_dynamics, kind = isbyte
#endif /* GAD_ALLOW_TS_SOM_ADV */
