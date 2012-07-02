C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_ad_check_lev2_dir.h,v 1.1 2012/07/02 22:58:07 heimbach Exp $
C $Name:  $

#ifdef GAD_ALLOW_TS_SOM_ADV
CADJ STORE som_S = tapelev2, key = ilev_2
CADJ STORE som_T = tapelev2, key = ilev_2
#endif /* GAD_ALLOW_TS_SOM_ADV */
