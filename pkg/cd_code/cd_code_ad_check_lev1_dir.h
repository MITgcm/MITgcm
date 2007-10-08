C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_ad_check_lev1_dir.h,v 1.2 2007/10/08 23:56:38 jmc Exp $
C $Name:  $

#ifdef ALLOW_CD_CODE
CADJ STORE uveld      = comlev1, key = ikey_dynamics
CADJ STORE vveld     = comlev1, key = ikey_dynamics
CADJ STORE unm1      = comlev1, key = ikey_dynamics
CADJ STORE vnm1      = comlev1, key = ikey_dynamics
#endif /* ALLOW_CD_CODE */
