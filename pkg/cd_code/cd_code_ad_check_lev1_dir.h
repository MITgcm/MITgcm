C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_ad_check_lev1_dir.h,v 1.3 2009/02/13 21:58:35 heimbach Exp $
C $Name:  $

#ifdef ALLOW_CD_CODE
CADJ STORE uveld     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE vveld     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE unm1      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE vnm1      = comlev1, key = ikey_dynamics, kind = isbyte
#endif /* ALLOW_CD_CODE */
