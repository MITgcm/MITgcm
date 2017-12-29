C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_ad_check_lev1_dir.h,v 1.2 2017/12/29 19:38:34 jmc Exp $
C $Name:  $

#ifdef GCHEM_ADD2TR_TENDENCY
CADJ STORE gchemTendency   = comlev1, key = ikey_dynamics
#endif /* GCHEM_ADD2TR_TENDENCY */
