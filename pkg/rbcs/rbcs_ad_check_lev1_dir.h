C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_ad_check_lev1_dir.h,v 1.3 2007/10/09 00:09:25 jmc Exp $
C $Name:  $

CADJ STORE rbct0 = comlev1, key = ikey_dynamics
CADJ STORE rbct1 = comlev1, key = ikey_dynamics
CADJ STORE rbcs0 = comlev1, key = ikey_dynamics
CADJ STORE rbcs1 = comlev1, key = ikey_dynamics

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = comlev1, key = ikey_dynamics
CADJ STORE rbcptr1 = comlev1, key = ikey_dynamics
#endif
