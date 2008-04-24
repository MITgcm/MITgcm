C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_ad_check_lev1_dir.h,v 1.1 2008/04/24 21:30:15 gforget Exp $
C $Name:  $

#ifdef ALLOW_CFC
CADJ STORE AtmosCFC11   = comlev1, key = ikey_dynamics
CADJ STORE AtmosCFC12   = comlev1, key = ikey_dynamics
CADJ STORE Atmosp       = comlev1, key = ikey_dynamics
CADJ STORE pisvel       = comlev1, key = ikey_dynamics
CADJ STORE fice         = comlev1, key = ikey_dynamics
CADJ STORE wind0        = comlev1, key = ikey_dynamics
CADJ STORE ice0         = comlev1, key = ikey_dynamics
CADJ STORE atmosp0      = comlev1, key = ikey_dynamics
CADJ STORE wind1        = comlev1, key = ikey_dynamics
CADJ STORE ice1         = comlev1, key = ikey_dynamics
CADJ STORE atmosp1      = comlev1, key = ikey_dynamics
#endif /* ALLOW_CFC */
