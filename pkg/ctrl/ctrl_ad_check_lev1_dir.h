C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_ad_check_lev1_dir.h,v 1.3 2014/07/08 18:58:41 jmc Exp $
C $Name:  $

#ifdef ALLOW_GENTIM2D_CONTROL
CADJ STORE xx_gentim2d     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_gentim2d0    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_gentim2d1    = comlev1, key=ikey_dynamics, kind=isbyte
#endif
