C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev1_dir.h,v 1.3 2014/01/17 21:56:30 jmc Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE shelficeForcingT = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeForcingS = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforT     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforS     = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_shifwflx1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_SHELFICE */
