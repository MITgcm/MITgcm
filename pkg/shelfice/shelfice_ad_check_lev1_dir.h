C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev1_dir.h,v 1.2 2011/05/10 07:49:19 mlosch Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE cMeanSHIforT   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforS   = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_shifwflx1   = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_SHELFICE */
