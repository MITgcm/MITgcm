C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev2_dir.h,v 1.3 2014/01/17 21:56:30 jmc Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE shelficeForcingT = tapelev2, key = ilev_2
CADJ STORE shelficeForcingS = tapelev2, key = ilev_2
CADJ STORE cMeanSHIforT     = tapelev2, key = ilev_2
CADJ STORE cMeanSHIforS     = tapelev2, key = ilev_2
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = tapelev2, key = ilev_2
CADJ STORE xx_shifwflx1     = tapelev2, key = ilev_2
# endif
#endif /* ALLOW_SHELFICE */
