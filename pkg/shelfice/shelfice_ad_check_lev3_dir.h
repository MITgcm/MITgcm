C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev3_dir.h,v 1.3 2014/01/17 21:56:30 jmc Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE shelficeForcingT = tapelev3, key = ilev_3
CADJ STORE shelficeForcingS = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforT     = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforS     = tapelev3, key = ilev_3
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = tapelev3, key = ilev_3
CADJ STORE xx_shifwflx1     = tapelev3, key = ilev_3
# endif
#endif /* ALLOW_SHELFICE */

