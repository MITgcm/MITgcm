C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev4_dir.h,v 1.3 2014/01/17 21:56:30 jmc Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE shelficeForcingT = tapelev4, key = ilev_4
CADJ STORE shelficeForcingS = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforT     = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforS     = tapelev4, key = ilev_4
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = tapelev4, key = ilev_4
CADJ STORE xx_shifwflx1     = tapelev4, key = ilev_4
# endif
#endif /* ALLOW_SHELFICE */
