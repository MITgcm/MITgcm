C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev4_dir.h,v 1.2 2011/05/10 07:49:19 mlosch Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE cMeanSHIforT   = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforS   = tapelev4, key = ilev_4
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0   = tapelev4, key = ilev_4
CADJ STORE xx_shifwflx1   = tapelev4, key = ilev_4
# endif
#endif /* ALLOW_SHELFICE */
