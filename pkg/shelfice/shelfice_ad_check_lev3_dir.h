C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_ad_check_lev3_dir.h,v 1.2 2011/05/10 07:49:19 mlosch Exp $
C $Name:  $

#ifdef ALLOW_SHELFICE
CADJ STORE cMeanSHIforT   = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforS   = tapelev3, key = ilev_3
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0   = tapelev3, key = ilev_3
CADJ STORE xx_shifwflx1   = tapelev3, key = ilev_3
# endif
#endif /* ALLOW_SHELFICE */

