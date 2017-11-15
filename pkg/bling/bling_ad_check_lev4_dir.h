C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_ad_check_lev4_dir.h,v 1.6 2017/11/15 18:17:24 mmazloff Exp $
C $Name:  $

#ifdef ALLOW_BLING

C common CARBON_NEEDS
CADJ STORE pH                = tapelev4, key = ilev_4
CADJ STORE fice              = tapelev4, key = ilev_4
CADJ STORE silica            = tapelev4, key = ilev_4

CADJ STORE irr_mem           = tapelev4, key = ilev_4
CADJ STORE phyto_sm          = tapelev4, key = ilev_4 
CADJ STORE phyto_lg          = tapelev4, key = ilev_4 
CADJ STORE phyto_diaz        = tapelev4, key = ilev_4
CADJ STORE chl               = tapelev4, key = ilev_4
CADJ STORE poc               = tapelev4, key = ilev_4

CADJ STORE atmosp0        = tapelev4, key = ilev_4
CADJ STORE atmosp1        = tapelev4, key = ilev_4
CADJ STORE feinput0       = tapelev4, key = ilev_4
CADJ STORE feinput1       = tapelev4, key = ilev_4
CADJ STORE ice0           = tapelev4, key = ilev_4
CADJ STORE ice1           = tapelev4, key = ilev_4
CADJ STORE silica0        = tapelev4, key = ilev_4
CADJ STORE silica1        = tapelev4, key = ilev_4
CADJ STORE dicwind0       = tapelev4, key = ilev_4
CADJ STORE dicwind1       = tapelev4, key = ilev_4

# ifdef USE_EXFCO2
CADJ STORE apco20        = tapelev4, key = ilev_4
CADJ STORE apco21        = tapelev4, key = ilev_4
# endif

#endif /* ALLOW_BLING */

