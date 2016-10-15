C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_ad_check_lev3_dir.h,v 1.4 2016/10/15 21:30:43 mmazloff Exp $
C $Name:  $

#ifdef ALLOW_BLING

C common CARBON_NEEDS
CADJ STORE pH                = tapelev3, key = ilev_3
CADJ STORE fice              = tapelev3, key = ilev_3
CADJ STORE silica            = tapelev3, key = ilev_3

CADJ STORE irr_mem           = tapelev3, key = ilev_3
CADJ STORE phyto_sm          = tapelev3, key = ilev_3 
CADJ STORE phyto_lg          = tapelev3, key = ilev_3 
CADJ STORE phyto_diaz        = tapelev3, key = ilev_3
CADJ STORE chl               = tapelev3, key = ilev_3

CADJ STORE atmosp0        = tapelev3, key = ilev_3
CADJ STORE atmosp1        = tapelev3, key = ilev_3
CADJ STORE feinput0       = tapelev3, key = ilev_3
CADJ STORE feinput1       = tapelev3, key = ilev_3
CADJ STORE ice0           = tapelev3, key = ilev_3
CADJ STORE ice1           = tapelev3, key = ilev_3
CADJ STORE silica0        = tapelev3, key = ilev_3
CADJ STORE silica1        = tapelev3, key = ilev_3
CADJ STORE dicwind0       = tapelev3, key = ilev_3
CADJ STORE dicwind1       = tapelev3, key = ilev_3

# ifdef USE_EXFCO2
CADJ STORE apco20        = tapelev3, key = ilev_3
CADJ STORE apco21        = tapelev3, key = ilev_3
# endif

#ifdef ALLOW_BLING_COST
CADJ STORE totcost  = tapelev3, key = ilev_3
#endif

#endif /* ALLOW_BLING */

