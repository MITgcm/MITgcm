C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ad_check_lev3_dir.h,v 1.6 2013/04/13 20:51:32 heimbach Exp $
C $Name:  $

CADJ STORE iceMask    = tapelev3, key = ilev_3
CADJ STORE iceHeight  = tapelev3, key = ilev_3
CADJ STORE snowHeight = tapelev3, key = ilev_3
CADJ STORE snowAge = tapelev3, key = ilev_3
CADJ STORE Tsrf    = tapelev3, key = ilev_3
CADJ STORE Qice1   = tapelev3, key = ilev_3
CADJ STORE Qice2   = tapelev3, key = ilev_3
CADJ STORE hOceMxL = tapelev3, key = ilev_3
CADJ STORE ocefwfx = tapelev3, key = ilev_3
CADJ STORE oceqnet = tapelev3, key = ilev_3
CADJ STORE ocesflx = tapelev3, key = ilev_3

CADJ STORE saltflux = tapelev3, key = ilev_3

#ifdef ATMOSPHERIC_LOADING
CADJ STORE siceload = tapelev3, key = ilev_3
#endif

CADJ STORE sheating = tapelev3, key = ilev_3
CADJ STORE tice1,tice2 = tapelev3, key = ilev_3

#ifdef ALLOW_THSICE_COST_TEST
CADJ STORE objf_thsice = tapelev3, key = ilev_3
#endif
