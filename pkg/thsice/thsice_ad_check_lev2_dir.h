C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ad_check_lev2_dir.h,v 1.6 2013/04/13 20:51:32 heimbach Exp $
C $Name:  $

CADJ STORE iceMask   = tapelev2, key = ilev_2
CADJ STORE iceHeight  = tapelev2, key = ilev_2
CADJ STORE snowHeight = tapelev2, key = ilev_2
CADJ STORE snowAge  = tapelev2, key = ilev_2
CADJ STORE Tsrf     = tapelev2, key = ilev_2
CADJ STORE Qice1    = tapelev2, key = ilev_2
CADJ STORE Qice2    = tapelev2, key = ilev_2
CADJ STORE hOceMxL  = tapelev2, key = ilev_2
CADJ STORE ocefwfx = tapelev2, key = ilev_2
CADJ STORE oceqnet = tapelev2, key = ilev_2
CADJ STORE ocesflx = tapelev2, key = ilev_2

CADJ STORE saltflux = tapelev2, key = ilev_2

#ifdef ATMOSPHERIC_LOADING
CADJ STORE siceload = tapelev2, key = ilev_2
#endif

CADJ STORE sheating = tapelev2, key = ilev_2
CADJ STORE tice1,tice2 = tapelev2, key = ilev_2

#ifdef ALLOW_THSICE_COST_TEST
CADJ STORE objf_thsice = tapelev2, key = ilev_2
#endif
