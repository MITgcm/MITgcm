C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_ad_check_lev4_dir.h,v 1.4 2013/06/26 20:47:02 heimbach Exp $
C $Name:  $

#ifdef ALLOW_DIC

C common CARBON_NEEDS
CADJ STORE pH                = tapelev4, key = ilev_4
CADJ STORE fice              = tapelev4, key = ilev_4
CADJ STORE silica            = tapelev4, key = ilev_4
CADJ STORE atmospco2         = tapelev4, key = ilev_4

C common dic_load
CADJ STORE dicwind0,dicwind1   = tapelev4, key = ilev_4
CADJ STORE ice0,ice1         = tapelev4, key = ilev_4
CADJ STORE atmosp0,atmosp1   = tapelev4, key = ilev_4
CADJ STORE silica0,silica1   = tapelev4, key = ilev_4
#ifdef ALLOW_FE
CADJ STORE feinput0,feinput1 = tapelev4, key = ilev_4
#endif

CADJ STORE gsm_s             = tapelev4, key = ilev_4
CADJ STORE co2atmos,pco2     = tapelev4, key = ilev_4
CADJ STORE total_atmos_carbon  = tapelev4, key = ilev_4

#ifdef DIC_BIOTIC
CADJ STORE omegaC  = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_COST
CADJ STORE totcost  = tapelev4, key = ilev_4
#endif

#endif /* ALLOW_DIC */

