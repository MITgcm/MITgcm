C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_ad_check_lev3_dir.h,v 1.1 2008/04/24 21:32:13 gforget Exp $
C $Name:  $

#ifdef ALLOW_DIC

C common CARBON_NEEDS
CADJ STORE pH                = tapelev3, key = ilev_3
CADJ STORE fice              = tapelev3, key = ilev_3
CADJ STORE silica            = tapelev3, key = ilev_3
CADJ STORE atmospco2         = tapelev3, key = ilev_3

C common dic_load
CADJ STORE dicwind0,dicwind1   = tapelev3, key = ilev_3
CADJ STORE ice0,ice1         = tapelev3, key = ilev_3
CADJ STORE atmosp0,atmosp1   = tapelev3, key = ilev_3
CADJ STORE silica0,silica1   = tapelev3, key = ilev_3
#ifdef ALLOW_FE
CADJ STORE feinput0,feinput1 = tapelev3, key = ilev_3
#endif

CADJ STORE gsm_s             = tapelev3, key = ilev_3
CADJ STORE co2atmos          = tapelev3, key = ilev_3
CADJ STORE total_atmos_carbon  = tapelev3, key = ilev_3


#endif /* ALLOW_DIC */

