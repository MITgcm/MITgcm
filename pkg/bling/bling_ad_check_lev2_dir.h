C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_ad_check_lev2_dir.h,v 1.2 2016/05/29 01:50:29 mmazloff Exp $
C $Name:  $

#ifdef ALLOW_BLING

C common CARBON_NEEDS
CADJ STORE pH                = tapelev2, key = ilev_2
CADJ STORE fice              = tapelev2, key = ilev_2
CADJ STORE silica            = tapelev2, key = ilev_2

CADJ STORE irr_mem           = tapelev2, key = ilev_2
CADJ STORE P_sm              = tapelev2, key = ilev_2 
CADJ STORE P_lg              = tapelev2, key = ilev_2 
CADJ STORE P_diaz            = tapelev2, key = ilev_2
CADJ STORE chl               = tapelev2, key = ilev_2

C CMM(
CADJ STORE atmosp0        = tapelev2, key = ilev_2
CADJ STORE atmosp1        = tapelev2, key = ilev_2
CADJ STORE feinput0       = tapelev2, key = ilev_2
CADJ STORE feinput1       = tapelev2, key = ilev_2
CADJ STORE ice0           = tapelev2, key = ilev_2
CADJ STORE ice1           = tapelev2, key = ilev_2
CADJ STORE silica0        = tapelev2, key = ilev_2
CADJ STORE silica1        = tapelev2, key = ilev_2
C CMM)

#ifdef ALLOW_BLING_COST
CADJ STORE totcost  = tapelev2, key = ilev_2
#endif

#endif /* ALLOW_BLING */

