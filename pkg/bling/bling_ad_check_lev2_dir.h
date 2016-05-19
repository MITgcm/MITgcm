C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_ad_check_lev2_dir.h,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
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

#ifdef ALLOW_BLING_COST
CADJ STORE totcost  = tapelev2, key = ilev_2
#endif

#endif /* ALLOW_BLING */

