C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_ad_check_lev3_dir.h,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
C $Name:  $

#ifdef ALLOW_BLING

C common CARBON_NEEDS
CADJ STORE pH                = tapelev3, key = ilev_3
CADJ STORE fice              = tapelev3, key = ilev_3
CADJ STORE silica            = tapelev3, key = ilev_3

CADJ STORE irr_mem           = tapelev3, key = ilev_3
CADJ STORE P_sm              = tapelev3, key = ilev_3 
CADJ STORE P_lg              = tapelev3, key = ilev_3 
CADJ STORE P_diaz            = tapelev3, key = ilev_3
CADJ STORE chl               = tapelev3, key = ilev_3

#ifdef ALLOW_BLING_COST
CADJ STORE totcost  = tapelev3, key = ilev_3
#endif

#endif /* ALLOW_BLING */

