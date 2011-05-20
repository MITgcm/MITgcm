C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_ad_check_lev3_dir.h,v 1.4 2011/05/20 00:45:59 gforget Exp $
C $Name:  $

CADJ STORE rbct0 = tapelev3, key = ilev_3
CADJ STORE rbct1 = tapelev3, key = ilev_3
CADJ STORE rbcs0 = tapelev3, key = ilev_3
CADJ STORE rbcs1 = tapelev3, key = ilev_3

#ifndef DISABLE_RBCS_MOM
CADJ STORE rbcu0 = tapelev3, key = ilev_3
CADJ STORE rbcu1 = tapelev3, key = ilev_3
CADJ STORE rbcv0 = tapelev3, key = ilev_3
CADJ STORE rbcv1 = tapelev3, key = ilev_3
#endif

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = tapelev3, key = ilev_3
CADJ STORE rbcptr1 = tapelev3, key = ilev_3
#endif
