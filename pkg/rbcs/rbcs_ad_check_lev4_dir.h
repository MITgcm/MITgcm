C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_ad_check_lev4_dir.h,v 1.4 2011/05/20 00:45:59 gforget Exp $
C $Name:  $

CADJ STORE rbct0 = tapelev4, key = ilev_4
CADJ STORE rbct1 = tapelev4, key = ilev_4
CADJ STORE rbcs0 = tapelev4, key = ilev_4
CADJ STORE rbcs1 = tapelev4, key = ilev_4

#ifndef DISABLE_RBCS_MOM
CADJ STORE rbcu0 = tapelev4, key = ilev_4
CADJ STORE rbcu1 = tapelev4, key = ilev_4
CADJ STORE rbcv0 = tapelev4, key = ilev_4
CADJ STORE rbcv1 = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = tapelev4, key = ilev_4
CADJ STORE rbcptr1 = tapelev4, key = ilev_4
#endif
