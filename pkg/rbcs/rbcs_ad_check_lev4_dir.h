C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_ad_check_lev4_dir.h,v 1.3 2007/10/09 00:09:25 jmc Exp $
C $Name:  $

CADJ STORE rbct0 = tapelev4, key = ilev_4
CADJ STORE rbct1 = tapelev4, key = ilev_4
CADJ STORE rbcs0 = tapelev4, key = ilev_4
CADJ STORE rbcs1 = tapelev4, key = ilev_4

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = tapelev4, key = ilev_4
CADJ STORE rbcptr1 = tapelev4, key = ilev_4
#endif
