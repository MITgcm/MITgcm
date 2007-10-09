C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_ad_check_lev3_dir.h,v 1.3 2007/10/09 00:09:25 jmc Exp $
C $Name:  $

CADJ STORE rbct0 = tapelev3, key = ilev_3
CADJ STORE rbct1 = tapelev3, key = ilev_3
CADJ STORE rbcs0 = tapelev3, key = ilev_3
CADJ STORE rbcs1 = tapelev3, key = ilev_3

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = tapelev3, key = ilev_3
CADJ STORE rbcptr1 = tapelev3, key = ilev_3
#endif
