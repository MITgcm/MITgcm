C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_ad_check_lev2_dir.h,v 1.3 2007/10/09 00:09:25 jmc Exp $
C $Name:  $

CADJ STORE rbct0 = tapelev2, key = ilev_2
CADJ STORE rbct1 = tapelev2, key = ilev_2
CADJ STORE rbcs0 = tapelev2, key = ilev_2
CADJ STORE rbcs1 = tapelev2, key = ilev_2

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = tapelev2, key = ilev_2
CADJ STORE rbcptr1 = tapelev2, key = ilev_2
#endif
