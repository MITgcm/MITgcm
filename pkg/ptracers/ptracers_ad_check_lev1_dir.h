C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev1_dir.h,v 1.2 2014/08/15 19:18:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef NONLIN_FRSURF
CADJ STORE pTracer   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE gpTrNm1   = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif /* ALLOW_PTRACERS */
