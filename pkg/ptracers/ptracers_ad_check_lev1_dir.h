C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev1_dir.h,v 1.1 2011/01/18 19:15:56 heimbach Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef NONLIN_FRSURF
CADJ STORE ptracer   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE gptrnm1   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE gptr      = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif /* ALLOW_PTRACERS */
