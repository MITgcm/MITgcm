C $Header: /u/gcmpack/MITgcm/pkg/showflops/SHOWFLOPS_OPTIONS.h,v 1.2 2007/06/04 21:36:16 heimbach Exp $
C $Name:  $

C CPP options file for SHOWFLOPS package
C
C Use this file for selecting options within the SHOWFLOPS package

#ifndef SHOWFLOPS_OPTIONS_H
#define SHOWFLOPS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_SHOWFLOPS

#include "CPP_OPTIONS.h"

#define TIME_PER_TIMESTEP
#define USE_PAPI_FLOPS
#define USE_FLIPS
#define PAPI_VERSION
#undef USE_PCL_FLOPS

#endif /* ALLOW_SHOWFLOPS */
#endif /* SHOWFLOPS_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
