C $Header: /u/gcmpack/MITgcm/pkg/showflops/SHOWFLOPS_OPTIONS.h,v 1.4 2011/12/24 01:09:41 jmc Exp $
C $Name:  $

C CPP options file for SHOWFLOPS package
C Use this file for selecting options within the SHOWFLOPS package

#ifndef SHOWFLOPS_OPTIONS_H
#define SHOWFLOPS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_SHOWFLOPS
C     Package-specific Options & Macros go here

#undef USE_FLIPS

#endif /* ALLOW_SHOWFLOPS */
#endif /* SHOWFLOPS_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
