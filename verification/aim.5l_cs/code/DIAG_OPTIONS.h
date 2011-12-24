C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/DIAG_OPTIONS.h,v 1.2 2011/12/24 01:17:50 jmc Exp $
C $Name:  $

#ifndef DIAG_OPTIONS_H
#define DIAG_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_DIAGNOSTICS
C     Package-specific Options & Macros go here

C allow to define specific regions and the corresponding mask ;
C  used to perform regional statistics over a limited area
#define DIAGSTATS_REGION_MASK

C allow to stop & restart at any time (i.e. not at a multiple of
C  the diagnostics frequency) reading diagnostics storage arrays
C  from pickup file.
C Note: Use with cautious since it does not work for all restart
C  cases (e.g., changing data.diagnostics).
#undef  DIAGNOSTICS_HAS_PICKUP

#endif /* ALLOW_DIAGNOSTICS */
#endif /* DIAG_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
