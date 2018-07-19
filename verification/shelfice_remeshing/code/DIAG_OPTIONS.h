C $Header: /u/gcmpack/MITgcm_contrib/verification_other/shelfice_remeshing/code/DIAG_OPTIONS.h,v 1.2 2016/01/22 16:09:33 dgoldberg Exp $
C $Name:  $

#ifndef DIAG_OPTIONS_H
#define DIAG_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_DIAGNOSTICS

#include "CPP_OPTIONS.h"

CEH3 package-specific options go here

C allow to define specific regions and the corresponding mask ;
C  used to perform regional statistics over a limited area
#undef DIAGSTATS_REGION_MASK

C allow to stop & restart at any time (i.e. not at a multiple of
C  the diagnostics frequency) reading diagnostics storage arrays
C  from pickup file.
C Note: Use with cautious since it does not work for all restart
C  cases (e.g., changing data.diagnostics).
#undef  DIAGNOSTICS_HAS_PICKUP

C for NetCDF (mnc) output: define a missing value (default is UNSET_RL)
C and fill land points of (so far only) scalar fields with it
#undef DIAGNOSTICS_MISSING_VALUE

#endif /* ALLOW_DIAGNOSTICS */
#endif /* DIAG_OPTIONS_H */


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
