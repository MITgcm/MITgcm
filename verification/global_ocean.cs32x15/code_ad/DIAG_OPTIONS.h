C $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code_ad/DIAG_OPTIONS.h,v 1.1 2008/06/14 16:58:32 heimbach Exp $
C $Name:  $

#ifndef DIAG_OPTIONS_H
#define DIAG_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_DIAGNOSTICS

#include "CPP_OPTIONS.h"

CEH3 package-specific options go here

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
