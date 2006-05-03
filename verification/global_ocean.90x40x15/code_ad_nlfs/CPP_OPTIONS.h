C
C $Header: /u/gcmpack/MITgcm/verification/global_ocean.90x40x15/code_ad_nlfs/Attic/CPP_OPTIONS.h,v 1.2 2006/05/03 23:37:13 heimbach Exp $
C $Name:  $

C CPP flags controlling which code in included in the files that
C will be compiled.

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

#include "PACKAGES_CONFIG.h"

C********* RELEVANT CHANGES *********

C o Exact volume conservation
#define EXACT_CONSERV

C o Nonlinear free surface
#define NONLIN_FRSURF

C o NEW OPTION to disable rStar (z*) code
cph#define DISABLE_RSTAR_CODE

C********* RELEVANT CHANGES *********

#ifdef ALLOW_KPP
#define  SHORTWAVE_HEATING
#endif

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude call to S/R CONVECT
#define INCLUDE_CONVECT_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

C o Allow nonHydrostatic code
#undef  ALLOW_NONHYDROSTATIC

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

#ifdef ALLOW_AUTODIFF
# include "ECCO_CPPOPTIONS.h"
#endif

#endif /* CPP_OPTIONS_H */


