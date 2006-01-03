C $Header: /u/gcmpack/MITgcm/verification/global1x1_tot/code/Attic/CPP_OPTIONS.h,v 1.4 2006/01/03 22:44:53 heimbach Exp $

C CPP flags controlling which code in included in the files that
C will be compiled.

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"

C o Shortwave heating as extra term in external_forcing.F
#ifdef ALLOW_KPP
#define  SHORTWAVE_HEATING
#endif

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude call to S/R FIND_RHO
#define INCLUDE_FIND_RHO_CALL

C o Include/exclude call to S/R CONVECT
#define INCLUDE_CONVECT_CALL

C o Include/exclude call to S/R CALC_ISOSLOPES
#define INCLUDE_CALC_ISOSLOPES_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

C o Allow nonHydrostatic code
#undef  ALLOW_NONHYDROSTATIC

C o Use "natural" boundary conditions for salinity
C   instead of the "virtual salt flux"
#undef  USE_NATURAL_BCS

C o Include/exclude exact volume conservaion
#define EXACT_CONSERV

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

C o Include/exclude code specific to the ECCO/SEALION version.
#ifdef ALLOW_ECCO
# include "ECCO_CPPOPTIONS.h"
#endif

#endif /* CPP_OPTIONS_H */


