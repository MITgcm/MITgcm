C
C $Header: /u/gcmpack/MITgcm/verification/global_ocean.90x40x15/code_ad_vecinv/Attic/CPP_OPTIONS.h,v 1.10 2003/11/04 21:39:42 edhill Exp $
C $Name:  $

C CPP flags controlling which code in included in the files that
C will be compiled.

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

#include "PACKAGES_CONFIG.h"

C o Shortwave heating as extra term in external_forcing.F
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

C o Read/write of checkpoint files for restarting.
#undef OLD_STYLE_WITH_MANY_FILES

C o Exact volume conservation
#define EXACT_CONSERV

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

#ifdef ALLOW_AUTODIFF
# include "ECCO_CPPOPTIONS.h"
#endif

#endif /* CPP_OPTIONS_H */

