C $Header: /u/gcmpack/MITgcm/verification/bottom_ctrl_5x5/code_ad/CPP_OPTIONS.h,v 1.2 2007/10/09 02:36:41 jmc Exp $
C $Name:  $

C
C CPP flags controlling which code in included in the files that
C will be compiled.

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

#include "PACKAGES_CONFIG.h"

CmlC not sure of this
Cmlcph(
Cml#define DISABLE_MOM_VECINV
Cml#define MOMVISCOSITY
Cml#define MOMADVECTION
Cmlcph)

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

C o NEW OPTION to disable rStar (z*) code
#define DISABLE_RSTAR_CODE

C o Exact volume conservation
#define EXACT_CONSERV

c o Allow atmospheric loading
#define ATMOSPHERIC_LOADING

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

#ifdef ALLOW_AUTODIFF
# include "ECCO_CPPOPTIONS.h"
#endif

#endif /* CPP_OPTIONS_H */
