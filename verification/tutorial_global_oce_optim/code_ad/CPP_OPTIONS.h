C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/CPP_OPTIONS.h,v 1.2 2008/01/18 03:39:26 dfer Exp $
C $Name:  $

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

C CPP flags controlling particular source code features

#include "PACKAGES_CONFIG.h"

C o Shortwave heating as extra term in external_forcing.F
C Note: this should be a run-time option
#undef SHORTWAVE_HEATING

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude call to S/R CONVECT
#define INCLUDE_CONVECT_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

C o Include/exclude nonHydrostatic code
#undef ALLOW_NONHYDROSTATIC

C o Use "Exact Convervation" of fluid in Free-Surface formulation
C   so that d/dt(eta) is exactly equal to - Div.Transport
#define EXACT_CONSERV

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

#ifdef ALLOW_AUTODIFF
# include "ECCO_CPPOPTIONS.h"
#endif

#endif /* CPP_OPTIONS_H */


