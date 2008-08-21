C
C $Header: /u/gcmpack/MITgcm/verification/front_relax/code_ad/CPP_OPTIONS.h,v 1.9 2008/08/21 16:03:21 jmc Exp $
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

C o Use "Exact Convervation" of fluid in Free-Surface formulation
C   so that d/dt(eta) is exactly equal to - Div.Transport
#define EXACT_CONSERV

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that surface thickness (hFactors) vary with time
#undef NONLIN_FRSURF

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

C o Add passive tracer advection routines
#define ALLOW_PASSIVE_TRACER

#ifdef ALLOW_AUTODIFF
# include "ECCO_CPPOPTIONS.h"
#endif

C o Allow full 3D specification of vertical diffusivity
#ifdef ALLOW_DIFFKR_CONTROL
C - Need to be defined if using DIFFKR_CONTROL
C   (alternatively, could have put this in ECCO_CPPOPTIONS)
#define ALLOW_3D_DIFFKR
#else
C - otherwise, can be turned on or off hereafter:
#undef  ALLOW_3D_DIFFKR
#endif /* ALLOW_DIFFKR_CONTROL */

#endif /* CPP_OPTIONS_H */
