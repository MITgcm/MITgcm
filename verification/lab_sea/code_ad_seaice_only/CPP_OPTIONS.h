C $Header: /u/gcmpack/MITgcm/verification/lab_sea/code_ad_seaice_only/Attic/CPP_OPTIONS.h,v 1.2 2008/08/21 16:03:22 jmc Exp $
C $Name:  $

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

#include "PACKAGES_CONFIG.h"

C CPP flags controlling particular source code features

C o Shortwave heating as extra term in external_forcing.F
C Note: this should be a run-time option
#define SHORTWAVE_HEATING

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude call to S/R CONVECT
#undef INCLUDE_CONVECT_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

C o Include/exclude Implicit vertical advection code
#undef INCLUDE_IMPLVERTADV_CODE

C o Include/exclude nonHydrostatic code
#undef ALLOW_NONHYDROSTATIC

C o Include pressure loading code
#undef ATMOSPHERIC_LOADING

C o Use "Exact Convervation" of fluid in Free-Surface formulation
C   so that d/dt(eta) is exactly equal to - Div.Transport
#undef EXACT_CONSERV

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that surface thickness (hFactors) vary with time
#undef NONLIN_FRSURF

C o Use "OLD" UV discretisation near boundaries (*not* recommended)
C   Note - only works with  #undef NO_SLIP_LATERAL  in calc_mom_rhs.F
C          because the old code did not have no-slip BCs
#undef  OLD_ADV_BCS

C o Minimal time-averaged output: S, T, U, V, W, ETA, and phiHydLow.
#undef MINIMAL_TAVE_OUTPUT

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

C o Include/exclude code specific to the ECCO/SEALION version.
C   AUTODIFF or EXF package.
C   Currently controled by a single header file
C   For this to work, PACKAGES_CONFIG.h needs to be included!
cph#if (defined (ALLOW_AUTODIFF) || \
cph     defined (ALLOW_ECCO) || \
cph     defined (ALLOW_EXF))
#include "ECCO_CPPOPTIONS.h"
cph#endif

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

