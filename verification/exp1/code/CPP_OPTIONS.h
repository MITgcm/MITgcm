C $Header: /u/gcmpack/MITgcm/verification/exp1/code/Attic/CPP_OPTIONS.h,v 1.9 2001/05/29 14:01:57 adcroft Exp $
C $Name:  $
C

C 
C CPP flags controlling which code in included in the files that
C will be compiled.
C

C o Include/exclude code for AIM package
#undef  ALLOW_AIM

C o Include/exclude code for GM/Redi parameterization
#undef  ALLOW_GMREDI

C o Include/exclude code for KPP mixing scheme
#undef  ALLOW_KPP

C o Shortwave heating as extra term in external_forcing.F
#ifdef ALLOW_KPP
#define  SHORTWAVE_HEATING
#endif

C o Include/exclude code for Shapiro filters
#define ALLOW_SHAP_FILT

C o Include/exclude code for C-D grid method of integrating the 
C   coriolis terms
#define  INCLUDE_CD_CODE

C o Include/exclude code for open-boundary conditions
#undef   ALLOW_OBCS

C o Include/exclude diagnostics package interface code
#define  ALLOW_TIMEAVE

C o Include/exclude zonal FFT filter code
#undef  ALLOW_ZONAL_FILT

C o Include/exclude temperature advection code
#define  INCLUDE_T_ADVECTION_CODE
#ifdef   INCLUDE_T_ADVECTION_CODE
#define  _ADT(a)a
#endif
#ifndef  INCLUDE_T_ADVECTION_CODE
#define  _ADT(a)
#endif

C o Include/exclude temperature diffusion code
#define  INCLUDE_T_DIFFUSION_CODE
#ifdef   INCLUDE_T_DIFFUSION_CODE
#define  _LPT(a)a
#define  _BHT(a)a
#endif
#ifndef  INCLUDE_T_DIFFUSION_CODE
#define  _LPT(a)
#define  _BHT(a)
#endif

C o Include/exclude temperature forcing code
#undef   INCLUDE_T_FORCING_CODE

C o Include/exclude momentum advection code
#define  INCLUDE_MOMENTUM_ADVECTION_CODE
#ifdef   INCLUDE_MOMENTUM_ADVECTION_CODE
#define  _ADM(a)a
#endif
#ifndef  INCLUDE_MOMENTUM_ADVECTION_CODE
#define  _ADM(a)
#endif

C o Include/exclude laplacian viscosity code
#define  INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#ifdef   INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#define  _LPM(a)a
#endif
#ifndef  INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#define  _LPM(a)
#endif

C o Include/exclude biharmonic viscosity code
#define  INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
#ifdef   INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
#define  _BHM(a)a
#endif
#ifndef  INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
#define  _BHM(a)
#endif

C o Include/exclude gradient of phy_hyd code
#define INCLUDE_GRADPH_CODE
#ifdef  INCLUDE_GRADPH_CODE
#define _PHM(a)a
#endif
#ifndef INCLUDE_GRADPH_CODE
#define _PHM(a)
#endif

C o Include/exclude momentum forcing code
#define INCLUDE_MOMENTUM_FORCING_CODE

C o Include/exclude momentum eqn metric terms code
#define INCLUDE_MOMENTUM_METRIC_TERM_CODE

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude call to S/R CONVECT
#define INCLUDE_CONVECT_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

C o Allow nonHydrostatic code
#undef  ALLOW_NONHYDROSTATIC

C o Use "natural" boundary conditions for salinity
C   instead of the "virtual salt flux"
#undef  USE_NATURAL_BCS

C o Use "Exact Convervation" of fluid in Free-Surface formulation
C   so that d/dt(eta) is exactly equal to - Div.Transport
#define EXACT_CONSERV

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that surface thickness (hFactors) vary with time
#undef NONLIN_FRSURF

C o Use "OLD" UV discretisation near boundaries (*not* recommended)
C   Note - only works with  #undef NO_SLIP_LATERAL  in calc_mom_rhs.F
C          because the old code did not have no-slip BCs
#undef  OLD_ADV_BCS

C o Use "OLD" UV geometry on sphere (definately *NOT* recommended)
C   Note - only works with  #undef NO_SLIP_LATERAL  in calc_mom_rhs.F
C          because the old code did not have no-slip BCs
#undef  OLD_UV_GEOMETRY

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

C o Include/exclude code specific to the ECCO/SEALION version.
#undef INCLUDE_ECCO_PACKAGE
#ifdef INCLUDE_ECCO_PACKAGE
#include "ECCO_CPPOPTIONS.h"
#endif

