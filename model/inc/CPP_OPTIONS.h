C $Header: /u/gcmpack/MITgcm/model/inc/CPP_OPTIONS.h,v 1.13 2000/11/13 16:32:57 heimbach Exp $
C

C 
C CPP flags controlling which code in included in the files that
C will be compiled.
C

C o Include/exclude code for GM/Redi parameterization
#undef  ALLOW_GMREDI

C o Include/exclude code for KPP mixing scheme
#define  ALLOW_KPP

C o Shortwave heating as extra term in external_forcing.F
#ifdef ALLOW_KPP
#define  SHORTWAVE_HEATING
#endif

C o Include/exclude code for C-D grid method of integrating the 
C   coriolis terms
#define  INCLUDE_CD_CODE

C o Include/exclude code for open-boundary conditions
#undef  ALLOW_OBCS

C o Include/exclude diagnostics package interface code
#define  INCLUDE_DIAGNOSTICS_INTERFACE_CODE

C o Include/exclude latitude circle FFT filter
#undef  INCLUDE_LAT_CIRC_FFT_FILTER_CODE

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
#define  INCLUDE_T_FORCING_CODE

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

C o Include/exclude prognostic variable shapiro filter code
C   Note - Shapiro filter of prognostics variables requires the
C          three steps "step forward including edges", filter, 
C          "communicate edges".
C           If the filtering code is included then we do not use the 
C          pipelined "step forward including edges" in S/R DYNAMICS. 
C          Instead the three steps are performed before DYNAMICS one 
C          after another in an un-pipelined fashion.
#undef  INCLUDE_SHAPIRO_FILTER_CODE
#ifdef  INCLUDE_SHAPIRO_FILTER_CODE
#undef  DO_PIPELINED_CORRECTION_STEP
#endif
#ifndef INCLUDE_SHAPIRO_FILTER_CODE
#define DO_PIPELINED_CORRECTION_STEP
#endif

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



