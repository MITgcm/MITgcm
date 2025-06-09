#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

CBOP
C !ROUTINE: CPP_OPTIONS.h
C !INTERFACE:
C #include "CPP_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | main CPP options file for the model:
C | Control which optional features to compile in model/src code.
C *==================================================================*
CEOP

C CPP flags controlling particular source code features

C-- Forcing code options:

C o Shortwave heating as extra term in APPLY_FORCING_T (apply_forcing.F)
#define SHORTWAVE_HEATING

C o Include/exclude Geothermal Heat Flux at the bottom of the ocean
#undef ALLOW_GEOTHERMAL_FLUX

C o Allow to account for heating due to friction (and momentum dissipation)
#undef ALLOW_FRICTION_HEATING

C o Allow mass source or sink of Fluid in the interior
C   (3-D generalisation of oceanic real-fresh water flux)
#define ALLOW_ADDFLUID

C o Include pressure loading code
#define ATMOSPHERIC_LOADING

C o Include/exclude balancing surface forcing fluxes code
#define ALLOW_BALANCE_FLUXES

C o Include/exclude balancing surface forcing relaxation code
#define ALLOW_BALANCE_RELAX

C o Include/exclude checking for negative salinity
#undef CHECK_SALINITY_FOR_NEGATIVE_VALUES

C-- Options to discard parts of the main code:

C o Exclude/allow external forcing-fields load
C   this allows to read & do simple linear time interpolation of oceanic
C   forcing fields, if no specific pkg (e.g., EXF) is used to compute them.
#undef EXCLUDE_FFIELDS_LOAD
C   If defined, use same method (with pkg/autodiff compiled or not) for checking
C   when to load new reccord ; by default, use simpler method with pkg/autodiff.
#undef STORE_LOADEDREC_TEST

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude sound speed calculation code
C o (Note that this is a diagnostic from Del Grasso algorithm, not derived
C    from EOS)
#undef INCLUDE_SOUNDSPEED_CALC_CODE

C-- Vertical mixing code options:

C o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT
#define INCLUDE_CONVECT_CALL

C o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT_INI, turned off by
C   default because it is an unpopular historical left-over
#undef INCLUDE_CONVECT_INI_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

C o Allow full 3D specification of vertical diffusivity
#undef ALLOW_3D_DIFFKR

C o Allow latitudinally varying BryanLewis79 vertical diffusivity
#undef ALLOW_BL79_LAT_VARY

C o Exclude/allow partial-cell effect (physical or enhanced) in vertical mixing
C   this allows to account for partial-cell in vertical viscosity and diffusion,
C   either from grid-spacing reduction effect or as artificially enhanced mixing
C   near surface & bottom for too thin grid-cell
#undef EXCLUDE_PCELL_MIX_CODE

C o Exclude/allow to use isotropic 3-D Smagorinsky viscosity as diffusivity
C   for tracers (after scaling by constant Prandtl number)
#undef ALLOW_SMAG_3D_DIFFUSIVITY

C-- Time-stepping code options:

C o Include/exclude combined Surf.Pressure and Drag Implicit solver code
#define ALLOW_SOLVE4_PS_AND_DRAG

C o Include/exclude Implicit vertical advection code
#define INCLUDE_IMPLVERTADV_CODE

C o Include/exclude AdamsBashforth-3rd-Order code
#undef ALLOW_ADAMSBASHFORTH_3

C o Include/exclude Quasi-Hydrostatic Stagger Time-step AdamsBashforth code
#undef ALLOW_QHYD_STAGGER_TS

C-- Model formulation options:

C o Allow/exclude "Exact Convervation" of fluid in Free-Surface formulation
C   that ensures that d/dt(eta) is exactly equal to - Div.Transport
#define EXACT_CONSERV

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that grid-cell thickness (hFactors) varies with time
#define NONLIN_FRSURF
C o Disable code for rStar coordinate and/or code for Sigma coordinate
c#define DISABLE_RSTAR_CODE
c#define DISABLE_SIGMA_CODE

C o Include/exclude nonHydrostatic code
#define ALLOW_NONHYDROSTATIC

C o Include/exclude GM-like eddy stress in momentum code
#undef ALLOW_EDDYPSI

C-- Algorithm options:

C o Include/exclude code for Non Self-Adjoint (NSA) conjugate-gradient solver
#undef ALLOW_CG2D_NSA

C o Include/exclude code for single reduction Conjugate-Gradient solver
#define ALLOW_SRCG

C o Choices for implicit solver routines solve_*diagonal.F
C   The following has low memory footprint, but not suitable for AD
#undef SOLVE_DIAGONAL_LOWMEMORY
C   The following one suitable for AD but does not vectorize
#undef SOLVE_DIAGONAL_KINNER

C   Implementation alternative (might be faster on some platforms ?)
#undef USE_MASK_AND_NO_IF

C-- Retired code options:

C-  These 2 flags: ISOTROPIC_COS_SCALING & COSINEMETH_III have no effect
C   here as they are reset in GAD_OPTIONS.h and in MOM_COMMON_OPTIONS.h
C   for tracer diffusivity and momentum viscosity respectively

C o Use LONG.bin, LATG.bin, etc., initialization for ini_curviliear_grid.F
C   Default is to use "new" grid files (OLD_GRID_IO undef) but OLD_GRID_IO
C   is still useful with, e.g., single-domain curvilinear configurations.
#undef OLD_GRID_IO

C-- Other option files:

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

C-  Place where multi-pkg header file ECCO_CPPOPTIONS.h used to be included

#endif /* CPP_OPTIONS_H */
