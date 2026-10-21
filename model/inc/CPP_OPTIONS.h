#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

!BOP
! !ROUTINE: CPP_OPTIONS.h
! !INTERFACE:
! #include "CPP_OPTIONS.h"

! !DESCRIPTION:
! *==================================================================*
! | main CPP options file for the model:
! | Control which optional features to compile in model/src code.
! *==================================================================*
!EOP

! CPP flags controlling particular source code features

!-- Forcing code options:

! o Shortwave heating as extra term in APPLY_FORCING_T (apply_forcing.F)
#undef SHORTWAVE_HEATING

! o Include/exclude Geothermal Heat Flux at the bottom of the ocean
#undef ALLOW_GEOTHERMAL_FLUX

! o Allow to account for heating due to friction (and momentum dissipation)
#undef ALLOW_FRICTION_HEATING

! o Allow mass source or sink of Fluid in the interior
!   (3-D generalisation of oceanic real-fresh water flux)
#undef ALLOW_ADDFLUID

! o Include pressure loading code
#define ATMOSPHERIC_LOADING

! o Include/exclude balancing surface forcing fluxes code
#undef ALLOW_BALANCE_FLUXES

! o Include/exclude balancing surface forcing relaxation code
#undef ALLOW_BALANCE_RELAX

! o Include/exclude checking for negative salinity
#undef CHECK_SALINITY_FOR_NEGATIVE_VALUES

!-- Options to discard parts of the main code:

! o Exclude/allow external forcing-fields load
!   this allows to read & do simple linear time interpolation of oceanic
!   forcing fields, if no specific pkg (e.g., EXF) is used to compute them.
#undef EXCLUDE_FFIELDS_LOAD
!   If defined, use same method (with pkg/autodiff compiled or not) for checking
!   when to load new reccord ; by default, use simpler method with pkg/autodiff.
#undef STORE_LOADEDREC_TEST

! o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

! o Include/exclude sound speed calculation code
! o (Note that this is a diagnostic from Del Grasso algorithm, not derived
!    from EOS)
#undef INCLUDE_SOUNDSPEED_CALC_CODE

!-- Vertical mixing code options:

! o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT
#define INCLUDE_CONVECT_CALL

! o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT_INI, turned off by
!   default because it is an unpopular historical left-over
#undef INCLUDE_CONVECT_INI_CALL

! o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

! o Allow full 3D specification of vertical diffusivity
#undef ALLOW_3D_DIFFKR

! o Allow latitudinally varying BryanLewis79 vertical diffusivity
#undef ALLOW_BL79_LAT_VARY

! o Exclude/allow partial-cell effect (physical or enhanced) in vertical mixing
!   this allows to account for partial-cell in vertical viscosity and diffusion,
!   either from grid-spacing reduction effect or as artificially enhanced mixing
!   near surface & bottom for too thin grid-cell
#undef EXCLUDE_PCELL_MIX_CODE

! o Exclude/allow to use isotropic 3-D Smagorinsky viscosity as diffusivity
!   for tracers (after scaling by constant Prandtl number)
#undef ALLOW_SMAG_3D_DIFFUSIVITY

!-- Time-stepping code options:

! o Include/exclude combined Surf.Pressure and Drag Implicit solver code
#undef ALLOW_SOLVE4_PS_AND_DRAG

! o Include/exclude Implicit vertical advection code
#define INCLUDE_IMPLVERTADV_CODE

! o Include/exclude AdamsBashforth-3rd-Order code
#undef ALLOW_ADAMSBASHFORTH_3

! o Include/exclude Quasi-Hydrostatic Stagger Time-step AdamsBashforth code
#undef ALLOW_QHYD_STAGGER_TS

!-- Model formulation options:

! o Allow the use of Non-Linear Free-Surface formulation
!   this implies that grid-cell thickness (hFactors) varies with time
#undef NONLIN_FRSURF
! o Disable code for rStar coordinate and/or code for Sigma coordinate
!#define DISABLE_RSTAR_CODE
!#define DISABLE_SIGMA_CODE

! o Include/exclude nonHydrostatic code
#undef ALLOW_NONHYDROSTATIC

! o Include/exclude GM-like eddy stress in momentum code
#undef ALLOW_EDDYPSI

!-- Algorithm options:

! o Include/exclude code for Non Self-Adjoint (NSA) conjugate-gradient solver
#undef ALLOW_CG2D_NSA

! o Include/exclude code for single reduction Conjugate-Gradient solver
#define ALLOW_SRCG

! o Choices for implicit solver routines solve_*diagonal.F
!   The following has low memory footprint, but not suitable for AD
#undef SOLVE_DIAGONAL_LOWMEMORY
!   The following one suitable for AD but does not vectorize
#undef SOLVE_DIAGONAL_KINNER

!   Implementation alternative (might be faster on some platforms ?)
#undef USE_MASK_AND_NO_IF

!-- Retired code options:

!-  These 2 flags: ISOTROPIC_COS_SCALING & COSINEMETH_III have no effect
!   here as they are reset in GAD_OPTIONS.h and in MOM_COMMON_OPTIONS.h
!   for tracer diffusivity and momentum viscosity respectively

! o Use "OLD" UV discretisation near boundaries (*not* recommended)
!   Note - only works with pkg/mom_fluxform and "no_slip_sides=.FALSE."
     ! because the old code did not have no-slip BCs
#undef OLD_ADV_BCS

! o Use LONG.bin, LATG.bin, etc., initialization for ini_curviliear_grid.F
!   Default is to use "new" grid files (OLD_GRID_IO undef) but OLD_GRID_IO
!   is still useful with, e.g., single-domain curvilinear configurations.
#undef OLD_GRID_IO

! o Use old EXTERNAL_FORCING_U,V,T,S subroutines (for backward compatibility)
#undef USE_OLD_EXTERNAL_FORCING

!-- Other option files:

! o Execution environment support options
#include "CPP_EEOPTIONS.h"

!-  Place where multi-pkg header file ECCO_CPPOPTIONS.h used to be included

#endif /* CPP_OPTIONS_H */
