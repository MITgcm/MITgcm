#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: DIC_OPTIONS.h
C !INTERFACE:
C #include "DIC_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "dic":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_DIC
C     Package-specific Options & Macros go here

C ABIOTIC OPTIONS
C Compile Munhoven (2013) "Solvesaphe" package for pH/pCO2
C  can still select Follows et al (2006) solver in data.dic,
C  but will use solvesaphe dissociation coefficient options.
#undef CARBONCHEM_SOLVESAPHE

C In S/R CARBON_CHEM convert ak1 and ak2 to the total pH scale
C  consistent with other coefficients (currently on the seawater scale).
C NOTE: Has NO effect when CARBONCHEM_SOLVESAPHE is defined (different
C  coeffs are used).
#undef CARBONCHEM_TOTALPHSCALE

C BIOTIC OPTIONS
#define DIC_BIOTIC
#define ALLOW_O2
#undef ALLOW_FE
#undef READ_PAR
#undef MINFE
#undef DIC_NO_NEG
#undef DIC_BOUNDS
C these all need to be defined for coupling to atmospheric model:
#undef USE_QSW
#undef USE_QSW_UNDERICE
#undef USE_PLOAD

C use surface salinity forcing (scaled by mean surf value) for DIC & ALK forcing
#undef ALLOW_OLD_VIRTUALFLUX

C put back bugs related to Water-Vapour in carbonate chemistry & air-sea fluxes
#undef WATERVAP_BUG

C dissolution only below saturation horizon following method by Karsten Friis
#undef DIC_CALCITE_SAT

C Include self-shading effect by phytoplankton
#undef LIGHT_CHL

C Include iron sediment source using DOP flux
#undef SEDFE

C For Adjoint built
#undef DIC_AD_SAFE

C Include code that copies the gchemApCO2 fieldto AtmospCO2 in S/R DIC_FIELDS_LOAD.
C For TAF-AD code this means that TAF will no generate AD-code for dic_atmos.F
C This flag only has an effect if GCHEM_ALLOW_FFIELDS is defined.
#undef DIC_ALLOW_READ_ATMOSPCO2

#endif /* ALLOW_DIC */
#endif /* DIC_OPTIONS_H */
