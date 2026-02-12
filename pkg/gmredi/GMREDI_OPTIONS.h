#ifndef GMREDI_OPTIONS_H
#define GMREDI_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: GMREDI_OPTIONS.h
C !INTERFACE:
C #include "GMREDI_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for GM/Redi package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_GMREDI
C     Package-specific Options & Macros go here

C Designed to simplify the Ajoint code:
C #define GMREDI_WITH_STABLE_ADJOINT
C -- exclude the clipping/tapering part of the code that is not used
C #define GM_EXCLUDE_CLIPPING
C #define GM_EXCLUDE_FM07_TAP
C #define GM_EXCLUDE_AC02_TAP
C #define GM_EXCLUDE_TAPERING
C #define GM_EXCLUDE_SUBMESO

C Allows to read-in background 3-D Redi and GM diffusivity coefficients
C Note: need these to be defined for use as control (pkg/ctrl) parameters
#undef GM_READ_K3D_REDI
#undef GM_READ_K3D_GM

C This allows to use Visbeck et al formulation to compute K_GM+Redi
#undef GM_VISBECK_VARIABLE_K

C This allows to use the GEOMETRIC formulation to compute K_GM
#undef GM_GEOM_VARIABLE_K

C This allows the Bates et al formulation to calculate the
C bolus transport and K for Redi
#undef GM_BATES_K3D
#undef GM_BATES_PASSIVE

C This allows the leading diagonal (top two rows) to be non-unity
C (a feature required when tapering adiabatically).
#define GM_NON_UNITY_DIAGONAL

C Allows to use different values of K_GM and K_Redi ; also to
C be used with the advective form (Bolus velocity) of GM
#define GM_EXTRA_DIAGONAL

C Allows to use the advective form (Bolus velocity) of GM
C  instead of the Skew-Flux form (=default)
#define GM_BOLUS_ADVEC

C Allows to use the Boundary-Value-Problem method to evaluate GM Bolus transport
#define GM_BOLUS_BVP

C Allow QG Leith variable viscosity to be added to GMRedi coefficient
#undef ALLOW_GM_LEITH_QG

C Related to Adjoint-code:
#undef GM_AUTODIFF_EXCESSIVE_STORE
#undef GMREDI_MASK_SLOPES

#endif /* ALLOW_GMREDI */
#endif /* GMREDI_OPTIONS_H */
