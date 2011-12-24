C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI_OPTIONS.h,v 1.13 2011/12/24 01:04:47 jmc Exp $
C $Name:  $

C CPP options file for GM/Redi package
C Use this file for selecting options within the GM/Redi package

#ifndef GMREDI_OPTIONS_H
#define GMREDI_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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

C This allows to use Visbeck et al formulation to compute K_GM+Redi
#undef GM_VISBECK_VARIABLE_K
C Use old calculation (before 2007/05/24) of Visbeck etal K_GM+Redi
C (which depends on tapering scheme)
#undef OLD_VISBECK_CALC

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

#endif /* ALLOW_GMREDI */
#endif /* GMREDI_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
