C $Header: /u/gcmpack/MITgcm/verification/global_ocean.90x40x15/code_ad/GMREDI_OPTIONS.h,v 1.4 2003/01/10 17:21:26 heimbach Exp $
C $Name:  $

C CPP options file for GM/Redi package
C
C Use this file for selecting options within the GM/Redi package
C
C GM/Redi is enabled with ALLOW_GMREDI in CPP_OPTIONS.h

#include "CPP_OPTIONS.h"

#ifdef ALLOW_GMREDI

C Specify as tapering scheme either 'orig' or 'clipping',
C otherwise one of the following is used:
C 'linear', 'gkw91', 'dm95', 'ldd97'
#undef GM_TAPER_ORIG_CLIPPING
#undef GM_TAPER_AC02
#define GM_TAPER_REST
 
C This allows to use Visbeck et al formulation to compute K_GM+Redi
#define GM_VISBECK_VARIABLE_K

C This allows the leading diagonal (top two rows) to be non-unity
C (a feature required when tapering adiabatically).
#define  GM_NON_UNITY_DIAGONAL

C Allows to use different values of K_GM and K_Redi ; also to
C be used with the advective form (Bolus velocity) of GM
#define  GM_EXTRA_DIAGONAL

C Allows to use the advective form (Bolus velocity) of GM
C  instead of the Skew-Flux form (=default)
#define  GM_BOLUS_ADVEC


#endif /* ALLOW_GMREDI */
