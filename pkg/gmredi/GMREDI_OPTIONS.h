C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI_OPTIONS.h,v 1.4 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

C CPP options file for GM/Redi package
C
C Use this file for selecting options within the GM/Redi package
C
C GM/Redi is enabled with ALLOW_GMREDI in CPP_OPTIONS.h

#include "CPP_OPTIONS.h"

#ifdef ALLOW_GMREDI


C This allows the leading diagonal (top two rows) to be non-unity
C (a feature required when tapering adiabatically).
#undef  GM_NON_UNITY_DIAGONAL

C Allows to use different values of K_GM and K_Redi ; also to
C be used with the advective form (Bolus velocity) of GM
#undef  GM_EXTRA_DIAGONAL

C Allows to use the advective form (Bolus velocity) of GM
C  instead of the Skew-Flux form (=default)
#undef  GM_BOLUS_ADVEC

C This allows to use Visbeck et al formulation to compute K_GM+Redi
#undef  GM_VISBECK_VARIABLE_K


#endif /* ALLOW_GMREDI */
