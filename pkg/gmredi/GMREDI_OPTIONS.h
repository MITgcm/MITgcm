C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI_OPTIONS.h,v 1.3 2001/02/04 14:38:49 cnh Exp $
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

C This allows the leading diagonal (top two rows) to be non-unity
C (a feature required when tapering adiabatically).
#undef  GM_VISBECK_VARIABLE_K


#endif /* ALLOW_GMREDI */
