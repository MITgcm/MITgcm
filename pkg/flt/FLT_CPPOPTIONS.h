C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/FLT_CPPOPTIONS.h,v 1.2 2004/09/07 16:19:30 edhill Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

c Include/Exclude part that allows 3-dimensional advection of floats
c 
#define ALLOW_3D_FLT

c Use the alternative method of adding random noise to float advection
c 
#define USE_FLT_ALT_NOISE

c Add noise also to the vertical velocity of 3D floats
c  
#ifdef ALLOW_3D_FLT
#define ALLOW_FLT_3D_NOISE
#endif


