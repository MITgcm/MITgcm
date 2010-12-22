C $Header: /u/gcmpack/MITgcm/pkg/flt/FLT_OPTIONS.h,v 1.2 2010/12/22 21:20:49 jahn Exp $
C $Name:  $

C CPP options file for FLT package

#ifndef FLT_OPTIONS_H
#define FLT_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_FLT

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

c define this to revert to old second-order Runge-Kutta integration
c
#undef  FLT_SECOND_ORDER_RUNGE_KUTTA

#endif /* ALLOW_FLT */
#endif /* FLT_OPTIONS_H */
