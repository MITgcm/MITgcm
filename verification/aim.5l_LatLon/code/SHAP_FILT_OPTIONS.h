C $Header: /u/gcmpack/MITgcm/verification/aim.5l_LatLon/code/Attic/SHAP_FILT_OPTIONS.h,v 1.2 2001/05/29 14:01:48 adcroft Exp $
C $Name:  $

C Header file for package pkg/shap_filt

#include "CPP_OPTIONS.h"

C Use [1-d_yy^n)(1-d_xx^n] instead of [1-d_xx^n-d_yy^n]
C This changes the spectral response function dramatically.
C You need to do some analysis before changing this option. ;^)
#define SEQUENTIAL_2D_SHAP

C Boundary conditions for U,V Shapiro code
C Horizontal shear is calculated as if the boundaries are no-slip
#define NO_SLIP_SHAP

C This invokes the older code which produces the same numbers
C and might even be faster because it does not do multiple
C exchanges within the filter. However, this requires the
C overlap to be sufficiently wide and also does not work
C for arbitrarily arranged tiles (ie. as in cubed-sphere).
C *DO NOT USE THIS OPTION UNLESS YOU REALLY WANT TO*  :-(
#undef  USE_OLD_SHAPIRO_FILTERS
