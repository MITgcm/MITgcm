C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/SHAP_FILT_OPTIONS.h,v 1.7 2011/12/24 01:09:41 jmc Exp $
C $Name:  $

C CPP options file for pkg SHAP_FILT

#ifndef SHAP_FILT_OPTIONS_H
#define SHAP_FILT_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_SHAP_FILT
C     Package-specific Options & Macros go here

C Use [1-d_yy^n)(1-d_xx^n] instead of [1-d_xx^n-d_yy^n]
C This changes the spectral response function dramatically.
C You need to do some analysis before changing this option. ;^)
#define SEQUENTIAL_2D_SHAP

C This invokes the older code which produces the same numbers
C and might even be faster because it does not do multiple
C exchanges within the filter. However, this requires the
C overlap to be sufficiently wide and also does not work
C for arbitrarily arranged tiles (ie. as in cubed-sphere).
C *DO NOT USE THIS OPTION UNLESS YOU REALLY WANT TO*  :-(
#undef  USE_OLD_SHAPIRO_FILTERS

#ifdef USE_OLD_SHAPIRO_FILTERS
C Boundary conditions for U,V Shapiro code
C Horizontal shear is calculated as if the boundaries are no-slip
C Note: option NO_SLIP_SHAP only used in OLD_SHAPIRO_FILTERS ;
C   it is replaced by parameter "Shap_noSlip=1." in new S/R.
#undef  NO_SLIP_SHAP
#endif

C Masking of vorticity in dissipation terms (and Shap-Filt)
C  can be different from the masking applied for momentum advection;
C This option allows to use the local S/R: SHAP_FILT_RELVORT3 to
C compute vorticity, instead of pkg/mom_common S/R: MOM_CALC_RELVORT3
#undef  USE_SHAP_CALC_VORTICITY

#endif /* ALLOW_SHAP_FILT */
#endif /* SHAP_FILT_OPTIONS_H */
