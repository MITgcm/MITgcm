C     *=============================================================*
C     | GGL90_OPTIONS.h
C     | o CPP options file for GGL90 package.
C     *=============================================================*
C     | Use this file for selecting options within the GGL90
C     | package.
C     *=============================================================*

#ifndef GGL90_OPTIONS_H
#define GGL90_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_GGL90
C     Package-specific Options & Macros go here

C     Enable horizontal diffusion of TKE.
#undef ALLOW_GGL90_HORIZDIFF

C     Use horizontal averaging for viscosity and diffusivity as
C     originally implemented in OPA.
#undef ALLOW_GGL90_SMOOTH

C     allow IDEMIX model
#define ALLOW_GGL90_IDEMIX
#ifdef ALLOW_GGL90_IDEMIX
C     The cvmix version of idemix uses different regularisations for the
C     Coriolis parameter, buoyancy frequency etc, when used in the denominator
# undef GGL90_IDEMIX_CVMIX_VERSION
#endif

C     include Langmuir circulation parameterization
#undef ALLOW_GGL90_LANGMUIR

C     recover old bug prior to Jun 2023
#undef GGL90_MISSING_HFAC_BUG

#endif /* ALLOW_GGL90 */
#endif /* GGL90_OPTIONS_H */
