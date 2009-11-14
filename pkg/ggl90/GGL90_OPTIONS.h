C $Header: /u/gcmpack/MITgcm/pkg/ggl90/GGL90_OPTIONS.h,v 1.5 2009/11/14 14:27:56 dfer Exp $
C $Name:  $
C     /=============================================================\
C     | GGL90_OPTIONS.h                                             |
C     | o CPP options file for GGL90 package.                       |
C     |=============================================================|
C     | Use this file for selecting options within the GGL90        |
C     | package. GGL90 is enabled with ALLOW_GGL90 in CPP_OPTIONS.h |
C     \=============================================================/

#ifndef GGL90_OPTIONS_H
#define GGL90_OPTIONS_H
#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_GGL90

#include "CPP_OPTIONS.h"

C     Enable horizontal diffusion of TKE.
#undef ALLOW_GGL90_HORIZDIFF

C     Use horizontal averaging for viscosity and diffusivity as 
C     originally implemented in OPA.
#undef ALLOW_GGL90_SMOOTH

#endif /* ALLOW_GGL90 */
#endif /* GGL90_OPTIONS_H */

