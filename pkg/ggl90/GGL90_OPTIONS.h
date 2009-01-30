C $Header: /u/gcmpack/MITgcm/pkg/ggl90/GGL90_OPTIONS.h,v 1.4 2009/01/30 02:23:56 dfer Exp $
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

C     Enable horizontal diffusion of TKE. The orignal OPA code does not use
C     horizontal diffusion but uses some sort of horizontal averaging
C     for viscosity and diffusivity.
#undef ALLOW_GGL90_HORIZDIFF

#undef ALLOW_GGL90_SMOOTH

#endif /* ALLOW_GGL90 */
#endif /* GGL90_OPTIONS_H */

