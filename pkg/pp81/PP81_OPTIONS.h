C $Header: /u/gcmpack/MITgcm/pkg/pp81/PP81_OPTIONS.h,v 1.3 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | PP81_OPTIONS.h
C     | o CPP options file for PP81 package.
C     *==========================================================*
C     | Use this file for selecting options within the PP81
C     | package.
C     *==========================================================*

#ifndef PP81_OPTIONS_H
#define PP81_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_PP81
C     Package-specific Options & Macros go here

C o Allow lower bound for viscosity and diffusivity
#undef ALLOW_PP81_LOWERBOUND

C o When set, smooth Richardson number
#undef PP81_SMOOTH_RI

#endif /* ALLOW_PP81 */
#endif /* PP81_OPTIONS_H */
