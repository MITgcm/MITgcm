C $Header: /u/gcmpack/MITgcm/pkg/pp81/PP81_OPTIONS.h,v 1.2 2007/10/09 00:13:15 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | PP81_OPTIONS.h                                            |
C     | o CPP options file for PP81 package.                      |
C     |==========================================================|
C     | Use this file for selecting options within the PP81       |
C     | package.  PP81 is enabled with ALLOW_PP81 in CPP_OPTIONS.h |
C     \==========================================================/

#ifndef PP81_OPTIONS_H
#define PP81_OPTIONS_H
#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_PP81

#include "CPP_OPTIONS.h"

C o Allow lower bound for viscosity and diffusivity
#undef ALLOW_PP81_LOWERBOUND

C o When set, smooth Richardson number
#undef PP81_SMOOTH_RI

#endif /* ALLOW_PP81 */
#endif /* PP81_OPTIONS_H */

