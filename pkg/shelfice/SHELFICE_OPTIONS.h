C $Header: /u/gcmpack/MITgcm/pkg/shelfice/SHELFICE_OPTIONS.h,v 1.5 2015/12/17 01:52:05 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | SHELFICE_OPTIONS.h
C     | o CPP options file for SHELFICE package.
C     *==========================================================*
C     | Use this file for selecting options within the SHELFICE
C     | package.
C     *==========================================================*

#ifndef SHELFICE_OPTIONS_H
#define SHELFICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_SHELFICE
C     Package-specific Options & Macros go here

C     allow code for simple ISOMIP thermodynamics
#define ALLOW_ISOMIP_TD

C     allow friction velocity-dependent transfer coefficient
C     following Holland and Jenkins, JPO, 1999
#define SHI_ALLOW_GAMMAFRICT
C     in uStar expression, use wet-point method to average velocity
C     at grid-cell center
#undef SHI_USTAR_WETPOINT

#endif /* ALLOW_SHELFICE */
#endif /* SHELFICE_OPTIONS_H */
