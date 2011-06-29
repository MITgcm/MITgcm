C $Header: /u/gcmpack/MITgcm/pkg/shelfice/SHELFICE_OPTIONS.h,v 1.3 2011/06/29 16:24:10 heimbach Exp $
C $Name:  $

C     /==========================================================\
C     | SHELFICE_OPTIONS.h                                       |
C     | o CPP options file for SHELFICE package.                 |
C     |==========================================================|
C     | Use this file for selecting options within the SHELFICE  |
C     | package.                                                 |
C     \==========================================================/

#ifndef SHELFICE_OPTIONS_H
#define SHELFICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_SHELFICE

#include "CPP_OPTIONS.h"

C     allow code for simple ISOMIP thermodynamics
#define ALLOW_ISOMIP_TD

C     allow friction velocity-dependent transfer coefficient
C     following Holland and Jenkins, JPO, 1999
#define SHI_ALLOW_GAMMAFRICT

#endif /* ALLOW_SHELFICE */
#endif /* SHELFICE_OPTIONS_H */

