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

C     Use a formulation of fresh water flux, derived from the heat
C     balance equation instead of the salt balance equation, that can
C     handle the case when the salinity of the ocean, boundary layer,
C     and ice are identical.
#define SHELFICE_USE_HEATBALANCE_FOR_FRESHWATERFLUX

C     allow (vertical) remeshing whenever ocean top thickness factor
C     exceeds thresholds
#undef ALLOW_SHELFICE_REMESHING
C     and allow to print message to STDOUT when this happens
#define SHELFICE_REMESH_PRINT

#endif /* ALLOW_SHELFICE */
#endif /* SHELFICE_OPTIONS_H */
