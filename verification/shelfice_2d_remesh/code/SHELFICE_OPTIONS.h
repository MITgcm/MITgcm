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
#undef ALLOW_ISOMIP_TD

C     allow friction velocity-dependent transfer coefficient
C     following Holland and Jenkins, JPO, 1999
#define SHI_ALLOW_GAMMAFRICT

C     Use old formulation of fresh water flux, derived from the salt
C     balance equation instead of the heat balance equation. Ill-defined
C     when all salinities (ice, ocean, boundary layer) are zero,
C     therefore deprecated.
#define SHI_SALTBAL_FWFLX

C     allow (vertical) remeshing whenever ocean top thickness factor
C     exceeds thresholds
#define ALLOW_SHELFICE_REMESHING
C     and allow to print message to STDOUT when this happens
#define SHELFICE_REMESH_PRINT

#endif /* ALLOW_SHELFICE */
#endif /* SHELFICE_OPTIONS_H */
