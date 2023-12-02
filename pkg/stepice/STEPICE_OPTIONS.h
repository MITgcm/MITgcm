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

C     allow Ian Fenty's shelfice/icefront merged code
#define shelfice_new_thermo

C     allow code for simple ISOMIP thermodynamics
#define ALLOW_ISOMIP_TD

C     allow friction velocity-dependent transfer coefficient
C     following Holland and Jenkins, JPO, 1999
#define SHI_ALLOW_GAMMAFRICT

C     in uStar expression, use wet-point method to average velocity
C     at grid-cell center
#undef SHI_USTAR_WETPOINT

CC use 3d shiTransCoeffT and shiTransCoeffS
C#define ALLOW_shiTransCoeff_3d

C     allow (vertical) remeshing whenever ocean top thickness factor
C     exceeds thresholds
#undef ALLOW_SHELFICE_REMESHING
C     and allow to print message to STDOUT when this happens
#define SHELFICE_REMESH_PRINT

#endif /* ALLOW_SHELFICE */
#endif /* SHELFICE_OPTIONS_H */
