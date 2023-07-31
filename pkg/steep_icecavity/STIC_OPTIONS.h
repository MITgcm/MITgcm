C     *==========================================================*
C     | STIC_OPTIONS.h
C     | o CPP options file for STEEP_ICECAVITY package.
C     *==========================================================*
C     | Use this file for selecting options within
C     | the STEEP_ICECAVITY package.
C     *==========================================================*

#ifndef STIC_OPTIONS_H
#define STIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_STEEP_ICECAVITY
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
#undef ALLOW_STIC_REMESHING
C     and allow to print message to STDOUT when this happens
#define STIC_REMESH_PRINT

#endif /* ALLOW_STEEP_ICECAVITY */
#endif /* STIC_OPTIONS_H */
