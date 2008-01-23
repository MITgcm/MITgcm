C $Header: /u/gcmpack/MITgcm/verification/tutorial_advection_in_gyre/code/Attic/GAD_OPTIONS.h,v 1.1 2008/01/23 17:12:17 jahn Exp $
C $Name:  $

CBOP
C !ROUTINE: GAD_OPTIONS.h

C !INTERFACE:
C #include "GAD_OPTIONS.h" 

C !DESCRIPTION:
C Contains CPP macros/flags for controlling optional features of package.
CEOP

C CPP options file for GAD (Generic Advection Diffusion) package
C
C Use this file for selecting options within the GAD package

#ifndef GAD_OPTIONS_H
#define GAD_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_GENERIC_ADVDIFF

#include "CPP_OPTIONS.h"

C This flag selects the form of COSINE(lat) scaling of bi-harmonic term.
C *only for use on a lat-lon grid*
C Setting this flag here only affects the bi-harmonic tracer terms; to
C use COSINEMETH_III in the momentum equations set it CPP_OPTIONS.h
#define COSINEMETH_III

C This selects isotropic scaling of harmonic and bi-harmonic term when
C using the COSINE(lat) scaling.
C Setting this flag here only affects the tracer diffusion terms; to
C use ISOTROPIC_COS_SCALING of the horizontal viscosity terms in the 
C momentum equations set it CPP_OPTIONS.h; the following line
C even overrides setting the flag in CPP_OPTIONS.h
#undef ISOTROPIC_COS_SCALING

C As of checkpoint41, the inclusion of multi-dimensional advection
C introduces excessive recomputation/storage for the adjoint.
C We can disable it here using CPP because run-time flags are insufficient.
#undef DISABLE_MULTIDIM_ADVECTION

C This enable the use of 2nd-Order Moment advection scheme (Prather, 1986)
C due to large memory space (10 times more / tracer) requirement,
C by default, this part of the code is not compiled.
#define GAD_ALLOW_SOM_ADVECT

#else

C If GAD is disabled then so is multi-dimensional advection
#define DISABLE_MULTIDIM_ADVECTION

#endif /* ALLOW_GENERIC_ADVDIFF */
#endif /* GAD_OPTIONS_H */
