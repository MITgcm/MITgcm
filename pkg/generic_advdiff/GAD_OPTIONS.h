C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_OPTIONS.h,v 1.2 2001/09/19 20:45:09 adcroft Exp $
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
C
C GAD is enabled with ALLOW_GAD in CPP_OPTIONS.h

#include "CPP_OPTIONS.h"

ccc#ifdef ALLOW_GAD

C This turns on the flux limiter terms
#define GAD_USE_FLUX_LIMITER

C The selects the form of COSINE(lat) scaling of bi-harmonic term.
C *only for use on a lat-lon grid*
#define COSINEMETH_III

C The selects isotropic scaling of bi-harmonic term when
C using the COSINE(lat) scaling.
#undef  ISOTROPIC_COS_SCALING


ccc#endif /* ALLOW_GAD */
