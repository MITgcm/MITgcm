C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_OPTIONS.h,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

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
