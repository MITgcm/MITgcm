C $Header: /u/gcmpack/MITgcm/pkg/mom_common/MOM_COMMON_OPTIONS.h,v 1.2 2011/03/31 20:33:58 jmc Exp $
C $Name:  $

C CPP options file for mom_common package
C
C Use this file for selecting CPP options within the mom_common package

#ifndef MOM_COMMON_OPTIONS_H
#define MOM_COMMON_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_MOM_COMMON

#include "CPP_OPTIONS.h"

C allow full 3D specification of horizontal Laplacian Viscosity
#undef ALLOW_3D_VISCAH

C allow full 3D specification of horizontal Biharmonic Viscosity
#undef ALLOW_3D_VISCA4

C CPP macros go here

#endif /* ALLOW_MOM_COMMON */
#endif /* MOM_COMMON_OPTIONS_H */
