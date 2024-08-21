C CPP options file for mom_common package
C Use this file for selecting CPP options within the mom_common package

#ifndef MOM_COMMON_OPTIONS_H
#define MOM_COMMON_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MOM_COMMON
C     Package-specific options go here

C allow LeithQG coefficient to be calculated
#undef ALLOW_LEITH_QG

C allow isotropic 3-D Smagorinsky viscosity
#undef ALLOW_SMAG_3D

C allow full 3D specification of horizontal Laplacian Viscosity
#undef ALLOW_3D_VISCAH

C allow full 3D specification of horizontal Biharmonic Viscosity
#undef ALLOW_3D_VISCA4

C Compute bottom drag coefficents, following the logarithmic law of the wall,
C as a function of grid cell thickness and roughness length
C zRoughBot (order 0.01m), assuming a von Karman constant = 0.4.
#undef ALLOW_BOTTOMDRAG_ROUGHNESS

C Compute extra momentum tendency diagnostics for bottom and ice-shelf drag.
C The code does not work with using implicit bottom or ice-shelf
C drag (selectImplicitDrag = 2). For non-r* vertical coordinate, these
C tendency diagnostics can be derived from existing diagnostics for
C frictional stress (botTauX etc.) and therefore are not necessarily needed.
#undef ALLOW_MOM_TEND_EXTRA_DIAGS

#endif /* ALLOW_MOM_COMMON */
#endif /* MOM_COMMON_OPTIONS_H */
