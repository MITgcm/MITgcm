C CPP options file for mom_common package
C Use this file for selecting CPP options within the mom_common package

#ifndef MOM_COMMON_OPTIONS_H
#define MOM_COMMON_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MOM_COMMON
C     Package-specific options go here

C This flag selects the form of COSINE(lat) scaling of horizontal
C bi-harmonic viscosity -- only on lat-lon grid.
C Setting this flag here only affects momentum viscosity; to use it
C in the tracer equations it needs to be set in GAD_OPTIONS.h
#define COSINEMETH_III

C This selects isotropic scaling of horizontal harmonic and bi-harmonic
C viscosity when using the COSINE(lat) scaling -- only on lat-lon grid.
C Setting this flag here only affects momentum viscosity; to use it
C in the tracer equations it needs to be set in GAD_OPTIONS.h
#undef ISOTROPIC_COS_SCALING

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

C-- Deep-Model: use the original vertical advection (with all NH metric terms)
C   instead of updated version that advects the product deepFac x (u,v)
C   thus removing the need for NH-metric terms: w.(u,v)/r
#undef MOM_USE_OLD_DEEP_VERT_ADV

C Compute extra momentum tendency diagnostics for bottom and ice-shelf drag.
C The code does not work with using implicit bottom or ice-shelf
C drag (selectImplicitDrag = 2). For non-r* vertical coordinate, these
C tendency diagnostics can be derived from existing diagnostics for
C frictional stress (botTauX etc.) and therefore are not necessarily needed.
#undef ALLOW_MOM_TEND_EXTRA_DIAGS

#endif /* ALLOW_MOM_COMMON */
#endif /* MOM_COMMON_OPTIONS_H */
