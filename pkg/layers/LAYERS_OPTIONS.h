C $Header: /u/gcmpack/MITgcm/pkg/layers/LAYERS_OPTIONS.h,v 1.4 2012/09/19 18:48:18 gforget Exp $
C $Name:  $

C CPP options file for LAYERS package
C Use this file for selecting options within package "LAYERS"

#ifndef LAYERS_OPTIONS_H
#define LAYERS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_LAYERS
C     Package-specific Options & Macros go here

C Compute isopycnal tranports in the U direction?
#define LAYERS_UFLUX
C Compute isopycnal tranports in the V direction?
#define LAYERS_VFLUX
C Keep track of layer thicknesses?
#define LAYERS_THICKNESS

C The MNC stuff is too complicated
#undef LAYERS_MNC

C To allow use for snapshots and time average without 
C pkg/diagnostics, using pkg/layers own codes and timeave
C the old-fasioned way, only for the first set of layers.
#undef ALLOW_LAYERS_OUTPUT

C Allow use of potential density as a layering field.
#define LAYERS_PRHO_REF

#endif /* ALLOW_LAYERS */
#endif /* LAYERS_OPTIONS_H */
