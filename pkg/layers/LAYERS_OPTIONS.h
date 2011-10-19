C $Header: /u/gcmpack/MITgcm/pkg/layers/LAYERS_OPTIONS.h,v 1.2 2011/10/19 01:28:45 dfer Exp $
C $Name:  $

C
C Use this file for selecting options within package "LAYERS"

#ifndef LAYERS_OPTIONS_H
#define LAYERS_OPTIONS_H

#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_LAYERS

#include "CPP_OPTIONS.h"

C Compute isopycnal tranports in the U direction?
#define LAYERS_UFLUX
C Compute isopycnal tranports in the V direction?
#define LAYERS_VFLUX
C Keep track of layer thicknesses?
#define LAYERS_THICKNESS

C The MNC stuff is too complicated
#undef LAYERS_MNC

C Allow use of potential density as a layering field.
#define LAYERS_PRHO_REF

#endif /* ALLOW_LAYERS */
#endif /* LAYERS_OPTIONS_H */

