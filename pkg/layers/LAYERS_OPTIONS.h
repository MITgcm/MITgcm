#ifndef LAYERS_OPTIONS_H
#define LAYERS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: LAYERS_OPTIONS.h
C !INTERFACE:
C #include "LAYERS_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "layers":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_LAYERS
C---- Package-specific Options & Macros go here

C-  Allow to use potential density as layer coordinate
C   (layers defined as between isopycnal surfaces)
#define LAYERS_PRHO_REF

C-  Allow to use Moist Static Energy as layer coordinate (for atmospheric setup)
#undef LAYERS_MSE

C-- To map horizontal transport to layers (isothermal, isohaline, isopycnal):
C   allow to map transport in U direction:
#define LAYERS_UFLUX
C   allow to map transport in V direction:
#define LAYERS_VFLUX
C   and allow to keep track of layer thickness and probability/occurence
#define LAYERS_THICKNESS

C-- Enable water mass transformation budget:
#undef LAYERS_THERMODYNAMICS
C   using refined grid for diapycnal terms (gives worse results)
#undef LAYERS_FINEGRID_DIAPYCNAL

C-  The MNC stuff is too complicated
#undef LAYERS_MNC

#endif /* ALLOW_LAYERS */
#endif /* LAYERS_OPTIONS_H */
