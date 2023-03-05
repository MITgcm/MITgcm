#ifndef CHEAPAML_OPTIONS_H
#define CHEAPAML_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: CHEAPAML_OPTIONS.h
C !INTERFACE:
C #include "CHEAPAML_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for CheapAML package
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_CHEAPAML
C Place CPP define/undef flag here

C to reproduce old results, with inconsistent wind location,
C  grid-cell center and grid-cell edges (C-grid).
#undef INCONSISTENT_WIND_LOCATION

#endif /* ALLOW_CHEAPAML */
#endif /* CHEAPAML_OPTIONS_H */
