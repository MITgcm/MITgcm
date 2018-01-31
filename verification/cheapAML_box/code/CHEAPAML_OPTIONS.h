C Options file for CheapAML package
C Use this file for selecting options within the CheapAML package

#ifndef CHEAPAML_OPTIONS_H
#define CHEAPAML_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_CHEAPAML
C Place CPP define/undef flag here

C to reproduce old results, with inconsitent wind location,
C  grid-cell center and grid-cell edges (C-grid).
#define INCONSISTENT_WIND_LOCATION

#endif /* ALLOW_CHEAPAML */
#endif /* CHEAPAML_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
