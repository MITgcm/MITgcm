C CPP options file for OBSFIT package
C Use this file for selecting options within the OBSFIT package

#ifndef OBSFIT_OPTIONS_H
#define OBSFIT_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OBSFIT
C     Package-specific Options & Macros go here

C To use file units between 9 and 99 (seems to conflict
C with NF_OPEN some times, but is needed when using g77)
# undef OBSFIT_USE_MDSFINDUNITS

#endif /* ALLOW_OBSFIT */
#endif /* OBSFIT_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
