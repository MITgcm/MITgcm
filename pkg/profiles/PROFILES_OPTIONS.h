C CPP options file for PROFILES package
C Use this file for selecting options within the PROFILES package

#ifndef PROFILES_OPTIONS_H
#define PROFILES_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_PROFILES
C     Package-specific Options & Macros go here

C To use file units between 9 and 999 (seems to conflict
C with NF_OPEN some times, but is needed when using g77)
C Note: comment out the #define below (instead of having an #undef) to
C       enable to set this Option in CPP command line (from the optfile)
c#define PROFILES_USE_MDSFINDUNITS

#endif /* ALLOW_PROFILES */
#endif /* PROFILES_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
