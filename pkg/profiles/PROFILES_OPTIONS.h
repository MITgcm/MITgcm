C $Header: /u/gcmpack/MITgcm/pkg/profiles/PROFILES_OPTIONS.h,v 1.8 2015/08/16 14:48:34 gforget Exp $
C $Name:  $

C CPP options file for PROFILES package
C Use this file for selecting options within the PROFILES package

#ifndef PROFILES_OPTIONS_H
#define PROFILES_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_PROFILES
C     Package-specific Options & Macros go here

C To use file units between 9 and 99 (seems to conflict
C with NF_OPEN some times, but is needed when using g77)
# undef PROFILES_USE_MDSFINDUNITS

#endif /* ALLOW_PROFILES */
#endif /* PROFILES_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
