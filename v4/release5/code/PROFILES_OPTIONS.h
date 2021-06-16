C $Header: /u/gcmpack/MITgcm_contrib/ecco_utils/ecco_v4_release3_devel/code/PROFILES_OPTIONS.h,v 1.1 2017/05/04 17:46:37 ou.wang Exp $
C $Name:  $

C CPP options file for PROFILES package
C Use this file for selecting options within the PROFILES package

#ifndef PROFILES_OPTIONS_H
#define PROFILES_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_PROFILES
C     Package-specific Options & Macros go here

#define ALLOW_PROFILES_SAMPLESPLIT_COST
#define ALLOW_PROFILES_CLIMMASK

C To use file units between 9 and 99 (seems to conflict
C with NF_OPEN some times, but is needed when using g77)
# undef PROFILES_USE_MDSFINDUNITS

#endif /* ALLOW_PROFILES */
#endif /* PROFILES_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
