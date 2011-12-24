C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_OPTIONS.h,v 1.6 2011/12/24 01:04:48 jmc Exp $
C $Name:  $

C  CPP options file for Land package

#ifndef LAND_OPTIONS_H
#define LAND_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_LAND
C     Package-specific Options & Macros go here

#ifdef ALLOW_TIMEAVE
C  allow time average diagnostic:
# define ALLOW_LAND_TAVE
#endif

C  to write debugging diagnostics:
#undef LAND_DEBUG

C  to reproduce results from version.1 (not conserving heat)
#undef LAND_OLD_VERSION

#endif /* ALLOW_LAND */
#endif /* LAND_OPTIONS_H */
