C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/Attic/LAND_OPTIONS.h,v 1.1 2004/03/11 14:45:28 jmc Exp $
C $Name:  $

C  CPP options file for Land package 

#ifndef LAND_OPTIONS_H
#define LAND_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_LAND

#include "CPP_OPTIONS.h"

C  allow time average diagnostic:
#define ALLOW_LAND_TAVE

C  to reproduce results from version.1 (not conserving heat)
#define LAND_OLD_VERSION

#endif /* ALLOW_LAND */
#endif /* LAND_OPTIONS_H */
