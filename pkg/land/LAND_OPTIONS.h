C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_OPTIONS.h,v 1.3 2004/03/11 14:41:59 jmc Exp $
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
#undef LAND_OLD_VERSION

#endif /* ALLOW_LAND */
#endif /* LAND_OPTIONS_H */
