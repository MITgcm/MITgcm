C $Header: /u/gcmpack/MITgcm/verification/cpl_aim+ocn/code_atm/Attic/LAND_OPTIONS.h,v 1.1 2004/04/18 15:43:53 jmc Exp $
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
