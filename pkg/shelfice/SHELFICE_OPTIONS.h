C     /==========================================================\
C     | SHELFICE_OPTIONS.h                                       |
C     | o CPP options file for SHELFICE package.                 |
C     |==========================================================|
C     | Use this file for selecting options within the SHELFICE  |
C     | package.                                                 |
C     \==========================================================/

#ifndef SHELFICE_OPTIONS_H
#define SHELFICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_SHELFICE

#include "CPP_OPTIONS.h"

C     allow code for simple ISOMIP thermodynamics
#define ALLOW_ISOMIP_TD

#endif /* ALLOW_SHELFICE */
#endif /* SHELFICE_OPTIONS_H */

