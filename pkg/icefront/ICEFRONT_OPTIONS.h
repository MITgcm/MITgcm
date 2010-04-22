C $Header: /u/gcmpack/MITgcm/pkg/icefront/ICEFRONT_OPTIONS.h,v 1.3 2010/04/22 18:24:44 yunx Exp $
C $Name:  $

C     /==========================================================\
C     | ICEFRONT_OPTIONS.h                                       |
C     | o CPP options file for ICEFRONT package.                 |
C     |==========================================================|
C     | Use this file for selecting options within the ICEFRONT  |
C     | package.                                                 |
C     \==========================================================/

#ifndef ICEFRONT_OPTIONS_H
#define ICEFRONT_OPTIONS_H
#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_ICEFRONT

#include "CPP_OPTIONS.h"

#undef ALLOW_SUBGLACIAL_RUNOFF

#endif /* ALLOW_ICEFRONT */
#endif /* ICEFRONT_OPTIONS_H */

