C $Header: /u/gcmpack/MITgcm/pkg/icefront/ICEFRONT_OPTIONS.h,v 1.4 2010/04/26 22:12:29 dimitri Exp $
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

C--   Allow code for specifying sub-glacial runoff.
C     Adds capability for time-evolving specification
C     of addmass array based on a 2D field, which is added
C     at bottommost wet level at each horizontal location.
#undef ALLOW_SUBGLACIAL_RUNOFF

#endif /* ALLOW_ICEFRONT */
#endif /* ICEFRONT_OPTIONS_H */
