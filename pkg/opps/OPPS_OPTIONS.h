C     *==========================================================*
C     | OPPS_OPTIONS.h
C     | o CPP options file for OPPS package.
C     *==========================================================*
C     | Use this file for selecting options within the OPPS
C     | package.
C     *==========================================================*

#ifndef OPPS_OPTIONS_H
#define OPPS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OPPS
C Package-specific Options & Macros go here

C allow snap-shot OPPS output
#undef ALLOW_OPPS_SNAPSHOT

C allow debugging OPPS_CALC
#define ALLOW_OPPS_DEBUG

#endif /* ALLOW_OPPS */
#endif /* OPPS_OPTIONS_H */
