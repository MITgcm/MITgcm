
C     *==========================================================*
C     | ICEPLUME_OPTIONS.h
C     | o CPP options file for ICEFRONT package.
C     *==========================================================*
C     | Use this file for selecting options within the ICEFRONT
C     | package.
C     *==========================================================*

#ifndef ICEPLUME_OPTIONS_H
#define ICEPLUME_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ICEPLUME
C     Package-specific Options & Macros go here

C     ALLOW FOR DETACHED PLUME - requires different array sizes
#undef ICEPLUME_ALLOW_DETACHED_PLUME

#endif /* ALLOW_ICEPLUME */
#endif /* ICEPLUME_OPTIONS_H */
