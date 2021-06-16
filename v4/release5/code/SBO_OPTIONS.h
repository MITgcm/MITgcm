C     *==========================================================*
C     | SBO_OPTIONS.h
C     | o CPP options file for SBO package.
C     *==========================================================*
C     | Use this file for selecting options within the SBO
C     | package.
C     *==========================================================*

#ifndef SBO_OPTIONS_H
#define SBO_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_SBO
C     Package-specific Options & Macros go here
#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
#define EXTRA_OAM_USING_DYN_SEALEVEL
#endif
#endif


#endif /* ALLOW_SBO */
#endif /* SBO_OPTIONS_H */

