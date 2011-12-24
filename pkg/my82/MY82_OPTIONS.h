C $Header: /u/gcmpack/MITgcm/pkg/my82/MY82_OPTIONS.h,v 1.3 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

C     *===========================================================*
C     | MY82_OPTIONS.h
C     | o CPP options file for MY82 package.
C     *===========================================================*
C     | Use this file for selecting options within the MY82
C     | package.
C     *===========================================================*

#ifndef MY82_OPTIONS_H
#define MY82_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MY82
C     Package-specific Options & Macros go here

C o When set, smooth vertical diffusivity horizontally
#undef MY82_SMOOTH_RI

#endif /* ALLOW_MY82 */
#endif /* MY82_OPTIONS_H */
