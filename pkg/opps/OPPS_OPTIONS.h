C $Header: /u/gcmpack/MITgcm/pkg/opps/OPPS_OPTIONS.h,v 1.2 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

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

C allow debugging OPPS_CALC
#define ALLOW_OPPS_DEBUG

#endif /* ALLOW_OPPS */
#endif /* OPPS_OPTIONS_H */
