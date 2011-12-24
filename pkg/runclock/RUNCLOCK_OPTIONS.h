C $Header: /u/gcmpack/MITgcm/pkg/runclock/RUNCLOCK_OPTIONS.h,v 1.2 2011/12/24 01:09:40 jmc Exp $
C $Name:  $

C CPP options file for RUNCLOCK package
C Use this file for selecting options within the RUNCLOCK package

#ifndef RUNCLOCK_OPTIONS_H
#define RUNCLOCK_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_RUNCLOCK
C     Package-specific Options & Macros go here

C Define this macro if using an F90-compiler to compile and link the code
#undef  RUNCLOCK_USES_DATE_AND_TIME

#endif /* ALLOW_RUNCLOCK */
#endif /* RUNCLOCK_OPTIONS_H */
