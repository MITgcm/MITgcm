C $Header: /u/gcmpack/MITgcm/pkg/runclock/RUNCLOCK_OPTIONS.h,v 1.1 2005/05/31 18:24:32 adcroft Exp $
C $Name:  $

C CPP options file for RUNCLOCK package
C
C Use this file for selecting options within the RUNCLOCK package

#ifndef RUNCLOCK_OPTIONS_H
#define RUNCLOCK_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_RUNCLOCK

#include "CPP_OPTIONS.h"

C Put RUNCLOCK specific CPP macros here

C Define this macro if using an F90-compiler to compile and link the code
#undef  RUNCLOCK_USES_DATE_AND_TIME

#endif /* ALLOW_RUNCLOCK */
#endif /* RUNCLOCK_OPTIONS_H */
