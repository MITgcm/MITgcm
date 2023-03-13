#ifndef REGRID_OPTIONS_H
#define REGRID_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: REGRID_OPTIONS.h
C !INTERFACE:
C #include "REGRID_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "regrid":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_REGRID
C     Place CPP define/undef flags here

C     Currently, there are no compile-time options for the REGRID package.

C     This CPP is not set (neither def nor undef) in CPP_EEMACROS.h
C     so it is set (to undef) here
#undef RL_IS_REAL4

#endif /* ALLOW_REGRID */
#endif /* REGRID_OPTIONS_H */
