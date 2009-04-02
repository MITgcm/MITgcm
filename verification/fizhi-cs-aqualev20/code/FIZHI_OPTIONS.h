C $Header: /u/gcmpack/MITgcm/verification/fizhi-cs-aqualev20/code/FIZHI_OPTIONS.h,v 1.1 2009/04/02 00:04:06 jmc Exp $
C $Name:  $

#ifndef FIZHI_OPTIONS_H
#define FIZHI_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_FIZHI

#include "CPP_OPTIONS.h"

C     Package-specific options go here

C     #define FIZHI_DEBUG

C     use fixed day in the year:
#define FIZHI_USE_FIXED_DAY

C     try new version of S/R GETPWHERE
#undef TRY_NEW_GETPWHERE

#endif /* ALLOW_FIZHI */
#endif /* FIZHI_OPTIONS_H */


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
