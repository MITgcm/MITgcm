#ifndef FIZHI_OPTIONS_H
#define FIZHI_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: FIZHI_OPTIONS.h
C !INTERFACE:
C #include "FIZHI_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "fizhi":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_FIZHI
C     Package-specific CPP options go here

C     use fixed day in the year:
#undef FIZHI_USE_FIXED_DAY

C     use new version of S/R GETPWHERE
#define TRY_NEW_GETPWHERE

C     Compiler and Processor specific code
#undef FIZHI_F77_COMPIL
#undef FIZHI_CRAY
#undef FIZHI_SGI

C     Bring back original/old bug in S/R TRBFLX
#undef FIZHI_TRBFLX_OLD_BUG

#endif /* ALLOW_FIZHI */
#endif /* FIZHI_OPTIONS_H */
