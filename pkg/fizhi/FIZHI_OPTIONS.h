#ifndef FIZHI_OPTIONS_H
#define FIZHI_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_FIZHI
C     Package-specific options go here

C     use fixed day in the year:
#undef FIZHI_USE_FIXED_DAY

C     use new version of S/R GETPWHERE
#define TRY_NEW_GETPWHERE

C     Compiler and Processor specific code
#undef FIZHI_F77_COMPIL
#undef FIZHI_CRAY
#undef FIZHI_SGI

#endif /* ALLOW_FIZHI */
#endif /* FIZHI_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
