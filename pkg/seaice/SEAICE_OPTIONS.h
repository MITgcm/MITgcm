C $Header:

C     /==========================================================\
C     | SEAICE_OPTIONS.h                                         |
C     | o CPP options file for sea ice package.                  |
C     |==========================================================|
C     | Use this file for selecting options within the sea ice   |
C     | package.  Sea ice model is enabled with ALLOW_SEAICE in  |
C     | CPP_OPTIONS.h                                            |
C     \==========================================================/

#include "CPP_OPTIONS.h"

#ifdef ALLOW_SEAICE

C--    Write "text-plots" of certain fields in STDOUT for debugging.
#undef SEAICE_DEBUG

C--    Allow sea-ice dynamic code.
C      This option is provided to allow us to use the TAMC
C      on the thermodynamics component of the code only.
C      Sea-ice dynamics can also be turned off at runtime
C      using variable SEAICEuseDYNAMICS.
#define SEAICE_ALLOW_DYNAMICS

#endif ALLOW_SEAICE
