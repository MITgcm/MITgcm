C $Header: /u/gcmpack/MITgcm/verification/carbon/code_ad_ptracers/Attic/PTRACERS_OPTIONS.h,v 1.1 2003/10/26 01:45:05 heimbach Exp $
C $Name:  $

C CPP options file for PTRACERS package
C
C Use this file for selecting options within the PTRACERS package
C
C PTRACERS is enabled with ALLOW_PTRACERS in CPP_OPTIONS.h or in
C the Makefile with DEFINES=-DALLOW_PTRACERS

#include "CPP_OPTIONS.h"

#ifdef ALLOW_PTRACERS

C NUMBER_OF_PTRACERS defines how many passive tracers are allocated/exist.
C This CPP macro is *only* used in PTRACERS.h to set an integer parameter.
C <Please> do not make use of it elsewhere.
C
C NUMBER_OF_PTRACERS can be defined in CPP_OPTIONS.h, or in the Makefile
C with DEFINES=-DNUMBER_OF_PTRACERS=4
C
C If NUMBER_OF_PTRACERS is not specified elsewhere and ALLOW_PTRACERS
C is set then NUMBER_OF_PTRACERS is set here (default 1)
#ifndef NUMBER_OF_PTRACERS
#define NUMBER_OF_PTRACERS 1
#endif

#endif /* ALLOW_PTRACERS */
