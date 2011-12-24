C $Header: /u/gcmpack/MITgcm/pkg/ptracers/PTRACERS_OPTIONS.h,v 1.8 2011/12/24 01:09:40 jmc Exp $
C $Name:  $

C CPP options file for PTRACERS package
C Use this file for selecting options within the PTRACERS package

#ifndef PTRACERS_OPTIONS_H
#define PTRACERS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_PTRACERS
C     Package-specific Options & Macros go here

C NUMBER_OF_PTRACERS defines how many passive tracers are allocated/exist.
C This CPP macro is *only* used in PTRACERS.h to set an integer parameter.
C <Please> do not make use of it elsewhere.
C   Note: this CPP macro has been removed to avoid confusion and risk of
C    error resulting from multiple definitions (default + explicit) within
C    the code.
C    The maximum number of tracers is now defined within PTRACERS_SIZE.h
C---

C     This enables the dynamically allocated internal state data structures
C     for PTracers.  Needed for PTRACERS_SOM_Advection.
C     This requires a Fortran 90 compiler!
#undef  PTRACERS_ALLOW_DYN_STATE

#endif /* ALLOW_PTRACERS */
#endif /* PTRACERS_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
