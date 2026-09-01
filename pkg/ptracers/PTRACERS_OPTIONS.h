! CPP options file for PTRACERS package
! Use this file for selecting options within the PTRACERS package

#ifndef PTRACERS_OPTIONS_H
#define PTRACERS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_PTRACERS
! Package-specific Options & Macros go here

! NUMBER_OF_PTRACERS defines how many passive tracers are allocated/exist.
! This CPP macro is *only* used in PTRACERS.h to set an integer parameter.
! <Please> do not make use of it elsewhere.
!   Note: this CPP macro has been removed to avoid confusion and risk of
!    error resulting from multiple definitions (default + explicit) within
!    the code.
!    The maximum number of tracers is now defined within PTRACERS_SIZE.h
!---

! This enables the dynamically allocated internal state data structures
! for PTracers.  Needed for PTRACERS_SOM_Advection.
! This requires a Fortran 90 compiler!
#undef  PTRACERS_ALLOW_DYN_STATE

#endif /* ALLOW_PTRACERS */
#endif /* PTRACERS_OPTIONS_H */

!EH3 ;;; Local Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
