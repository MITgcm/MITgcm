#ifndef OBSFIT_OPTIONS_H
#define OBSFIT_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: OBSFIT_OPTIONS.h

C     !INTERFACE:
C     #include "OBSFIT_OPTIONS.h"

C     !DESCRIPTION:
C     ==================================================================
C     | CPP options file for pkg ObsFit:
C     | Control which optional features to compile in this package code
C     ==================================================================
CEOP

#ifdef ALLOW_OBSFIT
C Place CPP define/undef flag here

C To use file units between 9 and 99 (seems to conflict
C with NF_OPEN some times, but is needed when using g77)
# undef OBSFIT_USE_MDSFINDUNITS

#undef ALLOW_OBSFIT_EXCLUDE_CORNERS

#endif /* ALLOW_OBSFIT */
#endif /* OBSFIT_OPTIONS_H */
