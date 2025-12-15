#ifndef STREAMICE_OPTIONS_H
#define STREAMICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: STREAMICE_OPTIONS.h
C !INTERFACE:
C #include "STREAMICE_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "streamice":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_STREAMICE
C-- Place CPP define/undef flag here

#define STREAMICE_CONSTRUCT_MATRIX
#undef STREAMICE_HYBRID_STRESS
#undef STREAMICE_FLOWLINE_BUTTRESS
#undef USE_ALT_RLOW
#undef STREAMICE_GEOM_FILE_SETUP
C   The following will taper basal stress in a cell based
C   on height above floatation, and option (2) will also
C   smooth surface elevation across grounding line;
C   only one should be defined
#undef STREAMICE_SMOOTH_FLOATATION
#undef STREAMICE_SMOOTH_FLOATATION2

#undef ALLOW_PETSC
#undef ALLOW_STREAMICE_2DTRACER
#undef STREAMICE_TRACER_AB
#undef STREAMICE_SERIAL_TRISOLVE

C-  Undocumented Options:
#undef STREAMICE_3D_GLEN_CONST
#undef STREAMICE_COULOMB_SLIDING
#undef STREAMICE_ECSECRYO_DOSUM
#undef STREAMICE_FALSE
#undef STREAMICE_FIRN_CORRECTION
#undef STREAMICE_PETSC_3_8
#undef STREAMICE_STRESS_BOUNDARY_CONTROL
#undef ALLOW_STREAMICE_TIMEDEP_FORCING
#undef ALLOW_STREAMICE_FLUX_CONTROL
#undef ALLOW_STREAMICE_TC_COST

C-- for OpenAD or Tapenade:
C   Fixed-Point problem treatment for adjoint computation as described in
C   Christianson et al 1994, Opt. Meth. & Software ;
C   this reduce size of adjoint tape in memory as well as decouple forward
C   and reverse convergence criteria
#undef ALLOW_STREAMICE_FP_ADJ

#endif /* ALLOW_STREAMICE */
#endif /* STREAMICE_OPTIONS_H */
