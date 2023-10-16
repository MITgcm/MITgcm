C CPP options file for STREAMICE
C Use this file for selecting options within package "streamice"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef STREAMICE_OPTIONS_H
#define STREAMICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_STREAMICE

C Place CPP define/undef flag here

#define STREAMICE_CONSTRUCT_MATRIX
#define STREAMICE_HYBRID_STRESS
#undef STREAMICE_FLOWLINE_BUTTRESS
#define USE_ALT_RLOW
#undef STREAMICE_GEOM_FILE_SETUP
C   The following will taper basal stress in a cell based
C   on height above floatation, and option (2) will also
C   smooth surface elevation across grounding line;
C   only one should be defined
#define STREAMICE_SMOOTH_FLOATATION
#undef STREAMICE_SMOOTH_FLOATATION2

#undef ALLOW_PETSC
#undef ALLOW_STREAMICE_2DTRACER
#undef STREAMICE_TRACER_AB
#undef STREAMICE_SERIAL_TRISOLVE

#endif /* ALLOW_STREAMICE */
#endif /* STREAMICE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
