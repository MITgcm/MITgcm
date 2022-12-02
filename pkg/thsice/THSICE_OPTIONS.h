#ifndef THSICE_OPTIONS_H
#define THSICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_THSICE
C     Package-specific Options & Macros go here

C- use continuous power-law function for partition of energy between lateral
C  melting/freezing and thinning/thickening ; otherwise, use step function.
#define THSICE_FRACEN_POWERLAW

C- allow single grid-point debugging write to standard-output
#define ALLOW_DBUG_THSICE

C- only to check conservation
C  (change content of ICE_qleft,fresh,salFx-T files)
#undef CHECK_ENERGY_CONSERV

C- replace MIN/MAX by smooth functions, avoid divisions by zero and
C  sqrt of zero mostly to help AD code generation, changes results
#undef THSICE_REGULARIZE_CALC_THICKN

C CPP Macros go here

#endif /* ALLOW_THSICE */
#endif /* THSICE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
