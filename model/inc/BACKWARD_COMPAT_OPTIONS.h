#ifndef BACKWARD_COMPAT_OPTIONS_H
#define BACKWARD_COMPAT_OPTIONS_H

CBOP
C !ROUTINE: BACKWARD_COMPAT_OPTIONS.h
C !INTERFACE:
C #include "BACKWARD_COMPAT_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | Set backward compatibility CPP options for model src code.
C | Just for convenience, to be included in few relevant src files
C | thus avoiding to recompile the full model+pkgs code if updated
C *==================================================================*
CEOP

C Backward compatibility CPP flags

C Back to original CG matrix units and, except for the case
C  selectNHfreeSurf =1, preserve pre checkpoint69c results (at machine
C truncation level). Note: new feature solveForDeltaP=T cannot be used
C with this option defined.
#undef PRESERVE_C69C_OUTPUT

#endif /* BACKWARD_COMPAT_OPTIONS_H */
