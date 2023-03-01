#ifndef OPENAD_OPTIONS_H
#define OPENAD_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: OPENAD_OPTIONS.h
C !INTERFACE:
C #include "OPENAD_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for OpenAD (openad) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_OPENAD

#undef ALLOW_OPENAD_ACTIVE_READ_XY
#undef ALLOW_OPENAD_ACTIVE_READ_XYZ
#undef ALLOW_OPENAD_ACTIVE_WRITE

#undef ALLOW_OPENAD_DIVA
#undef OAD_DEBUG

#endif /* ALLOW_OPENAD */
#endif /* OPENAD_OPTIONS_H */
