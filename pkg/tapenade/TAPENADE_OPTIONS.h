CBOP
C !ROUTINE: TAPENADE_OPTIONS.h
C !INTERFACE:
C #include "TAPENADE_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Tapenade (tapenade) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef TAPENADE_OPTIONS_H
#define TAPENADE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_TAPENADE

#define ALLOW_TAPENADE_ACTIVE_READ_XYZ
#define ALLOW_TAPENADE_ACTIVE_READ_XY
#undef ALLOW_TAPENADE_ACTIVE_WRITE

#endif /* ALLOW_TAPENADE */
#endif /* TAPENADE_OPTIONS_H */
