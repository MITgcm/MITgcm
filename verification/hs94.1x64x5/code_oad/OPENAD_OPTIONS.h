C $Header: /u/gcmpack/MITgcm/verification/hs94.1x64x5/code_oad/OPENAD_OPTIONS.h,v 1.3 2015/02/22 23:50:22 heimbach Exp $
C $Name:  $

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

#ifndef OPENAD_OPTIONS_H
#define OPENAD_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OPENAD

#undef ALLOW_OPENAD_ACTIVE_READ_XYZ
#define ALLOW_OPENAD_ACTIVE_READ_XY
#define ALLOW_OPENAD_ACTIVE_WRITE

#endif /* ALLOW_OPENAD */
#endif /* OPENAD_OPTIONS_H */
