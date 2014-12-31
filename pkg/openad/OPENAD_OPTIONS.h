C $Header: /u/gcmpack/MITgcm/pkg/openad/OPENAD_OPTIONS.h,v 1.2 2014/12/31 17:57:43 jmc Exp $
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

#undef ALLOW_OPENAD_ACTIVE_FILE

#endif /* ALLOW_OPENAD */
#endif /* OPENAD_OPTIONS_H */
