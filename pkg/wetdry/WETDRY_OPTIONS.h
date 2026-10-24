#ifndef WETDRY_OPTIONS_H
#define WETDRY_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: WETDRY_OPTIONS.h
C !INTERFACE:
C #include "WETDRY_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "wetdry" (wetting and drying):
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_WETDRY

C Print extra per-tile information on face flips and clipped cells
#undef WETDRY_DEBUG

C Smooth (tanh) face mask for adjoint differentiability (planned,
C  not implemented yet: wetdry_check stops if wdSmoothWidth > 0)
#undef WETDRY_SMOOTH_MASK

#endif /* ALLOW_WETDRY */
#endif /* WETDRY_OPTIONS_H */
