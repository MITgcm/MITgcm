#ifndef STIC_OPTIONS_H
#define STIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: STIC_OPTIONS.h
C !INTERFACE:
C #include "STIC_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "steep_icecavity":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_STEEP_ICECAVITY
C Place CPP define/undef flag here

C     use 3D version of transfer coefficients (needed for variable
C     transfer coefficients)
#define ALLOW_SHITRANSCOEFF_3D

#endif /* ALLOW_STEEP_ICECAVITY */
#endif /* STIC_OPTIONS_H */
