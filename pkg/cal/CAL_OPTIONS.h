C $Header: /u/gcmpack/MITgcm/pkg/cal/CAL_OPTIONS.h,v 1.5 2012/08/08 20:11:32 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: CAL_OPTIONS.h
C !INTERFACE:
C #include "CAL_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Calendar (cal) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef CAL_OPTIONS_H
#define CAL_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_CAL
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */

C-- Package-specific Options & Macros go here

#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_CAL */
#endif /* CAL_OPTIONS_H */
