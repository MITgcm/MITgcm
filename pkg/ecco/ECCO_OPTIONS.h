C $Header: /u/gcmpack/MITgcm/pkg/ecco/ECCO_OPTIONS.h,v 1.2 2014/08/15 09:27:03 atn Exp $
C $Name:  $

CBOP
C !ROUTINE: ECCO_OPTIONS.h
C !INTERFACE:
C #include "ECCO_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for ECCO (ecco) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef ECCO_OPTIONS_H
#define ECCO_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ECCO
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */
C   ==================================================================
C-- Package-specific Options & Macros go here

C-  fake options (only used to be printed in S/R ECCO_SUMMARY):
#define  ALLOW_ECCO_FORWARD_RUN
#undef  ALLOW_ECCO_DIAGNOSTIC_RUN
C       >>> Just do a "dry" run ( useful for testing ).
#undef  ALLOW_NO_DYNAMICS
C       >>> Use the Yearly-Monthly-Daily-Stepping call tree.
#undef  ALLOW_YMDS_TREE
C       >>> Do not call stepping
#define ALLOW_STEPPING_CALL
C       >>> Projection onto Spherical Harmonics
#undef  ALLOW_SPH_PROJECTION

C-  real options:

C o include dump of snap shots for checks
#undef ALLOW_SNAPSHOTS

cph >>>>>> !!!!!! SPECIAL SEAICE FLAG FOR TESTING !!!!!! <<<<<<
c#define SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
cph >>>>>> !!!!!! SPECIAL SEAICE FLAG FOR TESTING !!!!!! <<<<<<

#undef  ALLOW_ECCO_OPTIMIZATION

C       >>> Do a long protocol.
#undef ECCO_VERBOSE

C-- default turn off cost for sigmaR
#undef ALLOW_SIGMAR_COST_CONTRIBUTION

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_ECCO */
#endif /* ECCO_OPTIONS_H */
