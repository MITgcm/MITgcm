#ifndef ECCO_OPTIONS_H
#define ECCO_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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

#ifdef ALLOW_ECCO
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */

C-- Package-specific Options & Macros go here

C       >>> ALLOW_GENCOST_CONTRIBUTION: interactive way to add basic 2D cost
C         function terms.
C       > In data.ecco, this requires the specification of data file (name,
C         frequency, etc.), bar file name for corresp. model average, standard
C         error file name, etc.
C       > In addition, adding such cost terms requires editing ecco_cost.h to
C         increase NGENCOST, and editing cost_gencost_customize.F to implement
C         the actual model average (i.e. the bar file content).
c #undef ALLOW_GENCOST_CONTRIBUTION
C       >>> free form version of GENCOST: allows one to use otherwise defined
C         elements (e.g. psbar and and topex data) while taking advantage of the
C         cost function/namelist slots that can be made available using
C         ALLOW_GENCOST_CONTRIBUTION. To this end ALLOW_GENCOST_CONTRIBUTION
C         simply switches off tests that check whether all of the gencost
C         elements (e.g. gencost_barfile and gencost_datafile) are specified
C         in data.ecco.
C       > While this option increases flexibility within the gencost framework,
C         it implies more room for error, so it should be used cautiously, and
C         with good knowledge of the rest of pkg/ecco.
C       > It requires providing a specific cost function routine, and editing
C         cost_gencost_all.F accordingly.
c #undef ALLOW_GENCOST_FREEFORM

C o Allow for generic cost function and integral terms
#define ALLOW_GENCOST_CONTRIBUTION
C o Allow for 3 dimensional generic cost terms
#define ALLOW_GENCOST3D

C o Allow Open-Boundary cost contributions
#ifdef ALLOW_OBCS
C   Open-Boundary cost is meaningless without compiling pkg/obcs
C   Note: Make sure that coresponding OBCS N/S/W/E Option is defined
# define ALLOW_OBCSN_COST_CONTRIBUTION
# define ALLOW_OBCSS_COST_CONTRIBUTION
# define ALLOW_OBCSW_COST_CONTRIBUTION
# define ALLOW_OBCSE_COST_CONTRIBUTION
#endif /* ALLOW_OBCS */

C o Set ALLOW_OBCS_COST_CONTRIBUTION (Do not edit/modify):
#if (defined (ALLOW_OBCSN_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSS_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSW_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSE_COST_CONTRIBUTION))
# define ALLOW_OBCS_COST_CONTRIBUTION
#endif

C o Include global mean steric sea level correction
#undef ALLOW_PSBAR_STERIC
#ifdef ATMOSPHERIC_LOADING
C   Apply inverse barometer correction
#define ALLOW_IB_CORR
#endif
C o Allow for near-shore and high-latitude altimetry
#undef ALLOW_SHALLOW_ALTIMETRY
#undef ALLOW_HIGHLAT_ALTIMETRY

C o Allow for In-Situ Profiles cost function contribution
#undef ALLOW_PROFILES_CONTRIBUTION

C o Allow cost function term for sigmaR
#undef ALLOW_SIGMAR_COST_CONTRIBUTION

C o Cost function output format
#undef ALLOW_ECCO_OLD_FC_PRINT

C-- real options?

C o Include dump of snap shots for checks
#undef ALLOW_SNAPSHOTS

C o Generate more text in STDOUT.0000
#undef ECCO_VERBOSE

C--  fake options (only used to be printed in S/R ECCO_SUMMARY):

C allow ???               (no corresponding code -> commented out)
c#define ALLOW_ECCO_FORWARD_RUN
c#undef  ALLOW_ECCO_DIAGNOSTIC_RUN
C o Just do a "dry" run ( useful for testing )(no code -> commented out)
c#undef  ALLOW_NO_DYNAMICS
C o Use the Yearly-Monthly-Daily-Stepping call tree (no code -> commented out)
c#undef  ALLOW_YMDS_TREE
C o Do not call stepping  (no corresponding code -> commented out)
c#define ALLOW_STEPPING_CALL
C o Projection onto Spherical Harmonics (no code -> commented out)
c#undef  ALLOW_SPH_PROJECTION

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_ECCO */
#endif /* ECCO_OPTIONS_H */
