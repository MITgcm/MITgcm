CBOP
C !ROUTINE: COST_OPTIONS.h
C !INTERFACE:
C #include "COST_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Cost-Function (cost) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef COST_OPTIONS_H
#define COST_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_COST
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */
C   ==================================================================
C-- Package-specific Options & Macros go here

C       >>> Use the EGM-96 geoid error covariance.
#undef  ALLOW_EGM96_ERROR_COV
#undef  ALLOW_READ_EGM_DATA
C       >>> Cost function contributions
#undef ALLOW_HFLUX_COST_CONTRIBUTION
#undef ALLOW_SFLUX_COST_CONTRIBUTION
#undef ALLOW_USTRESS_COST_CONTRIBUTION
#undef ALLOW_VSTRESS_COST_CONTRIBUTION
#undef ALLOW_THETA_COST_CONTRIBUTION
#undef ALLOW_SALT_COST_CONTRIBUTION
#undef ALLOW_SST_COST_CONTRIBUTION
#undef ALLOW_SSS_COST_CONTRIBUTION
#undef ALLOW_SSH_COST_CONTRIBUTION
#undef ALLOW_CTDT_COST_CONTRIBUTION
#undef ALLOW_CTDS_COST_CONTRIBUTION
#undef ALLOW_COST_ATLANTIC
#undef ALLOW_COST_ATLANTIC_HEAT

#undef ALLOW_COST_TEST
#undef ALLOW_COST_TSQUARED
#undef ALLOW_COST_TRACER
#define ALLOW_COST_STREAMICE

C       >>> ALLOW_GENCOST_CONTRIBUTION: interactive way to add basic 2D cost
C         function terms.
C       > In data.ecco, this requires the specification of data file (name,
C         frequency, etc.), bar file name for corresp. model average, standard
C         error file name, etc.
C       > In addition, adding such cost terms requires editing ecco_cost.h to
C         increase NGENCOST, and editing cost_gencost_customize.F to implement
C         the actual model average (i.e. the bar file content).
#undef ALLOW_GENCOST_CONTRIBUTION
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
#undef ALLOW_GENCOST_FREEFORM

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_COST */
#endif /* COST_OPTIONS_H */
