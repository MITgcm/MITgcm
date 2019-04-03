C $Header: /u/gcmpack/MITgcm_contrib/gael/verification/global_oce_llc90/code/ECCO_OPTIONS.h,v 1.3 2015/08/06 20:37:05 gforget Exp $
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
C
cph >>>>>> !!!!!! SPECIAL SEAICE FLAG FOR TESTING !!!!!! <<<<<<
cph#define  SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
cph >>>>>> !!!!!! SPECIAL SEAICE FLAG FOR TESTING !!!!!! <<<<<<


CAB 
cph#define  ALLOW_ECCO_FORWARD_RUN
#define  ALLOW_ECCO_OPTIMIZATION

C       >>> Do a long protocol.
#undef ECCO_VERBOSE
C o Include/exclude code in order to be able to automatically

#define ALLOW_AUTODIFF_TAMC

C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING

C extend to 4-level checkpointing
#undef AUTODIFF_4_LEVEL_CHECKPOINT

C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR

C o use divided adjoint to split adjoint computations
C#define ALLOW_DIVIDED_ADJOINT

C o TAMC compatible subroutine parameter list
#undef AUTODIFF_TAMC_COMPATIBILITY

C       >>> Cost function contributions
#define ALLOW_ECCO_OLD_FC_PRINT

C       >>> ALLOW_GENCOST_CONTRIBUTION: interactive way to add basic 2D cost function terms.
C       > In data.ecco, this requires the specification of data file (name, frequency,
C         etc.), bar file name for corresp. model average, standard error file name, etc.
C       > In addition, adding such cost terms requires editing ecco_cost.h to increase
C         NGENCOST, and editing cost_gencost_customize.F to implement the actual
C         model average (i.e. the bar file content).
#define ALLOW_GENCOST_CONTRIBUTION
C       >>> free form version of GENCOST: allows one to use otherwise defined elements (e.g.
C         psbar and and topex data) while taking advantage of the cost function/namelist slots
C         that can be made available using ALLOW_GENCOST_CONTRIBUTION. To this end
C         ALLOW_GENCOST_CONTRIBUTION simply switches off tests that check whether all of the
C         gencost elements (e.g. gencost_barfile and gencost_datafile) are specified in data.ecco.
C       > While this option increases flexibility within the gencost framework, it implies more room
C         for error, so it should be used cautiously, and with good knowledge of the rest of pkg/ecco.
C       > It requires providing a specific cost function routine, and editing cost_gencost_all.F accordingly.
#define ALLOW_GENCOST_FREEFORM
C       >>> 3 dimensional version of GENCOST:
#define ALLOW_GENCOST3D

C#define ALLOW_GENCOST_SIGMAR
C#define ALLOW_GENCOST_DTHETADR

c in case there is a single observational file (rather than yearly files)
c assume it contains a climatology (otherwise, assume it is a full time series)
#define COST_GENERIC_ASSUME_CYCLIC

c include global mean steric sea level correction in etanFull
c#define ALLOW_PSBAR_STERIC
C#define ALLOW_SHALLOW_ALTIMETRY
C#undef  ALLOW_HIGHLAT_ALTIMETRY

C       >>> In-Situ Profiles.
C#define ALLOW_PROFILES_CONTRIBUTION
C       >>> ADDED THIS FOR TACC Debug
C#define AUTODIFF_USE_OLDSTORE_2D
C#define AUTODIFF_USE_OLDSTORE_3D


CAB
#undef ALLOW_ECCO_DEBUG
C Here i want to test the flags
C#define ECCO_CTRL_DEPRECATED
C
C
CCC ********************************************************************
CCC ***               Control vector Package                         ***
CCC ********************************************************************
CC
CC#define  ALLOW_NONDIMENSIONAL_CONTROL_IO
CC#undef  ALLOW_TAMC_SINGLEPREC_COMLEV
CC       >>> Atmospheric state.
C#define  ALLOW_ATEMP_CONTROL
C#define  ALLOW_AQH_CONTROL
C#define  ALLOW_PRECIP_CONTROL
C#define  ALLOW_SNOWPRECIP_CONTROL
C#define  ALLOW_SWDOWN_CONTROL
C#define  ALLOW_LWDOWN_CONTROL
C#define  ALLOW_UWIND_CONTROL
C#define  ALLOW_VWIND_CONTROL
C#undef  ALLOW_EVAP_CONTROL
C#define  ALLOW_APRESSURE_CONTROL
C#undef  ALLOW_RUNOFF_CONTROL

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_ECCO */
#endif /* ECCO_OPTIONS_H */

