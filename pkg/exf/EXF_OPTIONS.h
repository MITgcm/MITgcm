C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_OPTIONS.h,v 1.30 2014/05/02 22:14:52 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: EXF_OPTIONS.h
C !INTERFACE:
C #include "EXF_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for EXternal Forcing (EXF) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef EXF_OPTIONS_H
#define EXF_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_EXF
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */

C-- Package-specific Options & Macros go here

C   pkg/exf CPP options:
C   --------------------
C
C   > ( EXF_VERBOSE ) < replaced with run-time, logical parameter "exf_verbose".
C
C   >>> ALLOW_ATM_WIND <<<
C       If defined, 10-m wind fields can be read-in from files.
C
C   >>> ALLOW_ATM_TEMP <<<
C       If defined, atmospheric temperature and specific
C       humidity fields can be read-in from files.
C
C   >>> ALLOW_DOWNWARD_RADIATION <<<
C       If defined, downward long-wave and short-wave radiation
C       can be read-in form files or computed from lwflux and swflux.
C
C   >>> ALLOW_ZENITHANGLE <<<
C       If defined, ocean albedo varies with the zenith angle, and
C       incoming fluxes at the top of the atmosphere are computed
C
C   >>> ALLOW_BULKFORMULAE <<<
C       Allows the use of bulk formulae in order to estimate
C       turbulent and radiative fluxes at the ocean surface.
C
C   >>> EXF_READ_EVAP <<<
C       If defined, evaporation fields are read-in, rather than
C       computed from atmospheric state.
C
C   >>> ALLOW_RUNOFF <<<
C       If defined, river and glacier runoff can be read-in from files.
C
C   >>> ALLOW_RUNOFTEMP <<<
C       If defined, river and glacier runoff temperature
C       can be read-in from files.
C
C   >>> ATMOSPHERIC_LOADING <<<
C       If defined, atmospheric pressure can be read-in from files.
C   WARNING: this flag is set (define/undef) in CPP_OPTIONS.h
C            and cannot be changed here (in EXF_OPTIONS)
C
C   >>> EXF_SEAICE_FRACTION <<<
C       If defined, seaice fraction can be read-in from files (areaMaskFile)
C
C   >>> ALLOW_CLIMSST_RELAXATION <<<
C       Allow the relaxation to a monthly climatology of sea surface
C       temperature, e.g. the Reynolds climatology.
C
C   >>> ALLOW_CLIMSSS_RELAXATION <<<
C       Allow the relaxation to a monthly climatology of sea surface
C       salinity, e.g. the Levitus climatology.
C
C   >>> USE_EXF_INTERPOLATION <<<
C       Allows specification of arbitrary Cartesian input grids.
C
C   >>> EXF_CALC_ATMRHO
C       Calculate the local atm density as function of temp, humidity
C       and pressure
C
C   ====================================================================
C
C       The following CPP options:
C
C          ALLOW_ATM_WIND              (WIND)
C          ALLOW_ATM_TEMP              (TEMP)
C          ALLOW_DOWNWARD_RADIATION    (DOWN)
C          ALLOW_BULKFORMULAE          (BULK)
C          EXF_READ_EVAP               (EVAP)
C
C       permit the ocean-model forcing configurations listed in the
C       table below.  The first configuration is the default,
C       flux-forced, ocean model.  The next four are stand-alone
C       configurations that use pkg/exf, open-water bulk formulae to
C       compute the missing surface fluxes from atmospheric variables.
C       The last four configurations can be used in conjunction with
C       pkg/seaice to model ice-covered regions.  The forcing fields
C       in the rightmost column are defined in exf_fields.
C
C
C    WIND |TEMP |DOWN |BULK |EVAP |            actions
C    -----|-----|-----|-----|-----|-------------------------------------
C         |     |     |     |     |
C      -  |  -  |  -  |  -  |  -  | Read-in ustress, vstress, hflux,
C         |     |     |     |     | swflux, and sflux.
C         |     |     |     |     |
C     def | def | def | def |  -  | Read-in uwind, vwind, atemp, aqh,
C         |     |     |     |     | swdown, lwdown, precip, and runoff.
C         |     |     |     |     | Compute ustress, vstress, hflux,
C         |     |     |     |     | swflux, and sflux.
C         |     |     |     |     |
C     def | def |  -  | def |  -  | Read-in uwind, vwind, atemp, aqh,
C         |     |     |     |     | swflux, lwflux, precip, and runoff.
C         |     |     |     |     | Compute ustress, vstress, hflux,
C         |     |     |     |     | and sflux.
C         |     |     |     |     |
C     def |  -  |  -  | def |  -  | Read-in uwind, vwind, hflux,
C         |     |     |     |     | swflux, and sflux.
C         |     |     |     |     | Compute ustress and vstress.
C         |     |     |     |     |
C      -  | def |  -  | def |  -  | Read-in ustress, vstress, atemp,
C         |     |     |     |     | aqh, swflux, lwflux, precip, and
C         |     |     |     |     | runoff.  Compute hflux and sflux.
C         |     |     |     |     |
C     def | def |  -  |  -  | def | Read-in uwind, vwind, atemp, aqh,
C         |     |     |     |     | swflux, lwflux, precip, runoff,
C         |     |     |     |     | and evap.
C         |     |     |     |     |
C     def | def |  -  | def |  -  | Read-in uwind, vwind, atemp, aqh,
C         |     |     |     |     | swflux, lwflux, precip, and runoff.
C         |     |     |     |     | Compute open-water ustress, vstress,
C         |     |     |     |     | hflux, swflux, and evap.
C         |     |     |     |     |
C     def | def | def |  -  | def | Read-in uwind, vwind, atemp, aqh,
C         |     |     |     |     | swdown, lwdown, precip, runoff,
C         |     |     |     |     | and evap.
C         |     |     |     |     |
C     def | def | def | def |  -  | Read-in uwind, vwind, atemp, aqh,
C         |     |     |     |     | swdown, lwdown, precip, and runoff.
C         |     |     |     |     | Compute open-water ustress, vstress,
C         |     |     |     |     | hflux, swflux, and evap.
C
C   ====================================================================

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#undef   ALLOW_RUNOFTEMP
#if (defined (ALLOW_ATM_TEMP) || defined (ALLOW_ATM_WIND))
# define ALLOW_BULKFORMULAE
# undef  ALLOW_BULK_LARGEYEAGER04
#endif
#if (defined (ALLOW_ATM_TEMP) && defined (ATMOSPHERIC_LOADING))
# undef  EXF_CALC_ATMRHO
#endif

C   Zenith Angle/Albedo related flags.
#ifdef ALLOW_DOWNWARD_RADIATION
# undef ALLOW_ZENITHANGLE
#endif

C   Use ocean_emissivity*lwdown in lwFlux. This flag should be defined
C   unless to reproduce old results (obtained with inconsistent old code)
#ifdef ALLOW_DOWNWARD_RADIATION
# define EXF_LWDOWN_WITH_EMISSIVITY
#endif

C   Relaxation to monthly climatologies.
#define ALLOW_CLIMSST_RELAXATION
#define ALLOW_CLIMSSS_RELAXATION

C   Allows to read-in seaice fraction from files (areaMaskFile)
#undef EXF_SEAICE_FRACTION

C   Use spatial interpolation to interpolate
C   forcing files from input grid to model grid.
#undef USE_EXF_INTERPOLATION
C   for interpolated vector fields, rotate towards model-grid axis
C   using old rotation formulae (instead of grid-angles)
#undef EXF_USE_OLD_VEC_ROTATION
C   for interpolation around N & S pole, use the old formulation
C   (no pole symmetry, single vector-comp interp, reset to 0 zonal-comp @ N.pole)
#undef EXF_USE_OLD_INTERP_POLE

#define EXF_INTERP_USE_DYNALLOC
#if ( defined (EXF_INTERP_USE_DYNALLOC) && defined (USING_THREADS) )
# define EXF_IREAD_USE_GLOBAL_POINTER
#endif

#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_EXF */
#endif /* EXF_OPTIONS_H */
