C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_OPTIONS.h,v 1.38 2017/01/27 17:22:55 jmc Exp $
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
C   > ( EXF_VERBOSE ) < replaced with run-time integer parameter "exf_debugLev"
C
C   >>> ALLOW_ATM_WIND <<<
C       If defined, 10-m wind fields can be read-in from files.
C
C   >>> ALLOW_ATM_TEMP <<<
C       This is the main EXF option controling air-sea boyancy fluxes:
C      When undefined, net heat flux (Qnet) and net fresh water flux
C       (EmP or EmPmR) are set according to hfluxfile & sfluxfile setting.
C      When defined, net heat flux and net fresh water flux are computed
C       from sum of various components (radiative SW,LW + turbulent heat
C       fluxes SH,LH ; Evap, Precip and optionally RunOff) thus ignoring
C       hfluxfile & sfluxfile.
C      In addition, it allows to read-in from files atmospheric temperature
C       and specific humidity, net radiative fluxes, and also precip.
C       Also enable to read-in Evap (if EXF_READ_EVAP is defined) or
C       turbulent heat fluxes (if ALLOW_READ_TURBFLUXES is defined).
C
C   >>> ALLOW_DOWNWARD_RADIATION <<<
C       If defined, downward long-wave and short-wave radiation
C       can be read-in form files to compute net lwflux and swflux.
C
C   >>> ALLOW_ZENITHANGLE <<<
C       If defined, ocean albedo varies with the zenith angle, and
C       incoming fluxes at the top of the atmosphere are computed
C
C   >>> ALLOW_BULKFORMULAE <<<
C       Allows the use of bulk formulae in order to estimate
C       turbulent fluxes (Sensible,Latent,Evap) at the ocean surface.
C
C   >>> EXF_CALC_ATMRHO
C       Calculate the local air density as function of temp, humidity
C       and pressure
C
C   >>> EXF_READ_EVAP <<<
C       If defined, evaporation field is read-in from file;
C       if using BULKFORMULAE, evap that was computed from atmospheric state
C       will be replaced by read-in one but computed latent heat flux is kept.
C
C   >>> ALLOW_READ_TURBFLUXES <<<
C       Enable to read-in Sensible and Latent Heat fluxes from files
C       (but overwritten if ALLOW_BULKFORMULAE is defined).
C
C   >>> ALLOW_RUNOFF <<<
C       If defined, river and glacier runoff can be read-in from files.
C
C   >>> ALLOW_SALTFLX <<<
C       If defined, upward salt flux can be read-in from files.
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
C       Allows specification of arbitrary Lat-Lon input grids.
C
C   ====================================================================
C
C    The following CPP options:
C       ALLOW_ATM_WIND              (WIND)
C       ALLOW_ATM_TEMP              (TEMP)
C       ALLOW_DOWNWARD_RADIATION    (DOWN)
C       ALLOW_BULKFORMULAE          (BULK)
C       EXF_READ_EVAP               (EVAP)
C       ALLOW_READ_TURBFLUXES       (TURB)
C
C    permit the ocean-model forcing configurations listed in the 2 tables
C    below. The first configuration is the standard, flux-forced, ocean
C    model. Configurations A2 with B3 or B4 are stand-alone configurations
C    that use pkg/exf, open-water bulk formulae to compute the missing
C    surface fluxes from atmospheric variables.
C    The forcing fields in the rightmost column are defined in EXF_FIELDS.h
C
C     A) Surface momentum flux options
C
C    # |WIND |            actions
C   ---|-----|-------------------------------------------------------------
C   (1)|  -  | Read-in ustress, vstress (if needed in B, compute wind-speed)
C      |     |
C   (2)| def | Read-in uwind, vwind ; compute ustress, vstress.
C   ---|-----|-------------------------------------------------------------
C
C     B) Surface boyancy flux options
C
C    # |TEMP |DOWN |BULK |EVAP |TURB |            actions
C   ---|-----|-----|-----|-----|-----|-------------------------------------
C   (1)|  -  |  -  |  -  |  -  |  -  | Read-in hflux, swflux and sflux.
C      |     |     |     |     |     |
C   (2)|  -  | def |  -  |  -  |  -  | Read-in hflux, swdown and sflux.
C      |     |     |     |     |     | Compute swflux.
C      |     |     |     |     |     |
C   (3)| def | def | def |  -  |  -  | Read-in atemp, aqh, swdown, lwdown,
C      |     |     |     |     |     |  precip, and runoff.
C      |     |     |     |     |     | Compute hflux, swflux and sflux.
C      |     |     |     |     |     |
C   (4)| def |  -  | def |  -  |  -  | Read-in atemp, aqh, swflux, lwflux,
C      |     |     |     |     |     |  precip, and runoff.
C      |     |     |     |     |     | Compute hflux and sflux.
C      |     |     |     |     |     |
C   (5)| def | def |  -  | def | def | Read-in hs, hl, swdown, lwdown,
C      |     |     |     |     |     |  evap, precip and runoff.
C      |     |     |     |     |     | Compute hflux, swflux and sflux.
C      |     |     |     |     |     |
C   (6)| def |  -  |  -  | def | def | Read-in hs, hl, swflux, lwflux,
C      |     |     |     |     |     |  evap, precip and runoff.
C      |     |     |     |     |     | Compute  hflux and sflux.
C
C   =======================================================================

C-  Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#ifdef ALLOW_ATM_TEMP
C Note: To use ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP needs to be defined
# define ALLOW_BULKFORMULAE
# undef  ALLOW_BULK_LARGEYEAGER04
# undef  EXF_READ_EVAP
# ifndef ALLOW_BULKFORMULAE
C  Note: To use ALLOW_READ_TURBFLUXES, ALLOW_ATM_TEMP needs to
C        be defined but ALLOW_BULKFORMULAE needs to be undef
#  define ALLOW_READ_TURBFLUXES
# endif
#endif /* ALLOW_ATM_TEMP */

C-  Other forcing fields
#define  ALLOW_RUNOFF
#undef   ALLOW_RUNOFTEMP
#define  ALLOW_SALTFLX

#if (defined (ALLOW_BULKFORMULAE) && defined (ATMOSPHERIC_LOADING))
C Note: To use EXF_CALC_ATMRHO, both ALLOW_BULKFORMULAE
C       and ATMOSPHERIC_LOADING need to be defined
# undef EXF_CALC_ATMRHO
#endif

C-  Zenith Angle/Albedo related flags.
#ifdef ALLOW_DOWNWARD_RADIATION
# undef ALLOW_ZENITHANGLE
#endif

C-  Use ocean_emissivity*lwdown in lwFlux. This flag should be defined
C   unless to reproduce old results (obtained with inconsistent old code)
#ifdef ALLOW_DOWNWARD_RADIATION
# define EXF_LWDOWN_WITH_EMISSIVITY
#endif

C-  Relaxation to monthly climatologies.
#define ALLOW_CLIMSST_RELAXATION
#define ALLOW_CLIMSSS_RELAXATION

C-  Allows to read-in seaice fraction from files (areaMaskFile)
#undef EXF_SEAICE_FRACTION

C-  Use spatial interpolation to interpolate
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
