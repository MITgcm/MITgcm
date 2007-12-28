C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_OPTIONS.h,v 1.12 2007/12/28 21:26:16 heimbach Exp $
C $Name:  $

#ifndef EXF_OPTIONS_H
#define EXF_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"

#ifdef ALLOW_EXF

#include "CPP_OPTIONS.h"

#ifdef ALLOW_AUTODIFF_TAMC

C When compile for AD mode,
C this package is currently configured by ECCO_CPPOPTIONS.h
C which is directly included in CPP_OPTIONS.h

#else /* ndef ALLOW_AUTODIFF_TAMC */

C CPP flags controlling which code is included in the files that
C will be compiled.
C

c   pkg/exf CPP options:
c   --------------------
c
c   >>> EXF_VERBOSE <<<
c       Do a bit more printout for the log file than usual.
c
c   >>> ALLOW_ATM_WIND <<<
c       If defined, 10-m wind fields can be read-in from files.
c                                        
c   >>> ALLOW_ATM_TEMP <<<
c       If defined, atmospheric temperature and specific
c       humidity fields can be read-in from files.
c                                        
c   >>> ALLOW_DOWNWARD_RADIATION <<<
c       If defined, downward long-wave and short-wave radiation
c       can be read-in form files or computed from lwflux and swflux.
c
c   >>> ALLOW_BULKFORMULAE <<<
c       Allows the use of bulk formulae in order to estimate
c       turbulent and radiative fluxes at the ocean's surface.
c
c   >>> EXF_READ_EVAP <<<
c       If defined, evaporation fields are read-in, rather than
c       computed from atmospheric state.
c                                        
c   >>> ALLOW_RUNOFF <<<
c       If defined, river and glacier runoff can be read-in from files.
c
c   >>> ATMOSPHERIC_LOADING <<<
c       If defined, atmospheric pressure can be read-in from files.
c   WARNING: this flag is set (define/undef) in CPP_OPTIONS.h 
c            and cannot be changed here (in EXF_OPTIONS)
c
c   >>> ALLOW_CLIMSST_RELAXATION <<<
c       Allow the relaxation to a monthly climatology of sea surface
c       temperature, e.g. the Reynolds climatology.
c
c   >>> ALLOW_CLIMSSS_RELAXATION <<<
c       Allow the relaxation to a monthly climatology of sea surface
c       salinity, e.g. the Levitus climatology.
c
c   >>> USE_EXF_INTERPOLATION <<<
c       Allows specification of arbitrary Cartesian input grids.
c
c   ====================================================================
c
c       The following CPP options:
c
c          ALLOW_ATM_WIND              (WIND)
c          ALLOW_ATM_TEMP              (TEMP)
c          ALLOW_DOWNWARD_RADIATION    (DOWN)
c          ALLOW_BULKFORMULAE          (BULK)
c          EXF_READ_EVAP               (EVAP)
c
c       permit the ocean-model forcing configurations listed in the
c       table below.  The first configuration is the default,
c       flux-forced, ocean model.  The next four are stand-alone
c       configurations that use pkg/exf, open-water bulk formulae to
c       compute the missing surface fluxes from atmospheric variables.
c       The last four configurations can be used in conjunction with
c       pkg/seaice to model ice-covered regions.  The forcing fields
c       in the rightmost column are defined in exf_fields.
c
c
c    WIND |TEMP |DOWN |BULK |EVAP |            actions
c    -----|-----|-----|-----|-----|-------------------------------------
c         |     |     |     |     |
c      -  |  -  |  -  |  -  |  -  | Read-in ustress, vstress, hflux,
c         |     |     |     |     | swflux, and sflux.
c         |     |     |     |     |
c     def | def | def | def |  -  | Read-in uwind, vwind, atemp, aqh,
c         |     |     |     |     | swdown, lwdown, precip, and runoff.
c         |     |     |     |     | Compute ustress, vstress, hflux,
c         |     |     |     |     | swflux, and sflux.
c         |     |     |     |     |
c     def | def |  -  | def |  -  | Read-in uwind, vwind, atemp, aqh,
c         |     |     |     |     | swflux, lwflux, precip, and runoff.
c         |     |     |     |     | Compute ustress, vstress, hflux,
c         |     |     |     |     | and sflux.
c         |     |     |     |     |
c     def |  -  |  -  | def |  -  | Read-in uwind, vwind, hflux,
c         |     |     |     |     | swflux, and sflux.
c         |     |     |     |     | Compute ustress and vstress.
c         |     |     |     |     |
c      -  | def |  -  | def |  -  | Read-in ustress, vstress, atemp,
c         |     |     |     |     | aqh, swflux, lwflux, precip, and
c         |     |     |     |     | runoff.  Compute hflux and sflux.
c         |     |     |     |     |
c     def | def |  -  |  -  | def | Read-in uwind, vwind, atemp, aqh,
c         |     |     |     |     | swflux, lwflux, precip, runoff,
c         |     |     |     |     | and evap.
c         |     |     |     |     |
c     def | def |  -  | def |  -  | Read-in uwind, vwind, atemp, aqh,
c         |     |     |     |     | swflux, lwflux, precip, and runoff.
c         |     |     |     |     | Compute open-water ustress, vstress,
c         |     |     |     |     | hflux, swflux, and evap.
c         |     |     |     |     |
c     def | def | def |  -  | def | Read-in uwind, vwind, atemp, aqh,
c         |     |     |     |     | swdown, lwdown, precip, runoff,
c         |     |     |     |     | and evap.
c         |     |     |     |     |
c     def | def | def | def |  -  | Read-in uwind, vwind, atemp, aqh,
c         |     |     |     |     | swdown, lwdown, precip, and runoff.
c         |     |     |     |     | Compute open-water ustress, vstress,
c         |     |     |     |     | hflux, swflux, and evap.
c
c   ====================================================================

C   Do more printout for the protocol file than usual.
#undef EXF_VERBOSE

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#if (defined (ALLOW_ATM_TEMP) || \
     defined (ALLOW_ATM_WIND))
# define ALLOW_BULKFORMULAE
# undef ALLOW_BULK_LARGEYEAGER04
#endif

C   Relaxation to monthly climatologies.
#define ALLOW_CLIMSST_RELAXATION
#define ALLOW_CLIMSSS_RELAXATION

C   Use spatial interpolation to interpolate
C   forcing files from input grid to model grid.
#undef USE_EXF_INTERPOLATION

#define EXF_INTERP_USE_DYNALLOC
#if ( defined (EXF_INTERP_USE_DYNALLOC) & defined (USING_THREADS) )
# define EXF_IREAD_USE_GLOBAL_POINTER
#endif

#endif /* ndef ALLOW_AUTODIFF_TAMC */
#endif /* ALLOW_EXF */
#endif /* EXF_OPTIONS_H */
