C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/EXF_CPPOPTIONS.h,v 1.2 2002/12/28 10:11:11 dimitri Exp $
C
#include "CPP_OPTIONS.h"
C 
C CPP flags controlling which code is included in the files that
C will be compiled.
C

c   CPP options:
c   ------------
c
c   >>> INCLUDE_EXTERNAL_FORCING_PACKAGE <<<
c       Include this package into the setup.
c
c   >>> EXF_VERBOSE <<<
c       Do a bit more printout for the log file than usual.
c
c   >>> ALLOW_BULKFORMULAE <<<
c       Allows the use of bulk formulae in order to estimate
c       the turbulent fluxes at the ocean's surface.
c                                        
c   >>> ALLOW_ATM_TEMP <<<
c       Allows the use the atmospheric temperature and specific
c       humidity to estimate the sensible and latent heat fluxes.
c
c   >>> ALLOW_ATM_WIND <<<
c       Allows the use the atmospheric wind field to estimate the
c       wind stress at the ocean's surface.
c
c   >>> EXF_NO_BULK_COMPUTATIONS <<<
c       If defined, no bulk formulae computations are carried out
c       within pkg/exf.  This option, in combination with
c       ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP, and ALLOW_ATM_WIND
c       can be used to read-in the atmospheric state and pass it
c       on to a separate package for bulk formulae computations.
c
c   >>> EXF_READ_EVAP <<<
c       If defined, evaporation is read-in from a file, rather than
c       computed from atmospheric state.
c
c   >>> ALLOW_CLIM_CYCLIC <<<
c       If defined, relaxation file record numbers are assumed 1 to
c       12 corresponding to Jan. through Dec.  Otherwise relaxation
c       file record numbers are specified in data.exf_clim
c
c   >>> ALLOW_CLIMTEMP_RELAXATION <<<
c       Allow the relaxation to a monthly climatology of potential
c       temperature, e.g. the Levitus climatology.
c
c   >>> ALLOW_CLIMSALT_RELAXATION <<<
c       Allow the relaxation to a monthly climatology of salinity,
c       e.g. the Levitus climatology.
c
c   >>> ALLOW_CLIMSST_RELAXATION <<<
c       Allow the relaxation to a monthly climatology of sea surface
c       temperature, e.g. the Reynolds climatology.
c
c   >>> ALLOW_CLIMSSS_RELAXATION <<<
c       Allow the relaxation to a monthly climatology of sea surface
c       salinity, e.g. the Levitus climatology.
