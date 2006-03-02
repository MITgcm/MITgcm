c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getforcing.F,v 1.22 2006/03/02 15:30:11 heimbach Exp $

#include "EXF_OPTIONS.h"

CBOI
C
C !TITLE: EXTERNAL FORCING
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: External forcing package
c \bv
c * The external forcing package, in conjunction with the
c   calendar package (cal), enables the handling of realistic forcing
c   fields of differing temporal forcing patterns.
c * It comprises climatological restoring and relaxation
c * Bulk formulae are implemented to convert atmospheric fields
c   to surface fluxes.
c * An interpolation routine provides on-the-fly interpolation of
c   forcing fields an arbitrary grid onto the model grid.
c * A list of EXF variables and units is in exf_fields.h
c
C     !CALLING SEQUENCE:
c ...
c  exf_getforcing (TOP LEVEL ROUTINE)
c  |
c  |-- exf_getclim (get climatological fields used e.g. for relax.)
c  |   |--- exf_set_climtemp (relax. to 3-D temperature field)
c  |   |--- exf_set_climsalt (relax. to 3-D salinity field)
c  |   |--- exf_set_climsst  (relax. to 2-D SST field)
c  |   |--- exf_set_climsss  (relax. to 2-D SSS field)
c  |   o
c  |
c  |-- exf_getffields <- this one does almost everything
c  |   |   1. reads in fields, either flux or atmos. state,
c  |   |      depending on CPP options (for each variable two fields
c  |   |      consecutive in time are read in and interpolated onto
c  |   |      current time step).
c  |   |   2. If forcing is atmos. state and control is atmos. state,
c  |   |      then the control variable anomalies are read here
c  |   |          * ctrl_getatemp
c  |   |          * ctrl_getaqh
c  |   |          * ctrl_getuwind
c  |   |          * ctrl_getvwind
c  |   |      If forcing and control are fluxes, then
c  |   |      controls are added later.
c  |   o
c  |
c  |-- exf_check_range
c  |   |   1. Check whether read fields are within assumed range
c  |   |      (may capture mismatches in units)
c  |   o
c  |
c  |-- exf_bulkformulae
c  |   |   1. Compute net or downwelling radiative fluxes via
c  |   |      Stefan-Boltzmann law in case only one is known.
c  |   |   2. Compute air-sea momentum and buoyancy fluxes from
c  |   |      atmospheric state following Large and Pond, JPO, 1981/82
c  |   o
c  |
c  |-- < add time-mean river runoff here, if available >
c  |
c  |-- < update tile edges here >
c  |
c  |-- exf_getsurfacefluxes
c  |   |   1. If forcing and control are fluxes, then
c  |   |      controls are added here.
c  |   o
c  |
c  |-- < treatment of hflux w.r.t. swflux >
c  |
c  |-- exf_diagnostics_fill
c  |   |   1. Do EXF-related diagnostics output here.
c  |   o
c  |
c  |-- exf_mapfields
c  |   |   1. Map the EXF variables onto the core MITgcm
c  |   |      forcing fields.
c  |   o
c  |
c  |-- exf_bulkformulae
c  |   If ALLOW_BULKFORMULAE, compute fluxes via bulkformulae
c  |
c  |-- exf_getsurfacefluxes
c  |   If forcing and control is flux, then the
c  |   control vector anomalies are read here
c  |      * ctrl_getheatflux
c  |      * ctrl_getsaltflux
c  |      * ctrl_getzonstress
c  |      * call ctrl_getmerstress
c  |
c  |-- exf_mapfields
c  |   Forcing fields from exf package are mapped onto
c  |   mitgcm forcing arrays.
c  |   Mapping enables a runtime rescaling of fields
c
c \ev
CEOI

CBOP
C     !ROUTINE: exf_getforcing
C     !INTERFACE:
      subroutine exf_getforcing( mytime, myiter, mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE exf_getforcing
c     *=================================================================
c     o Get the forcing fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_OPTIONS.h (or ECCO_CPPOPTIONS.h).
c       A note on surface fluxes:
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c       started: eckert@mit.edu, heimbach@mit.edu, ralf@ocean.mit.edu
c       mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c     *=================================================================
c     | SUBROUTINE exf_getforcing
c     *=================================================================
C     \ev

C     !USES:
      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

c     == global variables ==

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid
      integer myiter
      _RL     mytime

C     !LOCAL VARIABLES:
c     == local variables ==

      integer bi,bj
      integer i,j,k
      character*(max_len_mbuf) msgbuf

c     == end of interface ==
CEOP

c     Get values of climatological fields.
      call exf_getclim( mytime, myiter, mythid )

c     Get the surface forcing fields.
      call exf_getffields( mytime, myiter, mythid )

c     Compute bulk formulae
#ifdef ALLOW_BULKFORMULAE
      call exf_bulkformulae( mytime, myiter, mythid )
#endif

c     Apply runoff, masks and exchanges
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          k = 1
          do j = 1,sny
            do i = 1,snx
#ifdef ALLOW_RUNOFF
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj) - runoff(i,j,bi,bj)
#endif

              hflux(i,j,bi,bj) = hflux(i,j,bi,bj)*maskc(i,j,1,bi,bj)
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj)*maskc(i,j,1,bi,bj)
            enddo
          enddo
        enddo
      enddo

c     Update the tile edges.
      _EXCH_XY_R8(hflux,   mythid)
      _EXCH_XY_R8(sflux,   mythid)
       CALL EXCH_UV_XY_RL(ustress, vstress, .TRUE., myThid)
#ifdef SHORTWAVE_HEATING
      _EXCH_XY_R8(swflux, mythid)
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_R8(apressure, mythid)
#endif

c     Get values of the surface flux anomalies.
      call exf_getsurfacefluxes( mytime, myiter, mythid )

      if ( useExfCheckRange .AND.
     &     ( myiter.EQ.niter0 .OR. debugLevel.GE.debLevB ) ) then
         call exf_check_range( mytime, myiter, mythid )
      endif

#ifdef SHORTWAVE_HEATING
c     Treatment of qnet
c     The location of te summation of Qnet in exf_mapfields is unfortunate.
c     For backward compatibility issues we want it to happen after
c     applying control variables, but before exf_diagnostics_fill.
c     Therefore, we do it exactly here:
      do bj = mybylo(mythid),mybyhi(mythid)
       do bi = mybxlo(mythid),mybxhi(mythid)
        do j = 1-oLy,sNy+oLy
         do i = 1-oLx,sNx+oLx
          hflux(i,j,bi,bj) = hflux(i,j,bi,bj) + swflux(i,j,bi,bj)
         enddo
        enddo
       enddo
      enddo
#endif

c     Diagnostics output
      call exf_diagnostics_fill( mytime, myiter, mythid )

c     Map the forcing fields onto the corresponding model fields.
      call exf_mapfields( mytime, myiter, mythid )

      end

