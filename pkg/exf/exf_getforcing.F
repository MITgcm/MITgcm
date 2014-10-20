C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getforcing.F,v 1.47 2014/10/20 03:13:32 gforget Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOI
C
C !TITLE: EXTERNAL FORCING
C !AUTHORS: mitgcm developers ( mitgcm-support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: External forcing package
C \bv
C * The external forcing package, in conjunction with the
C   calendar package (cal), enables the handling of realistic forcing
C   fields of differing temporal forcing patterns.
C * It comprises climatological restoring and relaxation
C * Bulk formulae are implemented to convert atmospheric fields
C   to surface fluxes.
C * An interpolation routine provides on-the-fly interpolation of
C   forcing fields an arbitrary grid onto the model grid.
C * A list of EXF variables and units is in EXF_FIELDS.h
C
C     !CALLING SEQUENCE:
C ...
C  exf_getforcing (TOP LEVEL ROUTINE)
C  |
C  |-- exf_getclim (get climatological fields used e.g. for relax.)
C  |   |--- exf_set_climtemp (relax. to 3-D temperature field)
C  |   |--- exf_set_climsalt (relax. to 3-D salinity field)
C  |   |--- exf_set_climsst  (relax. to 2-D SST field)
C  |   |--- exf_set_climsss  (relax. to 2-D SSS field)
C  |   o
C  |
C  |-- exf_getffields <- this one does almost everything
C  |   |   1. reads in fields, either flux or atmos. state,
C  |   |      depending on CPP options (for each variable two fields
C  |   |      consecutive in time are read in and interpolated onto
C  |   |      current time step).
C  |   |   2. If forcing is atmos. state and control is atmos. state,
C  |   |      then the control variable anomalies are read here
C  |   |          * ctrl_getatemp
C  |   |          * ctrl_getaqh
C  |   |          * ctrl_getuwind
C  |   |          * ctrl_getvwind
C  |   |      If forcing and control are fluxes, then
C  |   |      controls are added later.
C  |   o
C  |
C  |-- exf_check_range
C  |   |   1. Check whether read fields are within assumed range
C  |   |      (may capture mismatches in units)
C  |   o
C  |
C  |-- exf_bulkformulae
C  |   |   1. Compute net or downwelling radiative fluxes via
C  |   |      Stefan-Boltzmann law in case only one is known.
C  |   |   2. Compute air-sea momentum and buoyancy fluxes from
C  |   |      atmospheric state following Large and Pond, JPO, 1981/82
C  |   o
C  |
C  |-- < add time-mean river runoff here, if available >
C  |
C  |-- < update tile edges here >
C  |
C  |-- exf_getsurfacefluxes
C  |   |   1. If forcing and control are fluxes, then
C  |   |      controls are added here.
C  |   o
C  |
C  |-- < treatment of hflux w.r.t. swflux >
C  |
C  |-- exf_diagnostics_fill
C  |   |   1. Do EXF-related diagnostics output here.
C  |   o
C  |
C  |-- exf_mapfields
C  |   |   1. Map the EXF variables onto the core MITgcm
C  |   |      forcing fields.
C  |   o
C  |
C  |-- exf_bulkformulae
C  |   If ALLOW_BULKFORMULAE, compute fluxes via bulkformulae
C  |
C  |-- exf_getsurfacefluxes
C  |   If forcing and control is flux, then the
C  |   control vector anomalies are read here
C  |      * ctrl_getheatflux
C  |      * ctrl_getsaltflux
C  |      * ctrl_getzonstress
C  |      * CALL ctrl_getmerstress
C  |
C  |-- exf_mapfields
C  |   Forcing fields from exf package are mapped onto
C  |   mitgcm forcing arrays.
C  |   Mapping enables a runtime rescaling of fields
C
C \ev
CEOI

CBOP
C     !ROUTINE: EXF_GETFORCING
C     !INTERFACE:
      SUBROUTINE EXF_GETFORCING( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE EXF_GETFORCING
C     *=================================================================
C     o Get the forcing fields for the current time step. The switches
C       for the inclusion of the individual forcing components have to
C       be set in EXF_OPTIONS.h (or ECCO_CPPOPTIONS.h).
C       A note on surface fluxes:
C       The MITgcm-UV vertical coordinate z is positive upward.
C       This implies that a positive flux is out of the ocean
C       model. However, the wind stress forcing is not treated
C       this way. A positive zonal wind stress accelerates the
C       model ocean towards the east.
C       started: eckert@mit.edu, heimbach@mit.edu, ralf@ocean.mit.edu
C       mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C     *=================================================================
C     | SUBROUTINE EXF_GETFORCING
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
C     == end of interface ==
CEOP

C     Get values of climatological fields.
      CALL exf_getclim( myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_ATM_TEMP
CADJ STORE precip0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE precip1     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowprecip0 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowprecip1 = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif
C     Get the surface forcing fields.
      CALL exf_getffields( myTime, myIter, myThid )
      IF ( .NOT.useAtmWind ) THEN
      IF ( stressIsOnCgrid .AND. ustressfile.NE.' '
     &                     .AND. vstressfile.NE.' ' )
     &  CALL EXCH_UV_XY_RL( ustress, vstress, .TRUE., myThid )
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_AUTODIFF_MONITOR
        CALL EXF_ADJOINT_SNAPSHOTS( 2, myTime, myIter, myThid )
# endif
#endif

#ifdef ALLOW_BULKFORMULAE
C     Set radiative fluxes
      CALL exf_radiation( myTime, myIter, myThid )

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vstress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wspeed       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
C     Set wind fields
      CALL exf_wind( myTime, myIter, myThid )
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vstress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wspeed       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
C     Compute turbulent fluxes (and surface stress) from bulk formulae
      CALL exf_bulkformulae( myTime, myIter, myThid )
#endif

C     Apply runoff, masks and exchanges
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
          k = 1
          DO j = 1,sNy
            DO i = 1,sNx
#ifdef ALLOW_ATM_TEMP
C             Net surface heat flux.
              hflux(i,j,bi,bj) =
     &              - hs(i,j,bi,bj)
     &              - hl(i,j,bi,bj)
     &              + lwflux(i,j,bi,bj)
#ifndef SHORTWAVE_HEATING
     &              + swflux(i,j,bi,bj)
#endif
C             fresh-water flux from Precipitation and Evaporation.
              sflux(i,j,bi,bj) = evap(i,j,bi,bj) - precip(i,j,bi,bj)
#endif /* ALLOW_ATM_TEMP */
#ifdef ALLOW_RUNOFF
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj) - runoff(i,j,bi,bj)
#endif

              hflux(i,j,bi,bj) = hflux(i,j,bi,bj)*maskC(i,j,1,bi,bj)
              sflux(i,j,bi,bj) = sflux(i,j,bi,bj)*maskC(i,j,1,bi,bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

C     Update the tile edges: needed for some EXF fields involved in horizontal
C     averaging, e.g., wind-stress; fields used by main model or other pkgs
C     are exchanged in EXF_MAPFIELDS.
c     _EXCH_XY_RL(hflux,   myThid)
c     _EXCH_XY_RL(sflux,   myThid)
      IF ( stressIsOnCgrid ) THEN
        CALL EXCH_UV_XY_RL( ustress, vstress, .TRUE., myThid )
      ELSE
        CALL EXCH_UV_AGRID_3D_RL(ustress, vstress, .TRUE., 1, myThid)
      ENDIF
#ifdef SHORTWAVE_HEATING
c     _EXCH_XY_RL(swflux, myThid)
#endif
#ifdef ATMOSPHERIC_LOADING
c     _EXCH_XY_RL(apressure, myThid)
#endif
#ifdef EXF_SEAICE_FRACTION
c     _EXCH_XY_RL(areamask, myThid)
#endif

C     Get values of the surface flux anomalies.
      CALL exf_getsurfacefluxes( myTime, myIter, myThid )

      IF ( useExfCheckRange .AND.
     &     ( myIter.EQ.nIter0 .OR. exf_debugLev.GE.debLevC ) ) THEN
         CALL exf_check_range( myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_AUTODIFF_MONITOR
        CALL EXF_ADJOINT_SNAPSHOTS( 1, myTime, myIter, myThid )
# endif
#endif

#ifdef SHORTWAVE_HEATING
C     Treatment of qnet
C     The location of te summation of Qnet in exf_mapfields is unfortunate.
C     For backward compatibility issues we want it to happen after
C     applying control variables, but before exf_diagnostics_fill.
C     Therefore, we DO it exactly here:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-oLy,sNy+oLy
         DO i = 1-oLx,sNx+oLx
          hflux(i,j,bi,bj) = hflux(i,j,bi,bj) + swflux(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

C     Diagnostics output
      CALL exf_diagnostics_fill( myTime, myIter, myThid )

C     Monitor output
      CALL exf_monitor( myTime, myIter, myThid )

C     Map the forcing fields onto the corresponding model fields.
      CALL exf_mapfields( myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_AUTODIFF_MONITOR
      IF ( .NOT. useSEAICE )
     &     CALL EXF_ADJOINT_SNAPSHOTS( 3, myTime, myIter, myThid )
# endif
#endif

      RETURN
      END
