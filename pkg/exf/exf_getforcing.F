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
C  EXF_GETFORCING (TOP LEVEL ROUTINE)
C  |
C  |-- EXF_GETCLIM (get climatological fields used e.g. for relax.)
C  |   |--- exf_set_climtemp (relax. to 3-D temperature field)
C  |   |--- exf_set_climsalt (relax. to 3-D salinity field)
C  |   |--- exf_set_climsst  (relax. to 2-D SST field)
C  |   |--- exf_set_climsss  (relax. to 2-D SSS field)
C  |   o
C  |
C  |-- EXF_GETFFIELDS <- this one does almost everything
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
C  |-- EXF_CHECK_RANGE
C  |   |   Check whether read fields are within assumed range
C  |   |   (may capture mismatches in units)
C  |   o
C  |
C  |-- EXF_RADIATION
C  |   |   Compute net or downwelling radiative fluxes via
C  |   |   Stefan-Boltzmann law in case only one is known.
C  |-- EXF_WIND
C  |   |   Compute air-sea wind-stress from winds (or the other way)
C  |-- EXF_BULKFORMULAE
C  |   |   Compute air-sea buoyancy fluxes from atmospheric
C  |   |   state following Large and Pond, JPO, 1981/82
C  |   o
C  |
C  |-- < add time-mean river runoff here, if available >
C  |
C  |-- < update tile edges here >
C  |
C  |-- EXF_GETSURFACEFLUXES
C  |   |   If forcing and control are fluxes, then
C  |   |   control vector anomalies are added here.
C  |   |--- ctrl_get_gen
C  |   o
C  |
C  |-- < treatment of hflux w.r.t. swflux >
C  |
C  |-- EXF_DIAGNOSTICS_FILL
C  |   |   Do EXF-related diagnostics output here.
C  |-- EXF_MONITOR
C  |   |   Monitor EXF-forcing fields
C  |   o
C  |
C  |-- EXF_MAPFIELDS
C  |   |   Forcing fields from exf package are mapped onto
C  |   |   mitgcm forcing arrays.
C  |   |   Mapping enables a runtime rescaling of fields
C  |   o
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
#ifdef ALLOW_TAPENADE
# include "EXF_INTERP_SIZE.h"
# include "EXF_INTERP_PARAM.h"
#endif /* ALLOW_TAPENADE */
#if ( defined ALLOW_DOWNWARD_RADIATION ) || \
      ( defined ALLOW_ATM_TEMP && defined ALLOW_BULKFORMULAE )
# include "FFIELDS.h"
# include "DYNVARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j,ks
#if ( defined ALLOW_DOWNWARD_RADIATION ) || \
      ( defined ALLOW_ATM_TEMP && defined ALLOW_BULKFORMULAE )
      INTEGER kl
      _RL deltaSST
C     exf_Tsf :: local copy of global field gcmSST or extrapolated
C                surface temperature (in deg Celsius)
      _RL exf_Tsf        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
C     == end of interface ==
CEOP

      ks = 1
      IF ( usingPCoords ) ks = Nr
#if ( defined ALLOW_DOWNWARD_RADIATION ) || \
      ( defined ALLOW_ATM_TEMP && defined ALLOW_BULKFORMULAE )
C     Compute the surface temperature either as copy of the global
C     variable gcmSST or by extrapolation to the actual surface
C     (W-point).
      kl = 2
      IF ( usingPCoords ) kl = Nr-1
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)
        IF ( Nr.GE.2 .AND. sstExtrapol.GT.0. _d 0 ) THEN
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           deltaSST = sstExtrapol
     &          *( theta(i,j,ks,bi,bj)-theta(i,j,kl,bi,bj) )
     &          *  maskC(i,j,kl,bi,bj)
           exf_Tsf(i,j,bi,bj) = gcmSST(i,j,bi,bj) + cen2kel
     &          + MAX( deltaSST, 0. _d 0 )
          ENDDO
         ENDDO
        ELSE
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           exf_Tsf(i,j,bi,bj) = gcmSST(i,j,bi,bj) + cen2kel
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#endif

C     Get values of climatological fields.
      CALL EXF_GETCLIM( myTime, myIter, myThid )

C     Get the surface forcing fields.
      CALL EXF_GETFFIELDS( myTime, myIter, myThid )
      IF ( .NOT.useAtmWind ) THEN
       IF ( stressIsOnCgrid .AND. ustressfile.NE.' '
     &                      .AND. vstressfile.NE.' ' )
     &  CALL EXCH_UV_XY_RL( ustress, vstress, .TRUE., myThid )
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
C     Store fields after reading them so that we do not need to save
C     their 0/1 levels to comlev1. Not all fields are required here (in
C     most cases only u/vwind or u/vstress, aqh, atemp, precip,
C     snowprecip, runoff), but we have directives for all potential
C     candidates here.
CADJ STORE ustress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vstress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wspeed       = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef ALLOW_ATM_TEMP
CADJ STORE aqh        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE atemp      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE precip     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE lwflux     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swflux     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowprecip = comlev1, key=ikey_dynamics, kind=isbyte
#  ifdef ALLOW_READ_TURBFLUXES
CADJ STORE hs         = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hl         = comlev1, key=ikey_dynamics, kind=isbyte
#  endif /* ALLOW_READ_TURBFLUXES */
#  ifdef EXF_READ_EVAP
CADJ STORE evap       = comlev1, key=ikey_dynamics, kind=isbyte
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
CADJ STORE swdown     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE lwdown     = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
# else /* ALLOW_ATM_TEMP undef */
#  ifdef SHORTWAVE_HEATING
CADJ STORE swflux     = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
# endif /* ALLOW_ATM_TEMP */
# ifdef ATMOSPHERIC_LOADING
CADJ STORE apressure  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_RUNOFF
CADJ STORE runoff     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SALTFLX
CADJ STORE saltflx    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef EXF_SEAICE_FRACTION
CADJ STORE areamask   = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
        CALL EXF_ADJOINT_SNAPSHOTS( 2, myTime, myIter, myThid )
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_DOWNWARD_RADIATION
C     Set radiative fluxes
      CALL EXF_RADIATION( exf_Tsf, myTime, myIter, myThid )
#endif

C     Set wind fields
      CALL EXF_WIND( myTime, myIter, myThid )

#ifdef ALLOW_ATM_TEMP
# ifdef ALLOW_BULKFORMULAE
#  ifdef ALLOW_AUTODIFF_TAMC
C     Here we probably only need to store uwind, vwind, wstress but we
C     keep the other fields for the paranoid AD-modeller
CADJ STORE uwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wspeed       = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE ustress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vstress      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE wstress      = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
C     Compute turbulent fluxes (and surface stress) from bulk formulae
      CALL EXF_BULKFORMULAE( exf_Tsf, myTime, myIter, myThid )
#  ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE evap         = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
# endif /* ALLOW_BULKFORMULAE */
#endif /* ALLOW_ATM_TEMP */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_ATM_TEMP
C     compute hflux & sflux from multiple components
        DO j = 1,sNy
         DO i = 1,sNx
C     Net surface heat flux.
          hflux(i,j,bi,bj) =
     &         - hs(i,j,bi,bj)
     &         - hl(i,j,bi,bj)
     &         + lwflux(i,j,bi,bj)
#ifndef SHORTWAVE_HEATING
     &         + swflux(i,j,bi,bj)
#endif
C             fresh-water flux from Precipitation and Evaporation.
          sflux(i,j,bi,bj) = evap(i,j,bi,bj) - precip(i,j,bi,bj)
         ENDDO
        ENDDO
#endif /* ALLOW_ATM_TEMP */

C     Apply runoff, masks and exchanges
        DO j = 1,sNy
         DO i = 1,sNx
#ifdef ALLOW_RUNOFF
          sflux(i,j,bi,bj) = sflux(i,j,bi,bj) - runoff(i,j,bi,bj)
#endif
          hflux(i,j,bi,bj) = hflux(i,j,bi,bj)*maskC(i,j,ks,bi,bj)
          sflux(i,j,bi,bj) = sflux(i,j,bi,bj)*maskC(i,j,ks,bi,bj)
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
      CALL EXF_GETSURFACEFLUXES( myTime, myIter, myThid )

      IF ( useExfCheckRange .AND.
     &     ( myIter.EQ.nIter0 .OR. exf_debugLev.GE.debLevC ) ) THEN
       CALL EXF_CHECK_RANGE( myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
      CALL EXF_ADJOINT_SNAPSHOTS( 1, myTime, myIter, myThid )
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_ATM_TEMP
# ifdef SHORTWAVE_HEATING
C     Treatment of qnet
C     The location of the summation of Qnet in exf_mapfields is unfortunate:
C     For backward compatibility issues we want it to happen after
C     applying control variables, but before exf_diagnostics_fill.
C     Therefore, we DO it exactly here:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          hflux(i,j,bi,bj) = hflux(i,j,bi,bj) + swflux(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* SHORTWAVE_HEATING */
#endif /* ALLOW_ATM_TEMP */

C     Diagnostics output
      CALL EXF_DIAGNOSTICS_FILL( myTime, myIter, myThid )

C     Monitor output
      CALL EXF_MONITOR( myTime, myIter, myThid )

C     Map the forcing fields onto the corresponding model fields.
      CALL EXF_MAPFIELDS( myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
      IF ( .NOT. useSEAICE )
     &     CALL EXF_ADJOINT_SNAPSHOTS( 3, myTime, myIter, myThid )
# endif
#endif /* ALLOW_AUTODIFF */

      RETURN
      END
