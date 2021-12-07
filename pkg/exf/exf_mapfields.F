#include "EXF_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP 0
C     !ROUTINE: EXF_MAPFIELDS

C     !INTERFACE:
      SUBROUTINE EXF_MAPFIELDS( myTime, myIter, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE EXF_MAPFIELDS
C     ==================================================================
C
C     o Map external forcing fields (ustress, vstress, hflux, sflux,
C       swflux, apressure, climsss, climsst, etc.) onto ocean model
C       arrays (fu, fv, Qnet, EmPmR, Qsw, pLoad, SSS, SST, etc.).
C       This routine is included to separate the ocean state estimation
C       tool as much as possible from the ocean model.  Unit and sign
C       conventions can be customized using variables exf_outscal_*,
C       which are set in exf_readparms.F.  See the header files
C       EXF_FIELDS.h and FFIELDS.h for definitions of the various input
C       and output fields and for default unit and sign convetions.
C
C     started: Christian Eckert eckert@mit.edu  09-Aug-1999
C
C     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
C              - Restructured the code in order to create a package
C                for the MITgcmUV.
C
C              Christian Eckert eckert@mit.edu  12-Feb-2000
C              - Changed Routine names (package prefix: exf_)
C
C              Patrick Heimbach, heimbach@mit.edu  06-May-2000
C              - added and changed CPP flag structure for
C                ALLOW_BULKFORMULAE, ALLOW_ATM_TEMP
C
C              Patrick Heimbach, heimbach@mit.edu  23-May-2000
C              - sign change of ustress/vstress incorporated into
C                scaling factors exf_outscal_ust, exf_outscal_vst
C
C     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C
C     ==================================================================
C     SUBROUTINE EXF_MAPFIELDS
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_FIELDS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,ks
      INTEGER imin, imax
      INTEGER jmin, jmax
      PARAMETER ( imin = 1-OLx , imax = sNx+OLx )
      PARAMETER ( jmin = 1-OLy , jmax = sNy+OLy )
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey
#endif
CEOP

C--   set surface level index:
      ks = 1
      IF ( usingPCoords ) ks = Nr

      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C     Heat flux.
          DO j = jmin,jmax
            DO i = imin,imax
             Qnet(i,j,bi,bj) = exf_outscal_hflux*hflux(i,j,bi,bj)
            ENDDO
          ENDDO
          IF ( hfluxfile .EQ. ' ' ) THEN
           DO j = jmin,jmax
            DO i = imin,imax
             Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) -
     &            exf_outscal_hflux * ( hflux_exfremo_intercept +
     &            hflux_exfremo_slope*(myTime-startTime) )
            ENDDO
           ENDDO
          ENDIF

C     Freshwater flux.
          DO j = jmin,jmax
            DO i = imin,imax
             EmPmR(i,j,bi,bj)= exf_outscal_sflux*sflux(i,j,bi,bj)
     &                                          *rhoConstFresh
            ENDDO
          ENDDO
          IF ( sfluxfile .EQ. ' ' ) THEN
           DO j = jmin,jmax
            DO i = imin,imax
             EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj) - rhoConstFresh*
     &            exf_outscal_sflux * ( sflux_exfremo_intercept +
     &            sflux_exfremo_slope*(myTime-startTime) )
            ENDDO
           ENDDO
          ENDIF

#ifdef ALLOW_ATM_TEMP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
          IF ( temp_EvPrRn .NE. UNSET_RL ) THEN
C--   Account for energy content of Precip + RunOff & Evap. Assumes:
C     1) Rain has same temp as Air
C     2) Snow has no heat capacity (consistent with seaice & thsice pkgs)
C     3) No distinction between sea-water Cp and fresh-water Cp
C     4) By default, RunOff comes at the temp of surface water (with same Cp);
C        ifdef ALLOW_RUNOFTEMP, RunOff temp can be specified in runoftempfile.
C     5) Evap is released to the Atmos @ surf-temp (=SST); should be using
C        the water-vapor heat capacity here and consistently in Bulk-Formulae;
C        Could also be put directly into Latent Heat flux.
           IF ( snowPrecipFile .NE. ' ' ) THEN
C--   Melt snow (if provided) into the ocean and account for rain-temp
            DO j = 1, sNy
             DO i = 1, sNx
              Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              + flami*snowPrecip(i,j,bi,bj)*rhoConstFresh
     &              - HeatCapacity_Cp
     &               *( atemp(i,j,bi,bj) - cen2kel - temp_EvPrRn )
     &               *( precip(i,j,bi,bj)- snowPrecip(i,j,bi,bj) )
     &               *rhoConstFresh
             ENDDO
            ENDDO
           ELSE
C--   Make snow (according to Air Temp) and melt it in the ocean
C     note: here we just use the same criteria as over seaice but would be
C           better to consider a higher altitude air temp, e.g., 850.mb
            DO j = 1, sNy
             DO i = 1, sNx
              IF ( atemp(i,j,bi,bj).LT.cen2kel ) THEN
               Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              + flami*precip(i,j,bi,bj)*rhoConstFresh
              ELSE
C--   Account for rain-temp
               Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              - HeatCapacity_Cp
     &               *( atemp(i,j,bi,bj) - cen2kel - temp_EvPrRn )
     &               *precip(i,j,bi,bj)*rhoConstFresh
              ENDIF
             ENDDO
            ENDDO
           ENDIF
#ifdef ALLOW_RUNOFF
C--   Account for energy content of RunOff:
           DO j = 1, sNy
            DO i = 1, sNx
              Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              - HeatCapacity_Cp
     &               *( theta(i,j,ks,bi,bj) - temp_EvPrRn )
     &               *runoff(i,j,bi,bj)*rhoConstFresh
            ENDDO
           ENDDO
#endif
C--   Account for energy content of Evap:
           DO j = 1, sNy
            DO i = 1, sNx
              Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              + HeatCapacity_Cp
     &               *( theta(i,j,ks,bi,bj) - temp_EvPrRn )
     &               *evap(i,j,bi,bj)*rhoConstFresh
              Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)*maskC(i,j,ks,bi,bj)
            ENDDO
           ENDDO
          ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_ATM_TEMP */
#if defined(ALLOW_RUNOFF) && defined(ALLOW_RUNOFTEMP)
          IF ( runoftempfile .NE. ' ' ) THEN
C--   Add energy content of RunOff
           DO j = 1, sNy
            DO i = 1, sNx
               Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              + HeatCapacity_Cp
     &              *( theta(i,j,ks,bi,bj) - runoftemp(i,j,bi,bj) )
     &              *runoff(i,j,bi,bj)*rhoConstFresh
            ENDDO
           ENDDO
          ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
C             Zonal wind stress.
              IF (ustress(i,j,bi,bj).GT.windstressmax) THEN
                ustress(i,j,bi,bj)=windstressmax
              ENDIF
            ENDDO
          ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
              IF (ustress(i,j,bi,bj).LT.-windstressmax) THEN
                ustress(i,j,bi,bj)=-windstressmax
              ENDIF
            ENDDO
          ENDDO
          IF ( stressIsOnCgrid ) THEN
           DO j = jmin,jmax
            DO i = imin+1,imax
              fu(i,j,bi,bj) = exf_outscal_ustress*ustress(i,j,bi,bj)
            ENDDO
           ENDDO
          ELSE
           DO j = jmin,jmax
            DO i = imin+1,imax
C     Shift wind stresses calculated at Grid-center to W/S points
              fu(i,j,bi,bj) = exf_outscal_ustress*
     &              (ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))
     &              *exf_half*maskW(i,j,ks,bi,bj)
            ENDDO
           ENDDO
          ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
C             Meridional wind stress.
              IF (vstress(i,j,bi,bj).GT.windstressmax) THEN
                vstress(i,j,bi,bj)=windstressmax
              ENDIF
            ENDDO
          ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vstress(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
          DO j = jmin,jmax
            DO i = imin,imax
              IF (vstress(i,j,bi,bj).LT.-windstressmax) THEN
                vstress(i,j,bi,bj)=-windstressmax
              ENDIF
            ENDDO
          ENDDO
          IF ( stressIsOnCgrid ) THEN
           DO j = jmin+1,jmax
            DO i = imin,imax
              fv(i,j,bi,bj) = exf_outscal_vstress*vstress(i,j,bi,bj)
            ENDDO
           ENDDO
          ELSE
           DO j = jmin+1,jmax
            DO i = imin,imax
C     Shift wind stresses calculated at C-points to W/S points
              fv(i,j,bi,bj) = exf_outscal_vstress*
     &              (vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))
     &              *exf_half*maskS(i,j,ks,bi,bj)
            ENDDO
           ENDDO
          ENDIF

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
C             Short wave radiative flux.
          DO j = jmin,jmax
            DO i = imin,imax
             Qsw(i,j,bi,bj)  = exf_outscal_swflux*swflux(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
          DO j = jmin,jmax
            DO i = imin,imax
             SST(i,j,bi,bj)  = exf_outscal_sst*climsst(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
          DO j = jmin,jmax
            DO i = imin,imax
             SSS(i,j,bi,bj)  = exf_outscal_sss*climsss(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef ATMOSPHERIC_LOADING
C-    subtract "surf_pRef" to fill in atmos. pressure anomaly "pLoad"
          DO j = jmin,jmax
            DO i = imin,imax
             pLoad(i,j,bi,bj) =
     &          exf_outscal_apressure*apressure(i,j,bi,bj) - surf_pRef
            ENDDO
          ENDDO
#endif

#ifdef EXF_ALLOW_TIDES
          DO j = jmin,jmax
            DO i = imin,imax
             phiTide2d(i,j,bi,bj)=exf_outscal_tidePot*tidePot(i,j,bi,bj)
            ENDDO
          ENDDO
#endif /* EXF_ALLOW_TIDES */

#ifdef ALLOW_SALTFLX
          DO j = jmin,jmax
            DO i = imin,imax
              saltFlux(i,j,bi,bj) = saltflx(i,j,bi,bj)
            ENDDO
          ENDDO
#endif

#ifdef EXF_SEAICE_FRACTION
          DO j = jmin,jmax
            DO i = imin,imax
              exf_iceFraction(i,j,bi,bj) =
     &           exf_outscal_areamask*areamask(i,j,bi,bj)
              exf_iceFraction(i,j,bi,bj) =
     &           MIN( MAX(exf_iceFraction(i,j,bi,bj),zeroRS), oneRS )
            ENDDO
          ENDDO
#endif

       ENDDO
      ENDDO

C--   Update the tile edges.
      _EXCH_XY_RS(  Qnet, myThid )
      _EXCH_XY_RS( EmPmR, myThid )
       CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
c#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
#ifdef SHORTWAVE_HEATING
C     Qsw used in SHORTWAVE_HEATING code & for diagnostics (<- EXCH not needed)
      _EXCH_XY_RS(   Qsw, myThid )
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
      _EXCH_XY_RS(   SST, myThid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
      _EXCH_XY_RS(   SSS, myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_RS( pLoad, myThid )
#endif
#ifdef EXF_ALLOW_TIDES
      _EXCH_XY_RS( phiTide2d, myThid )
#endif
#ifdef EXF_SEAICE_FRACTION
      _EXCH_XY_RS( exf_iceFraction, myThid )
#endif

      RETURN
      END
