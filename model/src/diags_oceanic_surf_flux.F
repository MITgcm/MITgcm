#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_OCEANIC_SURF_FLUX
C     !INTERFACE:
      SUBROUTINE DIAGS_OCEANIC_SURF_FLUX( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DIAGS_OCEANIC_SURF_FLUX
C     | o Compute Diagnostics of Surface Fluxes (ocean only)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

C     !LOCAL VARIABLES:
C     i,j,bi,bj :: loop indices
C     ks        :: surface level index
      INTEGER i,j,bi,bj
      INTEGER ks
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpFac

C-    Time Averages of surface fluxes
       IF ( usingPCoords ) THEN
        ks=Nr
       ELSE
        ks=1
       ENDIF

C-    net Fresh Water flux into the ocean (+=down), [kg/m2/s]
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL_RS( EmPmR,tmpFac,1,
     &                             'oceFWflx',0, 1,0,1,1,myThid )

C-    net Salt flux into the ocean (+=down), [psu.kg/m2/s ~ g/m2/s]
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL_RS( saltFlux,tmpFac,1,
     &                             'oceSflux',0, 1,0,1,1,myThid )

C-    Qnet (= net heat flux into the ocean, +=down, [W/m2])
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL_RS( Qnet,tmpFac,1,
     &                             'oceQnet ',0, 1,0,1,1,myThid )

C-    Qsw (= net short-wave into the ocean, +=down, [W/m2])
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL_RS( Qsw,tmpFac,1,
     &                             'oceQsw  ',0, 1,0,1,1,myThid )

      IF ( fluidIsWater .OR. useAtm_Phys ) THEN
C-    taux (surface momentum flux [Pa=N/m2], +=down = increase u-oce)
       CALL DIAGNOSTICS_SCALE_FILL_RS( fu,foFacMom,1,
     &                             'oceTAUX ',0, 1,0,1,1,myThid )

C-    tauy (surface momentum flux [Pa=N/m2], +=down = increase v-oce)
       CALL DIAGNOSTICS_SCALE_FILL_RS( fv,foFacMom,1,
     &                             'oceTAUY ',0, 1,0,1,1,myThid )
      ENDIF

C-    sea-ice loading (expressed in Mass of ice+snow / area unit, [kg/m2])
       CALL DIAGNOSTICS_FILL_RS( sIceLoad,'sIceLoad',0,1,0,1,1,myThid )

      IF ( fluidIsWater ) THEN
C-    pLoad (Atmospheric pressure anomaly relative to surf_pRef [Pa=N/m2])
       CALL DIAGNOSTICS_FILL_RS( pLoad,   'atmPload',0,1,0,1,1,myThid )

C-    oceFreez (= heating from freezing of sea-water, if allowFreezing=T)
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL( adjustColdSST_diag,tmpFac,1,
     &                             'oceFreez',0, 1,0,1,1,myThid )

C-    surForcT (=model surface forcing for Temperature [W/m2], >0 increases T
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingT,tmpFac,1,
     &                             'surForcT',0, 1,0,1,1,myThid )

C-    surForcS (=model surface forcing for Salinity, [g/m2/s], >0 increases S
       tmpFac = rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingS,tmpFac,1,
     &                             'surForcS',0, 1,0,1,1,myThid )
      ENDIF

C-    TFLUX (=total heat flux, match heat-content variations, [W/m2])
      IF ( fluidIsWater .AND.
     &     DIAGNOSTICS_IS_ON('TFLUX   ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) =
#ifdef SHORTWAVE_HEATING
     &      -Qsw(i,j,bi,bj)+
#endif
     &      (surfaceForcingT(i,j,bi,bj)+adjustColdSST_diag(i,j,bi,bj))
     &      *HeatCapacity_Cp*rUnit2mass
          ENDDO
         ENDDO
#ifdef NONLIN_FRSURF
         IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &        .AND. useRealFreshWaterFlux ) THEN
          DO j=1,sNy
           DO i=1,sNx
            tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
     &       + PmEpR(i,j,bi,bj)*theta(i,j,ks,bi,bj)*HeatCapacity_Cp
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL( tmp1k,'TFLUX   ',0,1,0,1,1,myThid )
#ifdef ALLOW_LAYERS
       IF ( useLayers ) THEN
        CALL LAYERS_FILL( tmp1k, 1, 'SUR', 0,1,0,1,1,myThid )
       ENDIF
#endif /* ALLOW_LAYERS */
      ENDIF

C-    SFLUX (=total salt flux, match salt-content variations [g/m2/s])
      IF ( fluidIsWater .AND.
     &     DIAGNOSTICS_IS_ON('SFLUX   ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) =
     &      surfaceForcingS(i,j,bi,bj)*rUnit2mass
          ENDDO
         ENDDO

#ifdef NONLIN_FRSURF
         IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &        .AND. useRealFreshWaterFlux ) THEN
          DO j=1,sNy
           DO i=1,sNx
            tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
     &       + PmEpR(i,j,bi,bj)*salt(i,j,ks,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */

        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL( tmp1k,'SFLUX   ',0,1,0,1,1,myThid )
#ifdef ALLOW_LAYERS
       IF ( useLayers ) THEN
        CALL LAYERS_FILL( tmp1k, 2, 'SUR', 0,1,0,1,1,myThid )
       ENDIF
#endif /* ALLOW_LAYERS */
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
