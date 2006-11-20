C $Header: /u/gcmpack/MITgcm/model/src/diags_oceanic_surf_flux.F,v 1.5 2006/11/20 04:24:34 jmc Exp $
C $Name:  $

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
C     !LOCAL VARIABLES:
C     i,j,bi,bj :: loop indices
C     ks        :: surface level index
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
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

C-    taux (surface momentum flux [Pa=N/m2], positive <-> increase u)
       CALL DIAGNOSTICS_SCALE_FILL( fu,foFacMom,1,
     &                             'oceTAUX ',0, 1,0,1,1,myThid )

C-    tauy (surface momentum flux [Pa=N/m2], positive <-> increase v)
       CALL DIAGNOSTICS_SCALE_FILL( fv,foFacMom,1,
     &                             'oceTAUY ',0, 1,0,1,1,myThid )

#ifdef ATMOSPHERIC_LOADING
C-    pload (Atmospheric pressure loading [Pa=N/m2])
       CALL DIAGNOSTICS_FILL( pload,   'atmPload',0,1,0,1,1,myThid )

C-    sea-ice loading (expressed in Mass of ice+snow / area unit, [kg/m2])
       CALL DIAGNOSTICS_FILL( sIceLoad,'sIceLoad',0,1,0,1,1,myThid )
#endif /* ATMOSPHERIC_LOADING */

C-    net Fresh Water flux into the ocean (+=down), [kg/m2/s]
       tmpFac = -rhoConstFresh
       CALL DIAGNOSTICS_SCALE_FILL( EmPmR,tmpFac,1,
     &                             'oceFWflx',0, 1,0,1,1,myThid )

C-    net Salt flux into the ocean (+=down), [psu.kg/m2/s ~ g/m2/s]
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL( saltFlux,tmpFac,1,
     &                             'oceSflux',0, 1,0,1,1,myThid )

C-    Qnet (= net heat flux into the ocean, +=down, [W/m2])
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL( Qnet,tmpFac,1,
     &                             'oceQnet ',0, 1,0,1,1,myThid )

#ifdef SHORTWAVE_HEATING
C-    Qsw (= net short-wave into the ocean, +=down, [W/m2])
       tmpFac = -1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL( Qsw,tmpFac,1,
     &                             'oceQsw  ',0, 1,0,1,1,myThid )
#endif

C-    oceFreez (= heating from freezing of sea-water, if allowFreezing=T)
       tmpFac = HeatCapacity_Cp*recip_horiVertRatio*rhoConst
       CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingTice,tmpFac,1,
     &                             'oceFreez',0, 1,0,1,1,myThid )

C-    surForcT (=model surface forcing for Temperature [W/m2], >0 increases T
       tmpFac = HeatCapacity_Cp*recip_horiVertRatio*rhoConst
       CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingT,tmpFac,1,
     &                             'surForcT',0, 1,0,1,1,myThid )

C-    surForcS (=model surface forcing for Salinity, [g/m2/s], >0 increases S
       tmpFac = recip_horiVertRatio*rhoConst
       CALL DIAGNOSTICS_SCALE_FILL( surfaceForcingS,tmpFac,1,
     &                             'surForcS',0, 1,0,1,1,myThid )

C-    TFLUX (=total heat flux, match heat-content variations, [W/m2])
      IF ( DIAGNOSTICS_IS_ON('TFLUX   ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) =
#ifdef SHORTWAVE_HEATING
     &      -Qsw(i,j,bi,bj)+
#endif
     &      (surfaceForcingT(i,j,bi,bj)+surfaceForcingTice(i,j,bi,bj))
     &      *HeatCapacity_Cp*recip_horiVertRatio*rhoConst
          ENDDO
         ENDDO
#ifdef NONLIN_FRSURF
         IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &        .AND. useRealFreshWaterFlux ) THEN
          DO j=1,sNy
           DO i=1,sNx
            tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
     &       + PmEpR(i,j,bi,bj)*rhoConstFresh
     &                         *theta(i,j,ks,bi,bj)*HeatCapacity_Cp
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL( tmp1k,'TFLUX   ',0,1,0,1,1,myThid )
      ENDIF

C-    SFLUX (=total salt flux, match salt-content variations [g/m2/s])
      IF ( DIAGNOSTICS_IS_ON('SFLUX   ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) =
     &      surfaceForcingS(i,j,bi,bj)*
     &      recip_horiVertRatio*rhoConst
          ENDDO
         ENDDO

#ifdef NONLIN_FRSURF
         IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &        .AND. useRealFreshWaterFlux ) THEN
          DO j=1,sNy
           DO i=1,sNx
            tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
     &       + PmEpR(i,j,bi,bj)*rhoConstFresh
     &                        *salt(i,j,ks,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */

        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL( tmp1k,'SFLUX   ',0,1,0,1,1,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
