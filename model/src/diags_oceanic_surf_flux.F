C $Header: /u/gcmpack/MITgcm/model/src/diags_oceanic_surf_flux.F,v 1.3 2006/11/07 00:38:22 dimitri Exp $
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
       CALL DIAGNOSTICS_SCALE_FILL(fu,foFacMom,1,
     &                             'TAUX    ',0, 1,0,1,1,myThid)

C-    tauy (surface momentum flux [Pa=N/m2], positive <-> increase v)
       CALL DIAGNOSTICS_SCALE_FILL(fv,foFacMom,1,
     &                             'TAUY    ',0, 1,0,1,1,myThid)

C     tFlux (=Heat flux [W/m2], positive <-> increasing Theta)
      IF ( DIAGNOSTICS_IS_ON('TFLUX   ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) =
#ifdef SHORTWAVE_HEATING
     &      -Qsw(i,j,bi,bj)+
#endif
     &      (surfaceForcingT(i,j,bi,bj)+surfaceForcingTice(I,J,bi,bj))
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
       CALL DIAGNOSTICS_FILL(tmp1k,'TFLUX   ',0,1,0,1,1,myThid)
      ENDIF

#ifdef SHORTWAVE_HEATING
C-    swflux
      IF ( DIAGNOSTICS_IS_ON('SWFLUX  ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) = -Qsw(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'SWFLUX  ',0,1,0,1,1,myThid)
      ENDIF
#endif

C     tIce (=Heat flux [W/m2] from melt/freeze of sea-ice, positive <-> increasing Theta)
       tmpFac = HeatCapacity_Cp*recip_horiVertRatio*rhoConst
       CALL DIAGNOSTICS_SCALE_FILL(surfaceForcingTice,tmpFac,1,
     &                             'TICE    ',0, 1,0,1,1,myThid)

C     sFlux (=salt flux [g/m2/s], positive <-> increasing Salt)
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

Cdm PmEpR already added to surfaceForcingS in external_forcing_surf.F
Cdm#ifdef NONLIN_FRSURF
Cdm         IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
Cdm     &        .AND. useRealFreshWaterFlux ) THEN
Cdm          DO j=1,sNy
Cdm           DO i=1,sNx
Cdm            tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
Cdm     &       + PmEpR(i,j,bi,bj)*rhoConstFresh
Cdm     &                        *salt(i,j,ks,bi,bj)
Cdm           ENDDO
Cdm          ENDDO
Cdm         ENDIF
Cdm#endif /* NONLIN_FRSURF */

        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'SFLUX   ',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN 
      END
