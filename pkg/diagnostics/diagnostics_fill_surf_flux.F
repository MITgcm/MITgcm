C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagnostics_fill_surf_flux.F,v 1.1 2004/12/17 19:17:57 dimitri Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

      SUBROUTINE DIAGNOSTICS_FILL_SURF_FLUX( myThid )

      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#include "FFIELDS.h"

      INTEGER myThid 

#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      INTEGER i,j,K,bi,bj
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C-    Time Averages of surface fluxes
       IF ( usingPCoords ) THEN
        k=Nr
       ELSE
        k=1
       ENDIF

C-    taux (surface momentum flux [Pa=N/m2], positive <-> increase u)
      IF ( DIAGNOSTICS_IS_ON('TAUX    ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = fu(i,j,bi,bj)*foFacMom
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'TAUX    ',0,1,0,1,1,myThid)
      ENDIF

C-    tauy (surface momentum flux [Pa=N/m2], positive <-> increase v)
      IF ( DIAGNOSTICS_IS_ON('TAUY    ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy
            DO i = 1,sNx
              tmp1k(i,j,bi,bj) = fv(i,j,bi,bj)*foFacMom
            ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'TAUY    ',0,1,0,1,1,myThid)
      ENDIF

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
     &                         *theta(i,j,k,bi,bj)*HeatCapacity_Cp
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'TFLUX   ',0,1,0,1,1,myThid)
      ENDIF

C     tIce (=Heat flux [W/m2] from melt/freeze of sea-ice, positive <-> increasing Theta)
      IF ( DIAGNOSTICS_IS_ON('TICE    ',myThid) ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           tmp1k(i,j,bi,bj) = surfaceForcingTice(I,J,bi,bj)
     &      *HeatCapacity_Cp*recip_horiVertRatio*rhoConst
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'TICE    ',0,1,0,1,1,myThid)
      ENDIF

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
#ifdef NONLIN_FRSURF
         IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &        .AND. useRealFreshWaterFlux ) THEN
          DO j=1,sNy
           DO i=1,sNx
            tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
     &       + PmEpR(i,j,bi,bj)*rhoConstFresh
     &                        *salt(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'SFLUX   ',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN 
      END
