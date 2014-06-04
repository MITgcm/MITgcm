#include "LAYERS_OPTIONS.h"
C--  File layers_thermodynamics.F:
C--   Contents
C--   o LAYERS_CALC_RHS

CBOP 0
C     !ROUTINE: LAYERS_CALC_RHS
C     !INTERFACE:
      SUBROUTINE LAYERS_CALC_RHS(
     I                  myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_CALC_RHS
C     | Recalculate the divergence of the RHS terms in T and S eqns.
C     | Replaces the values of layers_surfflux, layers_df? IN PLACE
C     | with the corresponding tendencies (same units as GT and GS)
C     *==========================================================*
C     \ev

C !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
      INTEGER myThid
      INTEGER iLa
CEOP

#ifdef ALLOW_LAYERS

C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid 
C     kdown    :: temporary placeholder
C     fluxfac  :: scaling factor for converting surface flux to tendency
C     fluxfac  :: scaling factor for converting diffusive flux to tendency 
C     downfac  :: mask for lower point

      INTEGER bi, bj
      INTEGER i,j,k,kdown,iTracer                             
      _RL fluxfac(2), downfac, tmpfac

C --  These factors convert the units of TFLUX and SFLUX diagnostics
C --  back to surfaceForcingT and surfaceForcingS units
      fluxfac(1) = 1.0/(HeatCapacity_Cp*rUnit2mass)
      fluxfac(2) = 1.0/rUnit2mass

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid) 
          DO iTracer = 1,2 
           k = 1
C --       Loop for surface fluxes             
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             layers_surfflux(i,j,k,iTracer,bi,bj) =
     &       layers_surfflux(i,j,k,iTracer,bi,bj)
     &       *recip_drF(1)*_recip_hFacC(i,j,1,bi,bj) 
     &       *fluxfac(iTracer)
            ENDDO
           ENDDO 
C --       Loop for diffusive fluxes
C --       If done correctly, we can overwrite the flux array in place
C --       with its own divergence
           DO k=1,Nr
            kdown= MIN(k+1,Nr)
            IF (k.EQ.Nr) THEN
              downfac = 0. _d 0
            ELSE
              downfac = 1. _d 0
            ENDIF 
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              tmpfac = -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &          *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
              layers_dfx(i,j,k,iTracer,bi,bj) = maskInC(i,j,bi,bj) *
     &         tmpfac * ( layers_dfx(i+1,j,k,iTracer,bi,bj) - 
     &          layers_dfx(i,j,k,iTracer,bi,bj) )
              layers_dfy(i,j,k,iTracer,bi,bj) = maskInC(i,j,bi,bj) *
     &         tmpfac * ( layers_dfy(i,j+1,k,iTracer,bi,bj) - 
     &          layers_dfy(i,j,k,iTracer,bi,bj) )     
              layers_dfr(i,j,k,iTracer,bi,bj) = tmpfac * rkSign *
     &        ( layers_dfr(i,j,kdown,iTracer,bi,bj)*downfac -
     &          layers_dfr(i,j,k,iTracer,bi,bj) )
             ENDDO
            ENDDO                                 
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C-    TFLUX (=total heat flux, match heat-content variations, [W/m2])
C       IF ( fluidIsWater .AND.
C      &     DIAGNOSTICS_IS_ON('TFLUX   ',myThid) ) THEN
C        DO bj = myByLo(myThid), myByHi(myThid)
C         DO bi = myBxLo(myThid), myBxHi(myThid)
C          DO j = 1,sNy
C           DO i = 1,sNx
C            tmp1k(i,j,bi,bj) =
C #ifdef SHORTWAVE_HEATING
C      &      -Qsw(i,j,bi,bj)+
C #endif
C      &      (surfaceForcingT(i,j,bi,bj)+surfaceForcingTice(i,j,bi,bj))
C      &      *HeatCapacity_Cp*rUnit2mass
C           ENDDO
C          ENDDO
C #ifdef NONLIN_FRSURF
C          IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
C      &        .AND. useRealFreshWaterFlux ) THEN
C           DO j=1,sNy
C            DO i=1,sNx
C             tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
C      &       + PmEpR(i,j,bi,bj)*theta(i,j,ks,bi,bj)*HeatCapacity_Cp
C            ENDDO
C           ENDDO
C          ENDIF
C #endif /* NONLIN_FRSURF */
C         ENDDO
C        ENDDO
C        CALL DIAGNOSTICS_FILL( tmp1k,'TFLUX   ',0,1,0,1,1,myThid )
C       ENDIF
C 
C C-    SFLUX (=total salt flux, match salt-content variations [g/m2/s])
C       IF ( fluidIsWater .AND.
C      &     DIAGNOSTICS_IS_ON('SFLUX   ',myThid) ) THEN
C        DO bj = myByLo(myThid), myByHi(myThid)
C         DO bi = myBxLo(myThid), myBxHi(myThid)
C          DO j = 1,sNy
C           DO i = 1,sNx
C            tmp1k(i,j,bi,bj) =
C      &      surfaceForcingS(i,j,bi,bj)*rUnit2mass
C           ENDDO
C          ENDDO
C 
C #ifdef NONLIN_FRSURF
C          IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
C      &        .AND. useRealFreshWaterFlux ) THEN
C           DO j=1,sNy
C            DO i=1,sNx
C             tmp1k(i,j,bi,bj) = tmp1k(i,j,bi,bj)
C      &       + PmEpR(i,j,bi,bj)*salt(i,j,ks,bi,bj)
C            ENDDO
C           ENDDO
C          ENDIF
C #endif /* NONLIN_FRSURF */
C 
C         ENDDO
C        ENDDO
C        CALL DIAGNOSTICS_FILL( tmp1k,'SFLUX   ',0,1,0,1,1,myThid )
C       ENDIF


C     Ocean: Add temperature surface forcing (e.g., heat-flux) in surface level
C      IF ( kLev .EQ. kSurface ) THEN
C       DO j=1,sNy
C        DO i=1,sNx
C          gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
C     &      +surfaceForcingT(i,j,bi,bj)
C     &      *recip_drF(kLev)*_recip_hFacC(i,j,kLev,bi,bj)
C        ENDDO
C       ENDDO
C      ELSEIF ( kSurface.EQ.-1 ) THEN
C       DO j=1,sNy
C        DO i=1,sNx
C         IF ( kSurfC(i,j,bi,bj).EQ.kLev ) THEN
C          gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
C     &      +surfaceForcingT(i,j,bi,bj)
C     &      *recip_drF(kLev)*_recip_hFacC(i,j,kLev,bi,bj)
C         ENDIF
C        ENDDO
C       ENDDO
C      ENDIF


C--   Divergence of fluxes
C     Anelastic: scale vertical fluxes by rhoFac and leave Horizontal fluxes unchanged
C     for Stevens OBC: keep only vertical diffusive contribution on boundaries
C     DO j=1-OLy,sNy+OLy-1
C      DO i=1-OLx,sNx+OLx-1
C       gTracer(i,j,k,bi,bj)=gTracer(i,j,k,bi,bj)
C    &   -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
C    &   *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
C    &   *( (fZon(i+1,j)-fZon(i,j))*maskInC(i,j,bi,bj)
C    &     +(fMer(i,j+1)-fMer(i,j))*maskInC(i,j,bi,bj)
C    &     +(fVerT(i,j,kDown)-fVerT(i,j,kUp))*rkSign
C    &     -localT(i,j)*( (uTrans(i+1,j)-uTrans(i,j))*advFac
C    &                   +(vTrans(i,j+1)-vTrans(i,j))*advFac
C    &                   +(rTransKp1(i,j)-rTrans(i,j))*rAdvFac
C    &                  )*maskInC(i,j,bi,bj)
C    &    )
C      ENDDO
C     ENDDO          


#endif /* LAYERS_THERMODYNAMICS */
      RETURN
      END
C -- end of S/R LAYERS_CALC_RHS
