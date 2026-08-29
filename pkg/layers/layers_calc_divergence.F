#include "LAYERS_OPTIONS.h"

CBOP 0
C     !ROUTINE: LAYERS_CALC_DIVERGENCE
C     !INTERFACE:
      SUBROUTINE LAYERS_CALC_DIVERGENCE(
     I                  myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_CALC_DIVERGENCE
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
#include "FFIELDS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_LAYERS
# ifdef LAYERS_THERMODYNAMICS
C !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     i,j      :: horizontal indices
C     k        :: vertical index for model grid
C     kp1      :: vertical index plus one
C     maskKp1  :: mask for kp1 level
C     fluxFac  :: scaling factor for converting surface flux to tendency
C     locVar   :: local 2-D array for horizontal (1-D) flux divergence
C     recVol   :: local 2-D array for volume reciprocal

      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, kp1, iTracer
      _RL fluxFac(2), maskKp1
      _RL locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recVol(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C --  These factors convert the units of TFLUX and SFLUX diagnostics
C --  back to surfaceForcingT and surfaceForcingS units
      fluxFac(1) = mass2rUnit/HeatCapacity_Cp
      fluxFac(2) = mass2rUnit

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO iTracer = 1,2

          k = 1
C    <==  THIS BELOW LOOKS WRONG for Ocean in P-Pcoords case (surface not at k=1)
C --      Loop for surface fluxes
#ifdef SHORTWAVE_HEATING
C --      Have to remove the shortwave from the surface flux because it is added later
          IF ( selectPenetratingSW.GT.0 .AND. iTracer.EQ.1 ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              layers_surfflux(i,j,k,iTracer,bi,bj) =
     &           layers_surfflux(i,j,k,iTracer,bi,bj)
C --      Sign convention for Qsw means we have to add it to subtract it
     &         + Qsw(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDIF
#endif /* SHORTWAVE_HEATING */
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             layers_surfflux(i,j,k,iTracer,bi,bj) =
     &       layers_surfflux(i,j,k,iTracer,bi,bj)
     &       *recip_drF(1)*_recip_hFacC(i,j,1,bi,bj)
     &       *fluxFac(iTracer)
           ENDDO
          ENDDO

C --      Loop for diffusive fluxes
C --      If done correctly, we can overwrite the vertical flux array in place
C --      with its own divergence
          DO k=1,Nr
            maskKp1 = 1. _d 0
            kp1 = MIN( k+1, Nr )
            IF ( k.EQ.Nr ) maskKp1 = 0. _d 0
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              recVol(i,j) = _recip_hFacC(i,j,k,bi,bj) * recip_drF(k)
     &                    * recip_rA(i,j,bi,bj) * recip_deepFac2C(k)
     &                    * recip_rhoFacC(k) * maskInC(i,j,bi,bj)
             ENDDO
            ENDDO

C -- Diffusion
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              locVar(i,j) = layers_dfx(i+1,j,k,iTracer,bi,bj)
     &                    - layers_dfx( i ,j,k,iTracer,bi,bj)
             ENDDO
            ENDDO
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              layers_dfx(i,j,k,iTracer,bi,bj) = -locVar(i,j)*recVol(i,j)
              locVar(i,j) = layers_dfy(i,j+1,k,iTracer,bi,bj)
     &                    - layers_dfy(i, j ,k,iTracer,bi,bj)
             ENDDO
            ENDDO
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              layers_dfy(i,j,k,iTracer,bi,bj) = -locVar(i,j)*recVol(i,j)
              locVar(i,j) = layers_dfr(i,j,kp1,iTracer,bi,bj)*maskKp1
     &                    - layers_dfr(i,j,k,iTracer,bi,bj)
              layers_dfr(i,j,k,iTracer,bi,bj) = -rkSign
     &                                          *locVar(i,j)*recVol(i,j)
             ENDDO
            ENDDO
C -- Advection
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              locVar(i,j) = layers_afx(i+1,j,k,iTracer,bi,bj)
     &                    - layers_dfx( i ,j,k,iTracer,bi,bj)
             ENDDO
            ENDDO
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              layers_afx(i,j,k,iTracer,bi,bj) = -locVar(i,j)*recVol(i,j)
              locVar(i,j) = layers_afy(i,j+1,k,iTracer,bi,bj)
     &                    - layers_afy(i, j ,k,iTracer,bi,bj)
             ENDDO
            ENDDO
            DO j=1-OLy,sNy+OLy-1
             DO i=1-OLx,sNx+OLx-1
              layers_afy(i,j,k,iTracer,bi,bj) = -locVar(i,j)*recVol(i,j)
              locVar(i,j) = layers_afr(i,j,kp1,iTracer,bi,bj)*maskKp1
     &                    - layers_afr(i,j,k,iTracer,bi,bj)
              layers_afr(i,j,k,iTracer,bi,bj) = -rkSign
     &                                          *locVar(i,j)*recVol(i,j)
             ENDDO
            ENDDO

#ifdef SHORTWAVE_HEATING
            IF ( selectPenetratingSW.GT.0 .AND. iTracer.EQ.1 ) THEN
             DO j=1-OLy,sNy+OLy-1
              DO i=1-OLx,sNx+OLx-1
                layers_sw(i,j,k,iTracer,bi,bj) =
     &            layers_sw(i,j,k,iTracer,bi,bj)
     &            + Qsw(i,j,bi,bj)*gravitySign
     &            *( SWFrac3D(i,j,k,bi,bj) - SWFrac3D(i,j,k+1,bi,bj) )
     &            *fluxFac(1)
     &            *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
              ENDDO
             ENDDO
            ENDIF
#endif /* SHORTWAVE_HEATING */

C--    end k loop
          ENDDO

C--     end iTracer, bi,bj loops
        ENDDO
       ENDDO
      ENDDO

CML   I am guessing that this copy of the original diagnostics code
CML   is here only for reference:
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

# endif /* LAYERS_THERMODYNAMICS */
#endif /* ALLOW_LAYERS */
      RETURN
      END
