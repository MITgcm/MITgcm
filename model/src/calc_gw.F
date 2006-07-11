C $Header: /u/gcmpack/MITgcm/model/src/calc_gw.F,v 1.30 2006/07/11 12:51:05 jmc Exp $
C     !DESCRIPTION: \bv
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GW
C     !INTERFACE:
      SUBROUTINE CALC_GW(
     I               bi, bj, KappaRU, KappaRV,
     I               myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_GW
C     | o Calculate vertical velocity tendency terms
C     |   ( Non-Hydrostatic only )
C     *==========================================================*
C     | In NH, the vertical momentum tendency must be
C     | calculated explicitly and included as a source term
C     | for a 3d pressure eqn. Calculate that term here.
C     | This routine is not used in HYD calculations.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "NH_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj   :: current tile indices
C     KappaRU :: vertical viscosity at U points
C     KappaRV :: vertical viscosity at V points
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread number for this instance of the routine.
      INTEGER bi,bj
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iMin,iMax
C     jMin,jMax
C     xA            :: W-Cell face area normal to X
C     yA            :: W-Cell face area normal to Y
C     rThickC_W     :: thickness (in r-units) of W-Cell at Western Edge
C     rThickC_S     :: thickness (in r-units) of W-Cell at Southern Edge
C     recip_rThickC :: reciprol thickness of W-Cell (centered on W-point)
C     flx_NS        :: vertical momentum flux, meridional direction
C     flx_EW        :: vertical momentum flux, zonal direction
C     flxAdvUp      :: vertical mom. advective   flux, vertical direction (@ level k-1)
C     flxDisUp      :: vertical mom. dissipation flux, vertical direction (@ level k-1)
C     flx_Dn        :: vertical momentum flux, vertical direction (@ level k)
C     gwDiss        :: vertical momentum dissipation tendency
C     i,j,k         :: Loop counters
      INTEGER iMin,iMax,jMin,jMax
      _RS    xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS    yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    rThickC_W    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    rThickC_S    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    recip_rThickC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_NS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_EW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_Dn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flxAdvUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flxDisUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gwDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gwAdd (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    del2w (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,k, kp1
      _RL  wOverride
      _RL  tmp_WbarZ
      _RL  uTrans, vTrans, rTrans
      _RL  viscLoc
      _RL  halfRL
      _RS  halfRS, zeroRS
      PARAMETER( halfRL = 0.5D0 )
      PARAMETER( halfRS = 0.5 , zeroRS = 0. )
CEOP

C Catch barotropic mode
      IF ( Nr .LT. 2 ) RETURN

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy

C--   Initialise gW to zero
      DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gW(i,j,k,bi,bj) = 0.
         ENDDO
        ENDDO
      ENDDO
C-    Initialise gwDiss to zero
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         gwDiss(i,j) = 0.
       ENDDO
      ENDDO

C--   Boundaries condition at top
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         flxAdvUp(i,j) = 0.
         flxDisUp(i,j) = 0.
       ENDDO
      ENDDO

C---  Sweep down column
      DO k=2,Nr
        kp1=k+1
        wOverRide=1.
        IF (k.EQ.Nr) THEN
          kp1=Nr
          wOverRide=0.
        ENDIF
C--   Compute grid factor arround a W-point:
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C-    note: assume fluid @ smaller k than bottom: does not work in p-coordinate !
           rThickC_W(i,j) =
     &          drF(k-1)*MAX( _hFacW(i,j,k-1,bi,bj)-halfRS, zeroRS )
     &        + drF( k )*MIN( _hFacW(i,j,k  ,bi,bj), halfRS )
           rThickC_S(i,j) =
     &          drF(k-1)*MAX( _hFacS(i,j,k-1,bi,bj)-halfRS, zeroRS )
     &        + drF( k )*MIN( _hFacS(i,j, k ,bi,bj), halfRS )
           IF ( maskC(i,j,k,bi,bj).EQ.0. ) THEN
             recip_rThickC(i,j) = 0.
           ELSE
             recip_rThickC(i,j) = 1. _d 0 /
     &        ( drF(k-1)*halfRS +
     &        + drF( k )*MIN( _hFacC(i,j, k ,bi,bj), halfRS )
     &        )
           ENDIF
C     W-Cell Western face area:
           xA(i,j) = _dyG(i,j,bi,bj)*rThickC_W(i,j)
C     W-Cell Southern face area:
           yA(i,j) = _dxG(i,j,bi,bj)*rThickC_S(i,j)
         ENDDO
        ENDDO

C--   horizontal bi-harmonic dissipation
        IF (momViscosity .AND. viscA4W.NE.0. ) THEN
C-    calculate the horizontal Laplacian of vertical flow
C     Zonal flux d/dx W
          DO j=1-Oly,sNy+Oly
           flx_EW(1-Olx,j)=0.
           DO i=1-Olx+1,sNx+Olx
            flx_EW(i,j) =
     &              (wVel(i,j,k,bi,bj)-wVel(i-1,j,k,bi,bj))
     &              *_recip_dxC(i,j,bi,bj)*xA(i,j)
#ifdef COSINEMETH_III
     &              *sqcosFacU(j,bi,bj)
#endif
           ENDDO
          ENDDO
C     Meridional flux d/dy W
          DO i=1-Olx,sNx+Olx
           flx_NS(i,1-Oly)=0.
          ENDDO
          DO j=1-Oly+1,sNy+Oly
           DO i=1-Olx,sNx+Olx
            flx_NS(i,j) =
     &              (wVel(i,j,k,bi,bj)-wVel(i,j-1,k,bi,bj))
     &              *_recip_dyC(i,j,bi,bj)*yA(i,j)
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &              *sqCosFacV(j,bi,bj)
#endif
#endif
           ENDDO
          ENDDO

C     del^2 W
C     Difference of zonal fluxes ...
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx-1
            del2w(i,j)=flx_EW(i+1,j)-flx_EW(i,j)
           ENDDO
           del2w(sNx+Olx,j)=0.
          ENDDO

C     ... add difference of meridional fluxes and divide by volume
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx
            del2w(i,j) = ( del2w(i,j)
     &                    +(flx_NS(i,j+1)-flx_NS(i,j))
     &                   )*recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
           ENDDO
          ENDDO
C-- No-slip BCs impose a drag at walls...
CML ************************************************************
CML   No-slip Boundary conditions for bi-harmonic dissipation
CML   need to be implemented here!
CML ************************************************************
        ELSE
C-    Initialize del2w to zero:
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            del2w(i,j) = 0. _d 0
           ENDDO
          ENDDO
        ENDIF

        IF (momViscosity) THEN
C Viscous Flux on Western face
          DO j=jMin,jMax
           DO i=iMin,iMax+1
             flx_EW(i,j)=
     &       - (viscAh_W(i,j,k,bi,bj)+viscAh_W(i-1,j,k,bi,bj))*halfRL
     &              *(wVel(i,j,k,bi,bj)-wVel(i-1,j,k,bi,bj))
c    &              *_recip_dxC(i,j,bi,bj)*xA(i,j)
     &              *_recip_dxC(i,j,bi,bj)*rThickC_W(i,j)
     &       + (viscA4_W(i,j,k,bi,bj)+viscA4_W(i-1,j,k,bi,bj))*halfRL
     &              *(del2w(i,j)-del2w(i-1,j))
c    &              *_recip_dxC(i,j,bi,bj)*xA(i,j)
     &              *_recip_dxC(i,j,bi,bj)*rThickC_W(i,j)
#ifdef COSINEMETH_III
     &              *sqCosFacU(j,bi,bj)
#else
     &              *CosFacU(j,bi,bj)
#endif
           ENDDO
          ENDDO
C Viscous Flux on Southern face
          DO j=jMin,jMax+1
           DO i=iMin,iMax
             flx_NS(i,j)=
     &       - (viscAh_W(i,j,k,bi,bj)+viscAh_W(i,j-1,k,bi,bj))*halfRL
     &              *(wVel(i,j,k,bi,bj)-wVel(i,j-1,k,bi,bj))
c    &              *_recip_dyC(i,j,bi,bj)*yA(i,j)
     &              *_recip_dyC(i,j,bi,bj)*rThickC_S(i,j)
     &       + (viscA4_W(i,j,k,bi,bj)+viscA4_W(i,j-1,k,bi,bj))*halfRL
     &              *(del2w(i,j)-del2w(i,j-1))
c    &              *_recip_dyC(i,j,bi,bj)*yA(i,j)
     &              *_recip_dyC(i,j,bi,bj)*rThickC_S(i,j)
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &              *sqCosFacV(j,bi,bj)
#else
     &              *CosFacV(j,bi,bj)
#endif
#endif
           ENDDO
          ENDDO
C Viscous Flux on Lower face of W-Cell (= at tracer-cell center, level k)
          DO j=jMin,jMax
           DO i=iMin,iMax
C     Interpolate vert viscosity to center of tracer-cell (level k):
             viscLoc = ( KappaRU(i,j,k)  +KappaRU(i+1,j,k)
     &                  +KappaRU(i,j,kp1)+KappaRU(i+1,j,kp1)
     &                  +KappaRV(i,j,k)  +KappaRV(i,j+1,k)
     &                  +KappaRV(i,j,kp1)+KappaRV(i,j+1,kp1)
     &                 )*0.125 _d 0
             flx_Dn(i,j) =
     &          - viscLoc*( wVel(i,j,kp1,bi,bj)*wOverRide
     &                     -wVel(i,j, k ,bi,bj) )*rkSign
c    &                   *recip_drF(k)*rA(i,j,bi,bj)
     &                   *recip_drF(k)
           ENDDO
          ENDDO
C     Tendency is minus divergence of viscous fluxes:
          DO j=jMin,jMax
           DO i=iMin,iMax
c            gwDiss(i,j) =
c    &        -(   ( flx_EW(i+1,j)-flx_EW(i,j) )
c    &           + ( flx_NS(i,j+1)-flx_NS(i,j) )
c    &           + ( flx_Dn(i,j)-flxDisUp(i,j) )*rkSign
c    &         )*recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
             gwDiss(i,j) =
     &        -(
     &          +_recip_dxF(i,j,bi,bj)*( flx_EW(i+1,j)-flx_EW(i,j) )
     &          +_recip_dyF(i,j,bi,bj)*( flx_NS(i,j+1)-flx_NS(i,j) )
     &          +                      ( flxDisUp(i,j)-flx_Dn(i,j) )
     &         )*recip_rThickC(i,j)
c    &         )*recip_drC(k)
C--        prepare for next level (k+1)
             flxDisUp(i,j)=flx_Dn(i,j)
           ENDDO
          ENDDO
        ENDIF

        IF (no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
c         CALL MOM_W_SIDEDRAG(
c    I        bi,bj,k,
c    O        gwAdd,
c    I        myThid)
c         DO j=jMin,jMax
c          DO i=iMin,iMax
c           gwDiss(i,j) = gwDiss(i,j) + gwAdd(i,j)
c          ENDDO
c         ENDDO
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( momAdvection ) THEN
C Advective Flux on Western face
          DO j=jMin,jMax
           DO i=iMin,iMax+1
C     transport through Western face area:
             uTrans = (
     &          drF(k-1)*_hFacW(i,j,k-1,bi,bj)*uVel(i,j,k-1,bi,bj)
     &        + drF( k )*_hFacW(i,j, k ,bi,bj)*uVel(i,j, k ,bi,bj)
c    &                )*halfRL*_dyG(i,j,bi,bj)
     &                )*halfRL
             flx_EW(i,j)=
     &         uTrans*(wVel(i,j,k,bi,bj)+wVel(i-1,j,k,bi,bj))*halfRL
           ENDDO
          ENDDO
C Advective Flux on Southern face
          DO j=jMin,jMax+1
           DO i=iMin,iMax
C     transport through Southern face area:
             vTrans = (
     &          drF(k-1)*_hFacS(i,j,k-1,bi,bj)*vVel(i,j,k-1,bi,bj)
     &         +drF( k )*_hFacS(i,j, k ,bi,bj)*vVel(i,j, k ,bi,bj)
c    &                )*halfRL*_dxG(i,j,bi,bj)
     &                )*halfRL
             flx_NS(i,j)=
     &         vTrans*(wVel(i,j,k,bi,bj)+wVel(i,j-1,k,bi,bj))*halfRL
           ENDDO
          ENDDO
C Advective Flux on Lower face of W-Cell (= at tracer-cell center, level k)
          DO j=jMin,jMax
           DO i=iMin,iMax
             tmp_WbarZ = halfRL*( wVel(i,j, k ,bi,bj)
     &                           +wVel(i,j,kp1,bi,bj)*wOverRide )
C     transport through Lower face area:
             rTrans = tmp_WbarZ*rA(i,j,bi,bj)
c            flx_Dn(i,j) = rTrans*tmp_WbarZ
             flx_Dn(i,j) = tmp_WbarZ*tmp_WbarZ
           ENDDO
          ENDDO
C     Tendency is minus divergence of advective fluxes:
          DO j=jMin,jMax
           DO i=iMin,iMax
c            gW(i,j,k,bi,bj) =
c    &        -(   ( flx_EW(i+1,j)-flx_EW(i,j) )
c    &           + ( flx_NS(i,j+1)-flx_NS(i,j) )
c    &           + ( flx_Dn(i,j)-flxAdvUp(i,j) )*rkSign
c    &         )*recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
             gW(i,j,k,bi,bj) =
     &        -(
     &          +_recip_dxF(i,j,bi,bj)*( flx_EW(i+1,j)-flx_EW(i,j) )
     &          +_recip_dyF(i,j,bi,bj)*( flx_NS(i,j+1)-flx_NS(i,j) )
     &          +                      ( flxAdvUp(i,j)-flx_Dn(i,j) )
     &         )*recip_rThickC(i,j)
c    &         )*recip_drC(k)
C--          prepare for next level (k+1)
             flxAdvUp(i,j)=flx_Dn(i,j)
           ENDDO
          ENDDO
        ENDIF

        IF ( useNHMTerms ) THEN
          CALL MOM_W_METRIC_NH(
     I               bi,bj,k,
     I               uVel, vVel,
     O               gwAdd,
     I               myThid )
          DO j=jMin,jMax
           DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)+gwAdd(i,j)
           ENDDO
          ENDDO
        ENDIF
        IF ( useCoriolis ) THEN
          CALL MOM_W_CORIOLIS_NH(
     I               bi,bj,k,
     I               uVel, vVel,
     O               gwAdd,
     I               myThid )
          DO j=jMin,jMax
           DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)+gwAdd(i,j)
           ENDDO
          ENDDO
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Dissipation term inside the Adams-Bashforth:
        IF ( momViscosity .AND. momDissip_In_AB) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)+gwDiss(i,j)
           ENDDO
          ENDDO
        ENDIF

C-    Compute effective gW_[n+1/2] terms (including Adams-Bashforth weights)
C     and save gW_[n] into gwNm1 for the next time step.
c#ifdef ALLOW_ADAMSBASHFORTH_3
c       CALL ADAMS_BASHFORTH3(
c    I                         bi, bj, k,
c    U                         gW, gwNm,
c    I                         momStartAB, myIter, myThid )
c#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL ADAMS_BASHFORTH2(
     I                         bi, bj, k,
     U                         gW, gwNm1,
     I                         myIter, myThid )
c#endif /* ALLOW_ADAMSBASHFORTH_3 */

C--   Dissipation term outside the Adams-Bashforth:
        IF ( momViscosity .AND. .NOT.momDissip_In_AB ) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)+gwDiss(i,j)
           ENDDO
          ENDDO
        ENDIF

C-    end of the k loop
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
