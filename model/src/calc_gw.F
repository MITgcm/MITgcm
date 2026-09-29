#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif
#define CALC_GW_NEW_THICK

CBOP
C     !ROUTINE: CALC_GW
C     !INTERFACE:
      SUBROUTINE CALC_GW(
     I               bi, bj, kappaRU, kappaRV,
     I               str13, str23, str33,
     I               viscAh3d_00, viscAh3d_13, viscAh3d_23,
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
#include "RESTART.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "NH_VARS.h"
#ifdef ALLOW_ADDFLUID
# include "FFIELDS.h"
#endif
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj   :: current tile indices
C     kappaRU :: vertical viscosity at U points
C     kappaRV :: vertical viscosity at V points
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread number for this instance of the routine.
      INTEGER bi,bj
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
#ifdef ALLOW_SMAG_3D
      _RL str13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str33(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_00(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
#else /* ALLOW_SMAG_3D */
      _RL str13(1), str23(1), str33(1)
      _RL viscAh3d_00(1), viscAh3d_13(1), viscAh3d_23(1)
#endif /* ALLOW_SMAG_3D */
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
#ifdef ALLOW_MOM_COMMON

C     !LOCAL VARIABLES:
C     == Local variables ==
C     biharmonicVisc:: use horizontal biharmonic viscosity for vertical momentum
C     iMin, iMax    :: Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     xA            :: W-Cell face area normal to X
C     yA            :: W-Cell face area normal to Y
C     rThickC_W     :: thickness (in r-units) of W-Cell at Western Edge
C     rThickC_S     :: thickness (in r-units) of W-Cell at Southern Edge
C     rThickC_C     :: thickness (in r-units) of W-Cell (centered on W pt)
C     recip_rThickC :: reciprocal thickness of W-Cell (centered on W-point)
C     flx_NS        :: vertical momentum flux, meridional direction
C     flx_EW        :: vertical momentum flux, zonal direction
C     flxAdvUp      :: vertical mom. advective   flux, vertical direction (@ level k-1)
C     flxDisUp      :: vertical mom. dissipation flux, vertical direction (@ level k-1)
C     flx_Dn        :: vertical momentum flux, vertical direction (@ level k)
C     gwDiss        :: vertical momentum dissipation tendency
C     gwAdd         :: other tendencies (Coriolis, Metric-terms)
C     gw_AB         :: tendency increment from Adams-Bashforth
C     del2w         :: laplacian of wVel
C     wFld          :: local copy of wVel
C     i,j,k         :: Loop counters
      LOGICAL biharmonicVisc
      INTEGER iMin,iMax,jMin,jMax
      _RS    xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS    yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    rThickC_W    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    rThickC_S    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    rThickC_C    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    recip_rThickC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_NS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_EW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flx_Dn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flxAdvUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    flxDisUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gwDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gwAdd (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gw_AB (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    del2w (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,k, km1, kp1
      _RL  mskM1, mskP1
      _RL  tmp_WbarZ
      _RL  uTrans, vTrans, rTrans
      _RL  viscLoc
      PARAMETER( iMin = 1 , iMax = sNx )
      PARAMETER( jMin = 1 , jMax = sNy )
CEOP
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL diagDiss, diagAdvec, diagMetric, diag_AB
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        diagDiss  = DIAGNOSTICS_IS_ON( 'Wm_Diss ', myThid )
        diagAdvec = DIAGNOSTICS_IS_ON( 'Wm_Advec', myThid )
        diagMetric= DIAGNOSTICS_IS_ON( 'Wm_Metr ', myThid )
        diag_AB   = DIAGNOSTICS_IS_ON( 'AB_gW   ', myThid )
      ELSE
        diagDiss  = .FALSE.
        diagAdvec = .FALSE.
        diagMetric= .FALSE.
        diag_AB   = .FALSE.
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      biharmonicVisc = viscA4W.NE.zeroRL
     &           .OR. ( useVariableVisc .AND. useBiharmonicVisc )

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
      IF (momViscosity) THEN
C-    Initialize del2w to zero:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           del2w(i,j) = 0. _d 0
         ENDDO
        ENDDO
      ENDIF

C--   Boundaries condition at top (vertical advection of vertical momentum):
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         flxAdvUp(i,j) = 0.
c        flxDisUp(i,j) = 0.
       ENDDO
      ENDDO

C---  Sweep down column
      DO k=1,Nr
        km1 = MAX( k-1, 1 )
        kp1 = MIN( k+1,Nr )
        mskM1 = 1.
        mskP1 = 1.
        IF ( k.EQ. 1 ) mskM1 = 0.
        IF ( k.EQ.Nr ) mskP1 = 0.
        IF ( k.GT.1 ) THEN
C--   Compute grid factor arround a W-point:
#ifdef CALC_GW_NEW_THICK
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( maskC(i,j,k-1,bi,bj).EQ.0. .OR.
     &          maskC(i,j, k ,bi,bj).EQ.0. ) THEN
             recip_rThickC(i,j) = 0.
           ELSE
C-    valid in z & p coord.; also accurate if Interface @ middle between 2 centers
             recip_rThickC(i,j) = 1. _d 0 /
     &        (  MIN( Ro_surf(i,j,bi,bj),rC(k-1) )
     &         - MAX( R_low(i,j,bi,bj),  rC(k)   )
     &        )
           ENDIF
          ENDDO
         ENDDO
         IF (momViscosity) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rThickC_C(i,j) = MAX( zeroRS,
     &                           MIN( Ro_surf(i,j,bi,bj), rC(k-1) )
     &                          -MAX(   R_low(i,j,bi,bj),  rC(k)  )
     &                         )
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           rThickC_W(i,j) = MAX( zeroRS,
     &                           MIN( rSurfW(i,j,bi,bj), rC(k-1) )
     &                          -MAX(  rLowW(i,j,bi,bj), rC(k)   )
     &                         )
C     W-Cell Western face area:
           xA(i,j) = _dyG(i,j,bi,bj)*rThickC_W(i,j)
c    &                              *deepFacF(k)
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rThickC_S(i,j) = MAX( zeroRS,
     &                           MIN( rSurfS(i,j,bi,bj), rC(k-1) )
     &                          -MAX(  rLowS(i,j,bi,bj), rC(k)   )
     &                         )
C     W-Cell Southern face area:
           yA(i,j) = _dxG(i,j,bi,bj)*rThickC_S(i,j)
c    &                              *deepFacF(k)
C deep-model: xA,yA is only used for viscous flux, in terms like: xA/dxC,yA/dyC.
C this gives deepFacF*recip_deepFacF => cancel each other (and therefore omitted)
          ENDDO
         ENDDO
         ENDIF
#else /* CALC_GW_NEW_THICK */
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C-    note: assume fluid @ smaller k than bottom: does not work in p-coordinate !
           IF ( maskC(i,j,k,bi,bj).EQ.0. ) THEN
             recip_rThickC(i,j) = 0.
           ELSE
             recip_rThickC(i,j) = 1. _d 0 /
     &        ( drF(k-1)*halfRS
     &        + drF( k )*MIN( _hFacC(i,j, k ,bi,bj), halfRS )
     &        )
           ENDIF
c          IF (momViscosity) THEN
#ifdef NONLIN_FRSURF
           rThickC_C(i,j) =
     &          drF(k-1)*MAX( h0FacC(i,j,k-1,bi,bj)-halfRS, zeroRS )
     &        + drF( k )*MIN( h0FacC(i,j,k  ,bi,bj), halfRS )
#else
           rThickC_C(i,j) =
     &          drF(k-1)*MAX( _hFacC(i,j,k-1,bi,bj)-halfRS, zeroRS )
     &        + drF( k )*MIN( _hFacC(i,j,k  ,bi,bj), halfRS )
#endif
           rThickC_W(i,j) =
     &          drF(k-1)*MAX( _hFacW(i,j,k-1,bi,bj)-halfRS, zeroRS )
     &        + drF( k )*MIN( _hFacW(i,j,k  ,bi,bj), halfRS )
           rThickC_S(i,j) =
     &          drF(k-1)*MAX( _hFacS(i,j,k-1,bi,bj)-halfRS, zeroRS )
     &        + drF( k )*MIN( _hFacS(i,j, k ,bi,bj), halfRS )
C     W-Cell Western face area:
           xA(i,j) = _dyG(i,j,bi,bj)*rThickC_W(i,j)
c    &                              *deepFacF(k)
C     W-Cell Southern face area:
           yA(i,j) = _dxG(i,j,bi,bj)*rThickC_S(i,j)
c    &                              *deepFacF(k)
C deep-model: xA,yA is only used for viscous flux, in terms like: xA/dxC,yA/dyC.
C this gives deepFacF*recip_deepFacF => cancel each other (and therefore omitted)
c          ENDIF
          ENDDO
         ENDDO
#endif /* CALC_GW_NEW_THICK */
        ELSEIF ( selectNHfreeSurf.GE.1 ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           recip_rThickC(i,j) = recip_drC(k)
c          rThickC_C(i,j) = drC(k)
c          rThickC_W(i,j) = drC(k)
c          rThickC_S(i,j) = drC(k)
c          xA(i,j) = _dyG(i,j,bi,bj)*drC(k)
c          yA(i,j) = _dxG(i,j,bi,bj)*drC(k)
          ENDDO
         ENDDO
        ENDIF

C--   local copy of wVel:
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            wFld(i,j) = wVel(i,j,k,bi,bj)
          ENDDO
        ENDDO

C--   horizontal bi-harmonic dissipation
        IF ( momViscosity .AND. k.GT.1 .AND. biharmonicVisc ) THEN

C-    calculate the horizontal Laplacian of vertical flow
C     Zonal flux d/dx W
          IF ( useCubedSphereExchange ) THEN
C     to compute d/dx(W), fill corners with appropriate values:
            CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                                 wFld, bi,bj, myThid )
          ENDIF
          DO j=1-OLy,sNy+OLy
           flx_EW(1-OLx,j)=0.
           DO i=1-OLx+1,sNx+OLx
            flx_EW(i,j) =
     &               ( wFld(i,j) - wFld(i-1,j) )
     &              *_recip_dxC(i,j,bi,bj)*xA(i,j)
#ifdef COSINEMETH_III
     &              *sqCosFacU(j,bi,bj)
#endif
#ifdef ALLOW_OBCS
     &              *maskInW(i,j,bi,bj)
#endif
           ENDDO
          ENDDO

C     Meridional flux d/dy W
          IF ( useCubedSphereExchange ) THEN
C     to compute d/dy(W), fill corners with appropriate values:
            CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                                 wFld, bi,bj, myThid )
          ENDIF
          DO i=1-OLx,sNx+OLx
           flx_NS(i,1-OLy)=0.
          ENDDO
          DO j=1-OLy+1,sNy+OLy
           DO i=1-OLx,sNx+OLx
            flx_NS(i,j) =
     &               ( wFld(i,j) - wFld(i,j-1) )
     &              *_recip_dyC(i,j,bi,bj)*yA(i,j)
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &              *sqCosFacV(j,bi,bj)
#endif
#endif
#ifdef ALLOW_OBCS
     &              *maskInS(i,j,bi,bj)
#endif
           ENDDO
          ENDDO

C     del^2 W
C     Divergence of horizontal fluxes
          DO j=1-OLy,sNy+OLy-1
           DO i=1-OLx,sNx+OLx-1
            del2w(i,j) = ( ( flx_EW(i+1,j)-flx_EW(i,j) )
     &                    +( flx_NS(i,j+1)-flx_NS(i,j) )
     &                   )*recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
     &                    *recip_deepFac2F(k)
           ENDDO
          ENDDO
C end if biharmonic viscosity
        ENDIF

        IF ( momViscosity .AND. k.GT.1 ) THEN
C Viscous Flux on Western face
          DO j=jMin,jMax
           DO i=iMin,iMax+1
             flx_EW(i,j)=
     &       - (viscAh_W(i,j,k,bi,bj)+viscAh_W(i-1,j,k,bi,bj))*halfRL
     &              *(wVel(i,j,k,bi,bj)-wVel(i-1,j,k,bi,bj))
     &              *_recip_dxC(i,j,bi,bj)*xA(i,j)
     &              *cosFacU(j,bi,bj)
     &       + (viscA4_W(i,j,k,bi,bj)+viscA4_W(i-1,j,k,bi,bj))*halfRL
     &              *(del2w(i,j)-del2w(i-1,j))
     &              *_recip_dxC(i,j,bi,bj)*xA(i,j)
#ifdef COSINEMETH_III
     &              *sqCosFacU(j,bi,bj)
#else
     &              *cosFacU(j,bi,bj)
#endif
           ENDDO
          ENDDO
C Viscous Flux on Southern face
          DO j=jMin,jMax+1
           DO i=iMin,iMax
             flx_NS(i,j)=
     &       - (viscAh_W(i,j,k,bi,bj)+viscAh_W(i,j-1,k,bi,bj))*halfRL
     &              *(wVel(i,j,k,bi,bj)-wVel(i,j-1,k,bi,bj))
     &              *_recip_dyC(i,j,bi,bj)*yA(i,j)
#ifdef ISOTROPIC_COS_SCALING
     &              *cosFacV(j,bi,bj)
#endif
     &       + (viscA4_W(i,j,k,bi,bj)+viscA4_W(i,j-1,k,bi,bj))*halfRL
     &              *(del2w(i,j)-del2w(i,j-1))
     &              *_recip_dyC(i,j,bi,bj)*yA(i,j)
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &              *sqCosFacV(j,bi,bj)
#else
     &              *cosFacV(j,bi,bj)
#endif
#endif
           ENDDO
          ENDDO
C Viscous Flux on Lower face of W-Cell (= at tracer-cell center, level k)
          DO j=jMin,jMax
           DO i=iMin,iMax
C     Interpolate vert viscosity to center of tracer-cell (level k):
             viscLoc = ( kappaRU(i,j,k)  +kappaRU(i+1,j,k)
     &                  +kappaRU(i,j,k+1)+kappaRU(i+1,j,k+1)
     &                  +kappaRV(i,j,k)  +kappaRV(i,j+1,k)
     &                  +kappaRV(i,j,k+1)+kappaRV(i,j+1,k+1)
     &                 )*0.125 _d 0
             flx_Dn(i,j) =
     &          - viscLoc*( wVel(i,j,kp1,bi,bj)*mskP1
     &                     -wVel(i,j, k ,bi,bj) )*rkSign
     &                   *recip_drF(k)*rA(i,j,bi,bj)
     &                   *deepFac2C(k)*rhoFacC(k)
           ENDDO
          ENDDO
          IF ( k.EQ.2 ) THEN
C Viscous Flux on Upper face of W-Cell (= at tracer-cell center, level k-1)
           DO j=jMin,jMax
            DO i=iMin,iMax
C     Interpolate horizontally (but not vertically) vert viscosity to center:
C     Although background visc. might be defined at k=1, this is not
C     generally true when using variable visc. (from vertical mixing scheme).
C     Therefore, no vert. interp. and only horizontal interpolation.
             viscLoc = ( kappaRU(i,j,k) + kappaRU(i+1,j,k)
     &                  +kappaRV(i,j,k) + kappaRV(i,j+1,k)
     &                 )*0.25 _d 0
             flxDisUp(i,j) =
     &          - viscLoc*( wVel(i,j, k ,bi,bj)
     &                     -wVel(i,j,k-1,bi,bj) )*rkSign
     &                   *recip_drF(k-1)*rA(i,j,bi,bj)
     &                   *deepFac2C(k-1)*rhoFacC(k-1)
C to recover old (before 2009/11/30) results (since flxDisUp(k=2) was zero)
c            flxDisUp(i,j) = 0.
            ENDDO
           ENDDO
          ENDIF
C     Tendency is minus divergence of viscous fluxes:
C     anelastic: vert.visc.flx is scaled by rhoFac but hor.visc.fluxes are not
          DO j=jMin,jMax
           DO i=iMin,iMax
             gwDiss(i,j) =
     &        -(   ( flx_EW(i+1,j)-flx_EW(i,j) )
     &           + ( flx_NS(i,j+1)-flx_NS(i,j) )
     &           + ( flx_Dn(i,j)-flxDisUp(i,j) )*rkSign
     &                                          *recip_rhoFacF(k)
     &         )*recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
     &          *recip_deepFac2F(k)
C--        prepare for next level (k+1)
             flxDisUp(i,j)=flx_Dn(i,j)
           ENDDO
          ENDDO
        ENDIF

        IF ( momViscosity .AND. k.GT.1 .AND. no_slip_sides ) THEN
C-     No-slip BCs impose a drag at walls...
          CALL MOM_W_SIDEDRAG(
     I               bi,bj,k,
     I               wVel, del2w,
     I               rThickC_C, recip_rThickC,
     I               viscAh_W, viscA4_W,
     O               gwAdd,
     I               myThid )
          DO j=jMin,jMax
           DO i=iMin,iMax
            gwDiss(i,j) = gwDiss(i,j) + gwAdd(i,j)
           ENDDO
          ENDDO
        ENDIF

#ifdef ALLOW_SMAG_3D
        IF ( useSmag3D .AND. k.GT.1 ) THEN
             CALL MOM_W_SMAG_3D(
     I         str13, str23, str33,
     I         viscAh3d_00, viscAh3d_13, viscAh3d_23,
     I         rThickC_W, rThickC_S, rThickC_C, recip_rThickC,
     O         gwAdd,
     I         iMin,iMax,jMin,jMax, k, bi, bj, myThid )
          DO j = jMin,jMax
           DO i = iMin,iMax
            gwDiss(i,j) = gwDiss(i,j) + gwAdd(i,j)
           ENDDO
          ENDDO
        ENDIF
#endif /* ALLOW_SMAG_3D */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( momAdvection ) THEN

         IF ( k.GT.1 .OR. selectNHfreeSurf.GE.1 ) THEN
C Advective Flux on Western face
          DO j=jMin,jMax
           DO i=iMin,iMax+1
C     transport through Western face area:
             uTrans = (
     &          drF(km1)*_hFacW(i,j,km1,bi,bj)*uVel(i,j,km1,bi,bj)
     &                  *rhoFacC(km1)*mskM1
     &        + drF( k )*_hFacW(i,j, k ,bi,bj)*uVel(i,j, k ,bi,bj)
     &                  *rhoFacC(k)
     &                )*halfRL*_dyG(i,j,bi,bj)*deepFacF(k)
             flx_EW(i,j) = uTrans*(wFld(i,j)+wFld(i-1,j))*halfRL
c            flx_EW(i,j)=
c    &         uTrans*(wVel(i,j,k,bi,bj)+wVel(i-1,j,k,bi,bj))*halfRL
           ENDDO
          ENDDO
C Advective Flux on Southern face
          DO j=jMin,jMax+1
           DO i=iMin,iMax
C     transport through Southern face area:
             vTrans = (
     &          drF(km1)*_hFacS(i,j,km1,bi,bj)*vVel(i,j,km1,bi,bj)
     &                  *rhoFacC(km1)*mskM1
     &         +drF( k )*_hFacS(i,j, k ,bi,bj)*vVel(i,j, k ,bi,bj)
     &                  *rhoFacC(k)
     &                )*halfRL*_dxG(i,j,bi,bj)*deepFacF(k)
             flx_NS(i,j) = vTrans*(wFld(i,j)+wFld(i,j-1))*halfRL
c            flx_NS(i,j)=
c    &         vTrans*(wVel(i,j,k,bi,bj)+wVel(i,j-1,k,bi,bj))*halfRL
           ENDDO
          ENDDO
         ENDIF
C Advective Flux on Lower face of W-Cell (= at tracer-cell center, level k)
c        IF (.TRUE.) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
C     NH in p-coord.: advect wSpeed [m/s] with rTrans
             tmp_WbarZ = halfRL*
     &              ( wVel(i,j, k ,bi,bj)*rVel2wUnit( k )
     &               +wVel(i,j,kp1,bi,bj)*rVel2wUnit(kp1)*mskP1 )
C     transport through Lower face area:
             rTrans = halfRL*
     &              ( wVel(i,j, k ,bi,bj)*deepFac2F( k )*rhoFacF( k )
     &               +wVel(i,j,kp1,bi,bj)*deepFac2F(kp1)*rhoFacF(kp1)
     &                                   *mskP1
     &              )*rA(i,j,bi,bj)
             flx_Dn(i,j) = rTrans*tmp_WbarZ
           ENDDO
          ENDDO
c        ENDIF
         IF ( k.EQ.1 .AND. selectNHfreeSurf.GE.1 ) THEN
C Advective Flux on Upper face of W-Cell (= at surface)
           DO j=jMin,jMax
            DO i=iMin,iMax
             tmp_WbarZ = wVel(i,j,k,bi,bj)*rVel2wUnit(k)
             rTrans = wVel(i,j,k,bi,bj)*deepFac2F(k)*rhoFacF(k)
     &               *rA(i,j,bi,bj)
             flxAdvUp(i,j) = rTrans*tmp_WbarZ
c            flxAdvUp(i,j) = 0.
            ENDDO
           ENDDO
         ENDIF

         IF ( k.GT.1 .OR. selectNHfreeSurf.GE.1 ) THEN
C     Tendency is minus divergence of advective fluxes:
C     anelastic: all transports & advect. fluxes are scaled by rhoFac
          DO j=jMin,jMax
           DO i=iMin,iMax
C to recover old (before 2009/11/30) results (since flxAdvUp(k=2) was zero)
c            IF (k.EQ.2) flxAdvUp(i,j) = 0.
             gW(i,j,k,bi,bj) =
     &        -(   ( flx_EW(i+1,j)-flx_EW(i,j) )
     &           + ( flx_NS(i,j+1)-flx_NS(i,j) )
     &           + ( flx_Dn(i,j)-flxAdvUp(i,j) )*rkSign*wUnit2rVel(k)
     &         )*recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
     &          *recip_deepFac2F(k)*recip_rhoFacF(k)
           ENDDO
          ENDDO
#ifdef ALLOW_ADDFLUID
          IF ( selectAddFluid.GE.1 ) THEN
           DO j=jMin,jMax
            DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)
     &        + wVel(i,j,k,bi,bj)*mass2rUnit*0.5 _d 0
     &          *( addMass(i,j,k,bi,bj)
     &            +addMass(i,j,km1,bi,bj)*mskM1 )
     &          *recip_rA(i,j,bi,bj)*recip_rThickC(i,j)
     &          *recip_deepFac2F(k)*recip_rhoFacF(k)
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_ADDFLUID */
         ENDIF

         DO j=jMin,jMax
           DO i=iMin,iMax
C--          prepare for next level (k+1)
             flxAdvUp(i,j)=flx_Dn(i,j)
           ENDDO
         ENDDO

c       ELSE
C-    if momAdvection / else
c         DO j=1-OLy,sNy+OLy
c          DO i=1-OLx,sNx+OLx
c            gW(i,j,k,bi,bj) = 0. _d 0
c          ENDDO
c         ENDDO

C-    endif momAdvection.
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( useNHMTerms .AND. k.GT.1 ) THEN
          CALL MOM_W_METRIC_NH(
     I               bi, bj, k,
     I               uVel, vVel,
     O               gwAdd,
     I               myThid )
          DO j=jMin,jMax
           DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)+gwAdd(i,j)
           ENDDO
          ENDDO
#ifdef ALLOW_DIAGNOSTICS
          IF ( diagMetric ) THEN
           CALL DIAGNOSTICS_FILL( gwAdd, 'Wm_Metr ',
     I                            k,1,2, bi,bj, myThid )
C- note: need to explicitly increment the counter since DIAGNOSTICS_FILL
C        does it only if k=1 (never the case here)
           IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT('Wm_Metr ',bi,bj,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF
        IF ( select3dCoriScheme.GE.1 .AND. k.GT.1 ) THEN
          CALL MOM_W_CORIOLIS_NH(
     I               bi, bj, k,
     I               uVel, vVel, recip_rThickC,
     O               gwAdd,
     I               myThid )
          DO j=jMin,jMax
           DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)+gwAdd(i,j)
           ENDDO
          ENDDO
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
        IF ( diagDiss )  THEN
          CALL DIAGNOSTICS_FILL( gwDiss, 'Wm_Diss ',
     &                           k, 1, 2, bi,bj, myThid )
c         IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT('Wm_Diss ',bi,bj,myThid)
        ENDIF
        IF ( diagAdvec ) THEN
          CALL DIAGNOSTICS_FILL( gW, 'Wm_Advec',
     &                           k,Nr, 1, bi,bj, myThid )
c         IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT('Wm_Advec',bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

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
#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL ADAMS_BASHFORTH3(
     I                         bi, bj, k, Nr,
     U                         gW(1-OLx,1-OLy,1,bi,bj), gwNm,
     O                         gw_AB,
     I                         nHydStartAB, myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
        CALL ADAMS_BASHFORTH2(
     I                         bi, bj, k, Nr,
     U                         gW(1-OLx,1-OLy,1,bi,bj),
     U                         gwNm1(1-OLx,1-OLy,1,bi,bj),
     O                         gw_AB,
     I                         nHydStartAB, myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
#ifdef ALLOW_DIAGNOSTICS
        IF ( diag_AB ) THEN
          CALL DIAGNOSTICS_FILL(gw_AB,'AB_gW   ',k,1,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

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

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(viscAh_W,'VISCAHW ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(viscA4_W,'VISCA4W ',0,Nr,1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MOM_COMMON */
#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
