C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_mom_rhs.F,v 1.42 2000/03/24 16:03:03 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CALC_MOM_RHS                                         |
C     | o Form the right hand-side of the momentum equation.     |
C     |==========================================================|
C     | Terms are evaluated one layer at a time working from     |
C     | the bottom to the top. The vertically integrated         |
C     | barotropic flow tendency term is evluated by summing the |
C     | tendencies.                                              |
C     | Notes:                                                   |
C     | We have not sorted out an entirely satisfactory formula  |
C     | for the diffusion equation bc with lopping. The present  |
C     | form produces a diffusive flux that does not scale with  |
C     | open-area. Need to do something to solidfy this and to   |
C     | deal "properly" with thin walls.                         |
C     \==========================================================/
      SUBROUTINE CALC_MOM_RHS( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I        xA,yA,uTrans,vTrans,rTrans,rVel,maskC,
     I        phi_hyd,KappaRU,KappaRV,
     U        aFArg,vFArg,cFArg,mTArg,pFArg,
     U        fZonArg, fMerArg, fVerU, fVerV,
     I        myCurrentTime, myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"

C     == Routine arguments ==
C     fZon    - Work array for flux of momentum in the east-west
C               direction at the west face of a cell.
C     fMer    - Work array for flux of momentum in the north-south
C               direction at the south face of a cell.
C     fVerU   - Flux of momentum in the vertical
C     fVerV     direction out of the upper face of a cell K
C               ( flux into the cell above ).
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     rTrans  - Vertical volume transport through cell upper face
C     rVel    - Vertical velocities at upper (kUp) and lower cell faces (kDown)
C     phi_hyd - Hydrostatic pressure
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     kUp, kDown, kM1                - Index for upper and lower layers.
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      _RS      xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS   maskC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL   fVerU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL   fVerV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL      phi_hyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL      KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL      KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL      aFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      cFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mTArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZonArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMerArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kUp,kDown,kM1
      INTEGER myThid
      _RL     myCurrentTime
      INTEGER bi,bj,iMin,iMax,jMin,jMax,jG

C     == Local variables ==
C     ab15, ab05    - Weights for Adams-Bashforth time stepping scheme.
C     i,j,k         - Loop counters
C     wMaskOverride - Land sea flag override for top layer.
C     afFacMom      - Tracer parameters for turning terms
C     vfFacMom        on and off.
C     pfFacMom        afFacMom - Advective terms 
C     cfFacMom        vfFacMom - Eddy viscosity terms
C     mTFacMom        pfFacMom - Pressure terms
C                     cfFacMom - Coriolis terms
C                     foFacMom - Forcing
C                     mTFacMom - Metric term
C     vF            - Temporary holding viscous term (Laplacian)
C     v4F           - Temporary holding viscous term (Biharmonic)
C     cF            - Temporary holding coriolis term.
C     mT            - Temporary holding metric terms(s).
C     pF            - Temporary holding pressure|potential gradient terms.
C     uDudxFac, AhDudxFac, etc ... individual term tracer parameters
      _RL      aF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      v4F(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      cF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mT (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZon (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMer (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS    hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     I,J,K - Loop counters
      INTEGER i,j,k
C     rVelMaskOverride - Factor for imposing special surface boundary conditions
C                        ( set according to free-surface condition ).
C     hFacROpen        - Lopped cell factos used tohold fraction of open
C     hFacRClosed        and closed cell wall.
      _RL  rVelMaskOverride
      _RS  hFacZOpen
      _RS  hFacZClosedN
      _RS  hFacZClosedS
      _RS  hFacZClosedE
      _RS  hFacZClosedW
C     xxxFac - On-off tracer parameters used for switching terms off.
      _RL  uDudxFac
      _RL  AhDudxFac
      _RL  A4DuxxdxFac
      _RL  vDudyFac
      _RL  AhDudyFac
      _RL  A4DuyydyFac
      _RL  rVelDudrFac
      _RL  ArDudrFac
      _RL  fuFac
      _RL  phxFac
      _RL  mtFacU
      _RL  uDvdxFac
      _RL  AhDvdxFac
      _RL  A4DvxxdxFac
      _RL  vDvdyFac
      _RL  AhDvdyFac
      _RL  A4DvyydyFac
      _RL  rVelDvdrFac
      _RL  ArDvdrFac
      _RL  fvFac
      _RL  phyFac
      _RL  vForcFac
      _RL  mtFacV
C     ab05, ab15 - Adams-Bashforth time-stepping weights.
      _RL  ab05, ab15
      _RL cosfacU(1-Oly:sNy+Oly)
      _RL cosfacV(1-Oly:sNy+Oly)

      rVelMaskOverride=1.
      IF ( k .EQ. 1 ) rVelMaskOverride=freeSurfFac
C     Initialise intermediate terms
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        aF(i,j)   = 0.
        vF(i,j)   = 0.
        v4F(i,j)  = 0.
        cF(i,j)   = 0.
        mT(i,j)   = 0.
        pF(i,j)   = 0.
        fZon(i,j) = 0.
        fMer(i,j) = 0.
       ENDDO
      ENDDO
C     COS(latitude) at U points and V points
      DO J=1-OLy,sNy+OLy
       jG = myYGlobalLo + (bj-1)*sNy + J-1
       jG = min(max(1,jG),Ny)
       IF (cosPower.NE.0.) THEN
        cosFacU(J) = COS( yC(1,J,bi,bj) *deg2rad )
     &                  ** cosPower
        cosFacV(J) = COS( (yC(1,J,bi,bj)-0.5*delY(jG)) *deg2rad )
     &                  ** cosPower
       ELSE
        cosFacU(J) = 1.
        cosFacV(J) = 1.
       ENDIF
      ENDDO
C--   Term by term tracer parmeters
C     o U momentum equation
      uDudxFac     = afFacMom*1.
      AhDudxFac    = vfFacMom*1.
      A4DuxxdxFac  = vfFacMom*1.
      vDudyFac     = afFacMom*1.
      AhDudyFac    = vfFacMom*1.
      A4DuyydyFac  = vfFacMom*1.
      rVelDudrFac  = afFacMom*1.
      ArDudrFac    = vfFacMom*1.
      mTFacU       = mtFacMom*1.
      fuFac        = cfFacMom*1.
      phxFac       = pfFacMom*1.
C     o V momentum equation
      uDvdxFac     = afFacMom*1.
      AhDvdxFac    = vfFacMom*1.
      A4DvxxdxFac  = vfFacMom*1.
      vDvdyFac     = afFacMom*1.
      AhDvdyFac    = vfFacMom*1.
      A4DvyydyFac  = vfFacMom*1.
      rVelDvdrFac  = afFacMom*1.
      ArDvdrFac    = vfFacMom*1.
      mTFacV       = mtFacMom*1.
      fvFac        = cfFacMom*1.
      phyFac       = pfFacMom*1.
      vForcFac     = foFacMom*1.

      IF (implicitViscosity) THEN
       ArDudrFac = 0.
       ArDvdrFac = 0.
      ENDIF

C--   Adams-Bashforth weighting factors
      ab15   =  1.5 _d 0 + abEps
      ab05   = -0.5 _d 0 - abEps
  
C--   Calculate open water fraction at vorticity points
      DO i=1-Olx,sNx+Olx
       hFacZ(i,1-Oly)=0.
      ENDDO
      DO j=2-Oly,sNy+Oly
       hFacZ(1-Olx,j)=0.
       DO i=2-Olx,sNx+Olx
        hFacZOpen=min(_hFacW(i,j,k,bi,bj),
     &                _hFacW(i,j-1,k,bi,bj))
        hFacZOpen=min(_hFacS(i,j,k,bi,bj),hFacZOpen)
        hFacZOpen=min(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
        hFacZ(i,j)=hFacZOpen
       ENDDO
      ENDDO

C---- Zonal momentum equation starts here

#ifdef INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
C--   del^2 U (for use in del^4 U)
C     Zonal flux d/dx U
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        fZon(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &   *_dyF(i,j,bi,bj)
     &   *_recip_dxF(i,j,bi,bj)
     &   *(uVel(i+1,j,k,bi,bj)-uVel(i,j,k,bi,bj))
       ENDDO
      ENDDO
C     Meridional flux d/dy U
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = drF(k)*hFacZ(i,j)
     &   *_dxV(i,j,bi,bj)
     &   *_recip_dyU(i,j,bi,bj)
     &   *(uVel(i,j,k,bi,bj)-uVel(i,j-1,k,bi,bj))
       ENDDO
      ENDDO
C     del^2 U
      DO j=0,sNy+1
       DO i=0,sNx+2
        v4F(i,j) = recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
     &  /rAw(i,j,bi,bj)
     &  *( fZon(i,j  )    - fZon(i-1,j)
     &    +fMer(i,j+1)    - fMer(i  ,j)
     &   )*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO
      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=0,sNy+1
       DO i=0,sNx+2
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
        v4F(i,j) = v4F(i,j)
     &  - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   /rAw(i,j,bi,bj)
     &   *( hFacZClosedS*dxV(i, j ,bi,bj)
     &     *_recip_dyU(i, j ,bi,bj)
     &     +hFacZClosedN*dxV(i,j+1,bi,bj)
     &     *_recip_dyU(i,j+1,bi,bj)
     &    )*drF(k)*2.*uVel(i,j,k,bi,bj)
     &     *_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO
      ENDIF
#endif /* INCLUDE_BH_MOMENTUM_DIFFUSION_CODE */

C---  Calculate mean and eddy fluxes between cells for zonal flow.
C--   Zonal flux (fZon is at east face of "u" cell)
#define _XAatP(    i,j,k,bi,bj) ( XA(i,j)+XA(i+1,j) )* 0.5 _d 0
#define _YAatUBY(  i,j,k,bi,bj) ( YA(i,j)+YA(i-1,j) )* 0.5 _d 0
#define _RAatU1(    i,j,k,bi,bj) ( rA(i,j,bi,bj)*maskC(i,j)
#define _RAatU2(    i,j,k,bi,bj) + rA(i-1,j,bi,bj)
#define _RAatU3(    i,j,k,bi,bj) *maskC(i-1,j) ) * 0.5 _d 0
#define _RAatU_Bot(i,j,k,bi,bj) ( rA(i,j,bi,bj) + rA(i-1,j,bi,bj) ) * 0.5 _d 0
C     o Mean flow component of zonal flux
#ifdef  INCLUDE_MOMENTUM_ADVECTION_CODE
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (uTrans(i,j)+uTrans(i+1,j) )
     &  *(uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))*0.25 _d 0
       ENDDO
      ENDDO
#endif /* INCLUDE_MOMENTUM_ADVECTION_CODE */
C     o Eddy component of zonal flux
C     - Laplacian  and bi-harmonic terms
      DO j=jMin,jMax
       DO i=iMin,iMax
        vF(i,j) =
#ifdef OLD_UV_GEOMETRY
     &    ( XA(i,j)+XA(i+1,j) )* 0.5 _d 0
#else
     &    _dyF(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
#endif /* OLD_UV_GEOMETRY */
     &     *(
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
     &       -viscAh*(uVel(i+1,j,k,bi,bj)-uVel(i,j,k,bi,bj))*cosFacU(J)
#else
     &        0.
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */
#ifdef INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
     &       +viscA4*( v4F(i+1,j)        -v4F(i,j)         )*cosFacU(J)
#endif /* INCLUDE_BH_MOMENTUM_DIFFUSION_CODE */
     &      )*_recip_dxF(i,j,bi,bj)

       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = 0.
     &  _ADM(     + uDudxFac    * aF (i,j)    )
     &            + AhDudxFac   * vF (i,j)
       ENDDO
      ENDDO

C--   Meridional flux (fMer is at south face of "u" cell)
#ifdef  INCLUDE_MOMENTUM_ADVECTION_CODE
C     o Mean flow component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (vTrans(i,j)+vTrans(i-1,j))
     &  *(uVel(i,j,k,bi,bj)+uVel(i,j-1,k,bi,bj))*0.25 _d 0
#ifdef OLD_ADV_BCS
     &  *_maskW(i,j,k,bi,bj)
     &  *_maskW(i,j-1,k,bi,bj)
#endif /* OLD_ADV_BCS */
C       Note!!!! The line "*maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
#endif /* INCLUDE_MOMENTUM_ADVECTION_CODE */
C     Eddy component of meridional flux
C     o Laplacian and bi-harmonic term
      DO j=jMin,jMax
       DO i=iMin,iMax
        vF(i,j) = _dxV(i,j,bi,bj)*drF(k)*hFacZ(i,j)
     &     *(
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
     &       -viscAh*(uVel(i,j,k,bi,bj)-uVel(i,j-1,k,bi,bj))*cosFacV(J)
#else
     &        0.
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */
#ifdef INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
     &       +viscA4*( v4F(i,j)        -v4F(i,j-1)         )*cosFacV(J)
#endif /* INCLUDE_BH_MOMENTUM_DIFFUSION_CODE */
     &      )
     &       *_recip_dyU(i,j,bi,bj)
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = 0.
     &  _ADM(      + vDudyFac    * aF (i,j)    )
     &             + AhDudyFac   * vF (i,j)
       ENDDO
      ENDDO
C--   Vertical flux (fVer is at upper face of "u" cell)
#ifdef INCLUDE_MOMENTUM_ADVECTION_CODE
C     Mean flow component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   rVelMaskOverride*(rTrans(i,j)+rTrans(i-1,j))
     &  *( uVel(i,j,k,bi,bj)+uVel(i,j,kM1,bi,bj) )
     &  *0.25 _d 0
#ifdef OLD_ADV_BCS
     &  *_maskW(i,j,k,bi,bj)
     &  *_maskW(i,j,kM1,bi,bj)
#endif /* OLD_ADV_BCS */
C       Note!!!! The line "*(maskW(i,j,k,bi,bj)*maskW(i,j,kM1,bi,bj))" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
#endif /* INCLUDE_MOMENTUM_ADVECTION_CODE */
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#ifdef OLD_UV_GEOMETRY
#define _RA_AT_W (RA(i,j,bi,bj)*maskC(i,j)+RA(i-1,j,bi,bj)*maskC(i-1,j))*.5
#else
#define _RA_AT_W rAw(i,j,bi,bj)
#endif /* OLD_UV_GEOMETRY */
C     Eddy component of vertical flux
      IF (no_slip_bottom) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &   -KappaRU(i,j,k)
     & *_RA_AT_W
     &   *( ( 2.-_maskW(i,j,k,bi,bj) )*uVel(i,j,kM1,bi,bj)
     &     -     _maskW(i,j,k,bi,bj)  *uVel(i,j,k,bi,bj)
     &    )*rkFac*recip_drC(k)
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &   -KappaRU(i,j,k)
     & *_RA_AT_W
     &   *( uVel(i,j,km1,bi,bj)-uVel(i,j,k,bi,bj)
     &    )*rkFac*recip_drC(k)
     &    *_maskW(i,j,k,bi,bj)*rVelMaskOverride
        ENDDO
       ENDDO
      ENDIF
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerU(i,j,kUp) = 0.
     &  _ADM(           + rVelDudrFac  * af(i,j)     )
     &  _LPM(           +    ArDudrFac * vf(i,j)     )
       ENDDO
      ENDDO
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#ifdef OLD_UV_GEOMETRY
#define _RA_AT_W2 (RA(i,j,bi,bj)+RA(i-1,j,bi,bj))*0.5 _d 0
#else
#define _RA_AT_W2 rAw(i,j,bi,bj)
#endif /* OLD_UV_GEOMETRY */
C     Bottom boundary condition
      IF ( no_slip_bottom .AND. k .EQ. Nr ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &    -KappaRU(i,j,k)
     & *_RA_AT_W2
     &    *_maskW(i,j,k,bi,bj)
     &    *uVel(i,j,k,bi,bj)*rkFac*recip_DrF(k)*2. _d 0
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerU(i,j,kDown) = ArDudrFac * vf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */

#ifdef INCLUDE_GRADPH_CODE
C---  Hydrostatic term ( -1/rhoConst . dphi/dx )
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = - _recip_dxC(i,j,bi,bj)
     &   *(phi_hyd(i,j,k)-phi_hyd(i-1,j,k))*recip_rhoConst
       ENDDO
      ENDDO
#endif /* INCLUDE_GRADPH_CODE */

C--   Tendency is minus divergence of the fluxes + coriolis + pressure 
C--   term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) =
#ifdef OLD_UV_GEOM
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)/
     &    ( 0.5 _d 0*(rA(i,j,bi,bj)+rA(i-1,j,bi,bj)) )
#else
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &    /rAw(i,j,bi,bj)
#endif
     &  *(fZon(i,j  )          - fZon(i-1,j)
     &   +fMer(i,j+1)          - fMer(i  ,j)
     &   +fVerU(i,j,kUp)*rkFac - fVerU(i,j,kDown)*rkFac
     &   )
     & _PHM( +phxFac * pf(i,j) )
       ENDDO
      ENDDO

#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=jMin,jMax
       DO i=iMin,iMax
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &   -_recip_hFacW(i,j,k,bi,bj)
     &   *recip_drF(k)/rAw(i,j,bi,bj)
     &   *( hFacZClosedS*_dxV(i, j ,bi,bj)
     &      *_recip_dyU(i, j ,bi,bj)
     &     +hFacZClosedN*_dxV(i,j+1,bi,bj)
     &      *_recip_dyU(i,j+1,bi,bj) )
     &   *drF(k)*2.*( viscAh*uVel(i,j,k,bi,bj)
     &               -viscA4*v4F(i,j) ) *cosFacU(J)
       ENDDO
      ENDDO
      ENDIF
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */

#ifdef INCLUDE_MOMENTUM_FORCING_CODE
C--   Forcing term
      CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myCurrentTime,myThid)
#endif /* INCLUDE_MOMENTUM_FORCING_CODE */

#ifdef INCLUDE_MOMENTUM_METRIC_TERM_CODE
C--   Metric terms for curvilinear grid systems
      IF ( usingSphericalPolarMTerms ) THEN
C      o Spherical polar grid metric terms
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = -1.* uVel(i,j,k,bi,bj)*recip_RSphere
     &    *0.25 _d 0*( rVel(i-1,j,kUp  )+rVel(i  ,j,kUp  )
     &                +rVel(i-1,j,KDown)+rVel(i  ,j,KDown)
     &              )*rkFac*recip_horiVertRatio
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = mT(i,j)+uVel(i,j,k,bi,bj)*recip_RSphere
     &    *0.25 _d 0*( vVel(i,j  ,k,bi,bj)+vVel(i-1,j  ,k,bi,bj)
     &                +vVel(i,j+1,k,bi,bj)+vVel(i-1,j+1,k,bi,bj)
     &              )*_tanPhiAtU(i,j,bi,bj)
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+
     &    mTFacU*mT(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif /* INCLUDE_MOMENTUM_METRIC_TERM_CODE */

C--   Set du/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

C---- Meridional momentum equation starts here

#ifdef INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
C--   del^2 V (for use in del^4 V)
C     Zonal flux d/dx V
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        fZon(i,j) = drF(k)*hFacZ(i,j)
     &   *_dyU(i,j,bi,bj)
     &   *_recip_dxV(i,j,bi,bj)
     &   *(vVel(i,j,k,bi,bj)-vVel(i-1,j,k,bi,bj))
       ENDDO
      ENDDO
C     Meridional flux d/dy V
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &   *_dxF(i,j,bi,bj)
     &   *_recip_dyF(i,j,bi,bj)
     &   *(vVel(i,j+1,k,bi,bj)-vVel(i,j,k,bi,bj))
       ENDDO
      ENDDO
C     del^2 U
      DO j=0,sNy+2
       DO i=0,sNx+1
        v4F(i,j) = recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
     &  /rAs(i,j,bi,bj)
     &  *( fZon(i+1,j)    - fZon(i, j )
     &    +fMer( i ,j)    - fMer(i,j-1)
     &   )*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=0,sNy+2
       DO i=0,sNx+1
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        v4F(i,j) = v4F(i,j)
     &  -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &  /rAs(i,j,bi,bj)
     &   *( hFacZClosedW*dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj)
     &    )*drF(k)*2.*vVel(i,j,k,bi,bj)
     &      *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
      ENDIF
#endif /* INCLUDE_BH_MOMENTUM_DIFFUSION_CODE */

C---  Calculate mean and eddy fluxes between cells for meridional 
C---  velocity. Zonal flux (fZon is at west face of "v" cell)
#define _XAatVBX(  i,j,k,bi,bj) ( XA(i,j)+XA(i,j-1) )* 0.5 _d 0
#define _YAatP(    i,j,k,bi,bj) ( YA(i,j) + YA(i,j+1) ) *0.5 _d 0
#define _RAatV1(    i,j,k,bi,bj) ( _rA(i,j,bi,bj)*maskC(i,j)
#define _RAatV2(    i,j,k,bi,bj) + _rA(i,j-1,bi,bj)
#define _RAatV3(    i,j,k,bi,bj) *maskC(i,j-1) ) * 0.5 _d 0
#define _RAatV_Bot1(i,j,k,bi,bj) ( _rA(i,j,bi,bj)
#define _RAatV_Bot2(i,j,k,bi,bj)  +_rA(i,j-1,bi,bj))*0.5 _d 0
#ifdef INCLUDE_MOMENTUM_ADVECTION_CODE
C     o Mean flow component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (uTrans(i,j)+uTrans(i,j-1) )
     &  *(vVel(i,j,k,bi,bj)+vVel(i-1,j,k,bi,bj))*0.25 _d 0
#ifdef OLD_ADV_BCS
     &  *_maskS(i,j,k,bi,bj)*_maskS(i-1,j,k,bi,bj)
#endif /* OLD_ADV_BCS */
C       Note!!!! The line "*maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
#endif /* INCLUDE_MOMENTUM_ADVECTION_CODE */
C     Eddy component of zonal flux
C     o Laplacian term
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) = _dyU(i,j,bi,bj)*drF(K)*hFacZ(i,j)
     &     *(
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
     &       -viscAh*(vVel(i,j,k,bi,bj)-vVel(i-1,j,k,bi,bj))*cosFacV(J)
#else
     &        0.
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */
#ifdef INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
     &       +viscA4*(v4F(i,j)         -v4F(i-1,j)         )*cosFacV(J)
#endif /* INCLUDE_BH_MOMENTUM_DIFFUSION_CODE */
     &      )*_recip_dxV(i,j,bi,bj)
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = 0.
     &  _ADM(     + uDvdxFac    * aF (i,j)    )
     &            + AhDvdxFac   * vF (i,j)
       ENDDO
      ENDDO
C--   Meridional flux (fMer is at north face of "v" cell)
#ifdef INCLUDE_MOMENTUM_ADVECTION_CODE
C     o Mean flow component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (vTrans(i,j)+vTrans(i,j+1))
     &  *(vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))*0.25 _d 0
       ENDDO
      ENDDO
#endif /* INCLUDE_MOMENTUM_ADVECTION_CODE */
C     Eddy component of meridional flux
C     o Laplacian term
      DO j=jMin,jMax
       DO i=iMin,iMax
        vF(i,j) = 
#ifdef OLD_UV_GEOMETRY
     & ( YA(i,j) + YA(i,j+1) ) *0.5 _d 0
#else
     & _dxF(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
#endif /* OLD_UV_GEOMETRY */
     &     *( 0.
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
     &       -viscAh*(vVel(i,j+1,k,bi,bj)-vVel(i,j,k,bi,bj))*cosFacU(J)
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */
#ifdef INCLUDE_BH_MOMENTUM_DIFFUSION_CODE
     &       +viscA4*(v4F(i,j+1)         -v4F(i,j)         )*cosFacU(J)
#endif /* INCLUDE_BH_MOMENTUM_DIFFUSION_CODE */
     &      )*_recip_dyF(i,j,bi,bj)

       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = 0.
     &  _ADM(     + vDvdyFac    * af(i,j)      )
     &            + AhDvdyFac   * vf(i,j) 
       ENDDO
      ENDDO
C--   Vertical flux (fVer is at upper face of "v" cell)
#ifdef INCLUDE_MOMENTUM_ADVECTION_CODE
C     o Mean flow component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   rVelMaskOverride*(rTrans(i,j)+rTrans(i,j-1))
     &  *( vVel(i,j,k,bi,bj)+vVel(i,j,kM1,bi,bj) )
     &  *0.25 _d 0
#ifdef OLD_ADV_BCS
     &  *_maskS(i,j,k,bi,bj)*_maskS(i,j,kM1,bi,bj)
#endif /* OLD_ADV_BCS */
C       Note!!!! The line "*(maskS(i,j,k,bi,bj)*maskS(i,j,kM1,bi,bj))" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
C       Note!!!! Is rVelMaskOverridde right for implicit free-surface?
       ENDDO
      ENDDO
#endif /* INCLUDE_MOMENTUM_ADVECTION_CODE */
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#ifdef OLD_UV_GEOMETRY
#define _RA_AT_S (RA(i,j,bi,bj)*maskC(i,j)+RA(i,j-1,bi,bj)*maskC(i,j-1))*.5
#else
#define _RA_AT_S rAs(i,j,bi,bj)
#endif /* OLD_UV_GEOMETRY */
C     Eddy component of vertical flux
      IF (no_slip_bottom) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &   -KappaRV(i,j,k)
     & *_RA_AT_S
     &   *( ( 2.-_maskS(i,j,k,bi,bj) )*vVel(i,j,kM1,bi,bj)
     &     -     _maskS(i,j,k,bi,bj)  *vVel(i,j,k,bi,bj)
     &    )*rkFac*recip_drC(k)
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &   -KappaRV(i,j,k)
     & *_RA_AT_S
     &   *(vVel(i,j,km1,bi,bj)-vVel(i,j,k,bi,bj))
     &   *rkFac*recip_drC(k)
     &   *_maskS(i,j,k,bi,bj)*rVelMaskOverride
        ENDDO
       ENDDO
      ENDIF
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kUp) = rVelDvdrFac * af(i,j) 
     &                 + ArDvdrFac   * vf(i,j)
       ENDDO
      ENDDO
C     Bottom level boundary condition
#ifdef INCLUDE_LP_MOMENTUM_DIFFUSION_CODE
#ifdef OLD_UV_GEOMETRY
#define _RA_AT_S2 (RA(i,j,bi,bj)+RA(i,j-1,bi,bj))*0.5 _d 0
#else
#define _RA_AT_S2 rAs(i,j,bi,bj)
#endif /* OLD_UV_GEOMETRY */
      IF ( no_slip_bottom .AND. k .EQ. Nr ) THEN
      DO j=jMin,jMax
       DO i=iMin,iMax
       vf(i,j) =
     & -KappaRV(i,j,k)
     & *_RA_AT_S2
     & *_maskS(i,j,k,bi,bj)
     & *vVel(i,j,k,bi,bj)*rkFac*recip_drF(k)*2. _d 0
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kDown) = ArDvdrFac * vf(i,j)
       ENDDO
      ENDDO
      ENDIF
#endif /* INCLUDE_LP_MOMENTUM_DIFFUSION_CODE */

#ifdef INCLUDE_GRADPH_CODE
C---  Hydorstatic term (-1/rho . dp/dy )
      DO j=jMin,jMax
       DO i=iMin,iMax
        pF(i,j) = -_recip_dyC(i,j,bi,bj)
     &   *(phi_hyd(i,j,k)-phi_hyd(i,j-1,k))*recip_rhoConst
       ENDDO
      ENDDO
#endif /* INCLUDE_GRADPH_CODE */

C--   Tendency is minus divergence of the fluxes + coriolis + pressure 
C--   term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) =
#ifdef OLD_UV_GEOM
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)/
     &    ( 0.5 _d 0*(_rA(i,j,bi,bj)+_rA(i,j-1,bi,bj)) )
#else
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    /rAs(i,j,bi,bj)
#endif
     &  *(fZon(i+1,j)          - fZon(i,j  )
     &   +fMer(i,j  )          - fMer(i,j-1)
     &   +fVerV(i,j,kUp)*rkFac - fVerV(i,j,kDown)*rkFac
     &   )
     & _PHM( +phyFac*pf(i,j) )
       ENDDO
      ENDDO

      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=jMin,jMax
       DO i=iMin,iMax
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    /rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj) )
     &   *rkFac*drF(k)*2.*( viscAh*vVel(i,j,k,bi,bj)
     &                     -viscA4*v4F(i,j) ) *cosFacV(J)
       ENDDO
      ENDDO
      ENDIF

#ifdef INCLUDE_MOMENTUM_FORCING_CODE
C--   Forcing term
      CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myCurrentTime,myThid)
#endif

#ifdef INCLUDE_MOMENTUM_METRIC_TERM_CODE
C--   Metric terms for curvilinear grid systems
      IF ( usingSphericalPolarMTerms ) THEN
C      o Spherical polar grid metric terms
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = -vVel(i,j,k,bi,bj)*recip_RSphere
     &    *0.25 _d 0*( rVel(i,j,kUp  )+rVel(i,j-1,kUp  )
     &               + rVel(i,j,KDown)+rVel(i,j-1,KDown)
     &              )*rkFac*recip_horiVertRatio
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = mT(i,j)-recip_RSphere
     &    *0.25 _d 0*( uVel(i,j  ,k,bi,bj)+uVel(i+1,j  ,k,bi,bj)
     &                +uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj)
     &              )
     &    *0.25 _d 0*( uVel(i,j  ,k,bi,bj)+uVel(i+1,j  ,k,bi,bj)
     &                +uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj)
     &              )
     &    *_tanPhiAtV(i,j,bi,bj)
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+
     &    mTFacV*mT(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C--   Set dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Coriolis term
C     Note. As coded here, coriolis will not work with "thin walls"
#ifdef INCLUDE_CD_CODE
C     Pressure extrapolated forward in time
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = 
     &   ab15*(   cg2d_x(i,j,bi,bj)*gBaro )
     &  +ab05*(cg2d_xNM1(i,j,bi,bj)*gBaro )
       ENDDO
      ENDDO
#endif /* INCLUDE_CD_CODE */
C--   Zonal velocity coriolis term
C     Note. As coded here, coriolis will not work with "thin walls"
#ifdef INCLUDE_CD_CODE
C--   Coriolis with CD scheme allowed
C     grady(p) + gV
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = -_maskS(i,j,k,bi,bj)
     &            *_recip_dyC(i,j,bi,bj)
     &            *(pf(i,j)-pf(i,j-1))
     &            +gV(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Average to Vd point and add coriolis
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) =      
     &    0.25 _d 0*( af(i  ,j)+af(i  ,j+1)
     &               +af(i-1,j)+af(i-1,j+1)
     &              )*_maskW(i,j,k,bi,bj)
     &   -0.5  _d 0*(_fCori(i,j,bi,bj)+
     &               _fCori(i-1,j,bi,bj))
     &              *uVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Step forward Vd
      DO j=jMin,jMax
       DO i=iMin,iMax
        vVelD(i,j,k,bi,bj) = vVelD(i,j,k,bi,bj) +
     &                        deltaTmom*vf(i,j)
       ENDDO
      ENDDO
C     Relax D grid V to C grid V
      DO j=jMin,jMax
       DO i=iMin,iMax
         vVelD(i,j,k,bi,bj) = rCD*vVelD(i,j,k,bi,bj)
     &   +(1. _d 0 - rCD)*(
     &    ab15*0.25 _d 0*( 
     &                vVel(i  ,j  ,k,bi,bj)+vVel(i  ,j+1,k,bi,bj)
     &               +vVel(i-1,j  ,k,bi,bj)+vVel(i-1,j+1,k,bi,bj)
     &              )*_maskW(i,j,k,bi,bj)
     &     +
     &    ab05*0.25 _d 0*( 
     &                vNM1(i  ,j  ,k,bi,bj)+vNM1(i  ,j+1,k,bi,bj)
     &               +vNM1(i-1,j  ,k,bi,bj)+vNM1(i-1,j+1,k,bi,bj)
     &              )*_maskW(i,j,k,bi,bj)
     &   )
       ENDDO
      ENDDO
C     Calculate coriolis force on U
      DO j=jMin,jMax
       DO i=iMin,iMax
        guCD(i,j,k,bi,bj) = 
     &    0.5  _d 0 *( _fCori(i  ,j,bi,bj) + 
     &                 _fCori(i-1,j,bi,bj)  )
     &   *vVelD(i,j,k,bi,bj)*fuFac
       ENDDO
      ENDDO
#endif /* INCLUDE_CD_CODE */
#ifndef INCLUDE_CD_CODE
C--   No CD scheme support
      DO j=jMin,jMax
       DO i=iMin,iMax
        cf(i,j) = 
     &    0.5  _d 0 *( _fCori(i  ,j,bi,bj) + 
     &                 _fCori(i-1,j,bi,bj)  )
     &   *0.25 _d 0 *(
     &     vVel(i  ,j,k,bi,bj)+vVel(i  ,j+1,k,bi,bj)
     &    +vVel(i-1,j,k,bi,bj)+vVel(i-1,j+1,k,bi,bj)
     &   )
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &   +fuFac*cf(i,j)
       ENDDO
      ENDDO
#endif /* !INCLUDE_CD_CODE */


C--   Meridional velocity coriolis term
#ifdef INCLUDE_CD_CODE
C     gradx(p)+gU
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = -_maskW(i,j,k,bi,bj)
     &            *_recip_dxC(i,j,bi,bj)*
     &            (pf(i,j)-pf(i-1,j))
     &            +gU(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Average to Ud point and add coriolis
      DO j=jMin,jMax
       DO i=iMin,iMax
        vf(i,j) =
     &    0.25 _d 0*( af(i  ,j)+af(i  ,j-1)
     &               +af(i+1,j)+af(i+1,j-1)
     &              )*_maskS(i,j,k,bi,bj)
     &   +0.5  _d 0*( _fCori(i,j,bi,bj)
     &               +_fCori(i,j-1,bi,bj))
     &              *vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Step forward Ud
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVelD(i,j,k,bi,bj) = uVelD(i,j,k,bi,bj) +
     &   deltaTmom*vf(i,j)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
C     Relax D grid U to C grid U
      DO j=jMin,jMax
       DO i=iMin,iMax
        uVelD(i,j,k,bi,bj) = rCD*uVelD(i,j,k,bi,bj) 
     &   +(1. _d 0 - rCD)*(
     &    ab15*0.25 _d 0*( 
     &                uVel(i,j  ,k,bi,bj)+uVel(i+1,j  ,k,bi,bj)
     &               +uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj)
     &              )*_maskS(i,j,k,bi,bj)
     &     +
     &    ab05*0.25 _d 0*( 
     &                uNM1(i,j  ,k,bi,bj)+uNM1(i+1,j  ,k,bi,bj)
     &               +uNM1(i,j-1,k,bi,bj)+uNM1(i+1,j-1,k,bi,bj)
     &              )*_maskS(i,j,k,bi,bj)
     &   )
       ENDDO
      ENDDO
C     Calculate coriolis force on V
      DO j=jMin,jMax
       DO i=iMin,iMax
        gvCD(i,j,k,bi,bj) =
     &    -0.5  _d 0 *( _fCori(i  ,j,bi,bj) 
     &                 +_fCori(i,j-1,bi,bj)  )
     &   *uVelD(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)*fvFac
       ENDDO
      ENDDO
#endif /* INCLUDE_CD_CODE */
#ifndef INCLUDE_CD_CODE
C--   No CD scheme support
      DO j=jMin,jMax
       DO i=iMin,iMax
        cf(i,j) = 
     &   -0.5 _d 0 *(_fCori(i,j  ,bi,bj)+_fCori(i,j-1,bi,bj))
     &   *0.25 _d 0
     &   *( uVel(i,j  ,k,bi,bj)+uVel(i+1,j  ,k,bi,bj)
     &     +uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj)
     &    )
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &   +fvFac*cf(i,j)
       ENDDO
      ENDDO
#endif /* !INCLUDE_CD_CODE */

C--   Set du/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO
C--   Set dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

#ifdef INCLUDE_LAT_CIRC_FFT_FILTER_CODE
      CALL FILTER_LATCIRCS_FFT_APPLY(
     &    gU, 1-1, sNy+1, k, k, bi, bj, 1, myThid)
      CALL FILTER_LATCIRCS_FFT_APPLY(
     &    gV, 1-1, sNy+1, k, k, bi, bj, 2, myThid)
#endif


#ifdef INCLUDE_CD_CODE
C--   Save "previous time level" variables
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uNM1(i,j,k,bi,bj) =  uVel(i,j,k,bi,bj)
         vNM1(i,j,k,bi,bj) =  vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
#endif /* INCLUDE_CD_CODE */

      RETURN
      END
