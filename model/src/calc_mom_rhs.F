C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_mom_rhs.F,v 1.21 1998/08/22 17:51:07 cnh Exp $

#include "CPP_OPTIONS.h"

#define NO_LATERAL_SLIP
#define NO_BOTTOM_SLIP
#define DB0 0.

C     /==========================================================\
C     | S/R CALC_MOM_RHS                                         |
C     | o Form the right hand-side of the momentum equation.     |
C     |==========================================================|
C     | Terms are evaluated one layer at a time working from     |
C     | the bottom to the top. The vertically integrated         |
C     | barotropic flow tendency term is evluated by summing the |
C     | tendencies.                                              |
C     | Notes:                                                   |
C     | We haven't sorted out an entirely satisfactory formula   |
C     | for the diffusion equation bc with lopping. The present  |
C     | form produces a diffusive flux that does not scale with  |
C     | open-area. Need to do something to solidfy this and to   |
C     | deal "properly" with thin walls.                         |
C     \==========================================================/
      SUBROUTINE CALC_MOM_RHS( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I        xA,yA,uTrans,vTrans,rTrans,rVel,maskC,
     I        phi_hyd,
     U        aFArg,vFArg,cFArg,mTArg,pFArg,
     U        fZonArg, fMerArg, fVerU, fVerV,
     I        myThid)

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
      _RL      phi_hyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS      xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS   maskC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL   fVerU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL   fVerV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL      aFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      cFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mTArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pFArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZonArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMerArg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kUp,kDown,kM1
      INTEGER myThid
      INTEGER bi,bj,iMin,iMax,jMin,jMax

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
C     uDudxFac, AhDudxFac, etc ... individual term tracer parameters
      _RL      aF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      cF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      sF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     I,J,K - Loop counters
      INTEGER i,j,k
C     rVelMaskOverride - Factor for imposing special surface boundary conditions
C                        ( set according to free-surface condition ).
C     hFacROpen        - Lopped cell factos used tohold fraction of open
C     hFacRClosed        and closed cell wall.
      _RL  rVelMaskOverride
      _RS  hFacROpen
      _RS  hFacRClosed
C     xxxFac - On-off tracer parameters used for switching terms off.
      _RL  uDudxFac
      _RL  AhDudxFac
      _RL  vDudyFac
      _RL  AhDudyFac
      _RL  rVelDudrFac
      _RL  ArDudrFac
      _RL  fuFac
      _RL  phxFac
      _RL  uForcFac
      _RL  mtFacU
      _RL  uDvdxFac
      _RL  AhDvdxFac
      _RL  vDvdyFac
      _RL  AhDvdyFac
      _RL  rVelDvdrFac
      _RL  ArDvdrFac
      _RL  fvFac
      _RL  phyFac
      _RL  vForcFac
      _RL  mtFacV
C     ab05, ab15 - Adams-Bashforth time-stepping weights.
      _RL  ab05, ab15
      rVelMaskOverride=1.
      IF ( k .EQ. 1 ) rVelMaskOverride=freeSurfFac
C     Initialise intermediate terms
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        aF(i,j)   = 0.*1. _d 37
        vF(i,j)   = 0.*1. _d 37
        cF(i,j)   = 0.*1. _d 37
        mT(i,j)   = 0.*1. _d 37
        pF(i,j)   = 0.*1. _d 37
        fZon(i,j) = 0.*1. _d 37
        fMer(i,j) = 0.*1. _d 37
       ENDDO
      ENDDO
C--   Term by term tracer parmeters
C     o U momentum equation
      uDudxFac  = afFacMom 
      AhDudxFac = vfFacMom
      vDudyFac  = afFacMom
      AhDudyFac = vfFacMom
      rVelDudrFac  = afFacMom
      ArDudrFac = vfFacMom
      mTFacU    = mtFacMom
      fuFac     = cfFacMom
      phxFac    = pfFacMom
      uForcFac  = foFacMom
C     o V momentum equation
      uDvdxFac  = afFacMom 
      AhDvdxFac = vfFacMom
      vDvdyFac  = afFacMom
      AhDvdyFac = vfFacMom
      rVelDvdrFac  = afFacMom
      ArDvdrFac = vfFacMom
      mTFacV    = mtFacMom
      fvFac     = cfFacMom
      phyFac    = pfFacMom
      vForcFac  = foFacMom

C--   Adams-Bashforth weighting factors
      ab15   =  1.5 _d 0 + abEps
      ab05   = -0.5 _d 0 - abEps
  
C---  Calculate mean and eddy fluxes between cells for zonal flow.
C--   Zonal flux (fZon is at east face of "u" cell)
C     Mean flow component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (uTrans(i,j)+uTrans(i+1,j) )
     &  *(uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))*0.25 _d 0
       ENDDO
      ENDDO
C     Eddy component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
#define _XAatP(i,j,k,bi,bj) ( XA(i,j)+XA(i+1,j) )* 0.5 _d 0
        vf(i,j) =
     &   -viscAh*_XAatP(i,j,k,bi,bj)
     &    *(uVel(i+1,j,k,bi,bj)-uVel(i,j,k,bi,bj))*_recip_dxF(i,j,bi,bj)
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = uDudxFac  * af(i,j) 
     &            + AhDudxFac * vf(i,j)
       ENDDO
      ENDDO
C--   Meridional flux (fMer is at south face of "u" cell)
C     Mean flow component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (vTrans(i,j)+vTrans(i-1,j))
     &  *(uVel(i,j,k,bi,bj)+uVel(i,j-1,k,bi,bj))*0.25 _d 0
     &  *_maskW(i,j,k,bi,bj)*_maskW(i,j-1,k,bi,bj)
C       Note!!!! The line "*maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
C     Eddy component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        hFacROpen=min(_hFacW(i,j,k,bi,bj),_hFacW(i,j-1,k,bi,bj))
        hFacROpen=min(_hFacS(i,j,k,bi,bj),hFacROpen)
        hFacROpen=min(_hFacS(i-1,j,k,bi,bj),hFacROpen)
        hFacRClosed= 1. _d 0 - hFacRopen
        hFacRClosed= 1. _d 0
#ifdef NO_LATERAL_SLIP
#define _YAatUBY(i,j,k,bi,bj) ( YA(i,j)+YA(i-1,j) )* 0.5 _d 0
        vF(i,j) =
     &  -viscAh*_YAatUBY(i,j,k,bi,bj)*hFacRClosed
     &   *( (2.-_maskW(i,j-1,k,bi,bj))*_maskW(i,j,k,bi,bj)*uVel(i,j  ,k,bi,bj)
     &     -(2.-_maskW(i,j,k,bi,bj))*_maskW(i,j-1,k,bi,bj)*uVel(i,j-1,k,bi,bj)
     &    )*_recip_dyU(i,j,bi,bj)
#else
        vF(i,j) =
     &  -viscAh*_dxV(i,j,bi,bj)*drF(k)*hFacROpen
     &   *( uVel(i,j  ,k,bi,bj)-uVel(i,j-1,k,bi,bj) )
     &   *_recip_dyU(i,j,bi,bj)
#endif
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = vDudyFac  * af(i,j) 
     &            + AhDudyFac * vf(i,j)
       ENDDO
      ENDDO
C--   Vertical flux (fVer is at upper face of "u" cell)
C     Mean flow component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   rVelMaskOverride*(rTrans(i,j)+rTrans(i-1,j))
     &  *( uVel(i,j,k,bi,bj)+uVel(i,j,kM1,bi,bj) )
     &  *0.25 _d 0
     &  *(_maskW(i,j,k,bi,bj)*_maskW(i,j,kM1,bi,bj))
C       Note!!!! The line "*(maskW(i,j,k,bi,bj)*maskW(i,j,kM1,bi,bj))" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
C     Eddy component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
#define _RAatU(i,j,k,bi,bj) ( rA(i,j,bi,bj)*maskC(i,j) + rA(i-1,j,bi,bj)*maskC(i-1,j) ) * 0.5 _d 0
#ifdef NO_BOTTOM_SLIP
        vf(i,j) =
     &   -viscAr*_RAatU(i,j,k,bi,bj)
     &   *( ( 2.-_maskW(i,j,k,bi,bj) )*uVel(i,j,kM1,bi,bj) 
     &      - uVel(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
     &    )*rkFac*recip_drC(k)
#else
        vf(i,j) =
     &   -viscAr*_dxC(i,j,bi,bj)*_dyG(i,j,bi,bj)
     &   *(uVel(i,j,km1,bi,bj)-uVel(i,j,k,bi,bj))*rkFac*recip_drC(k)
    &    *_maskW(i,j,k,bi,bj)*rVelMaskOverride
#endif
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerU(i,j,kUp) = rVelDudrFac  * af(i,j) 
     &                 +    ArDudrFac * vf(i,j)
       ENDDO
      ENDDO
C     Bottom boundary condition
#ifdef NO_BOTTOM_SLIP
      IF ( k .EQ. Nr ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
#define _RAatU(i,j,k,bi,bj) ( rA(i,j,bi,bj) + rA(i-1,j,bi,bj) ) * 0.5 _d 0
         vf(i,j) =
     &    -viscAr*_RAatU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
     &    *uVel(i,j,k,bi,bj)*recip_drF(k)*2. _d 0*rkFac
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerU(i,j,kDown) = ArDudrFac * vf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C---  Hydrostatic term ( -1/rhoConst . dphi/dx )
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = - _recip_dxC(i,j,bi,bj)
     &   *(phi_hyd(i,j,k)-phi_hyd(i-1,j,k))/rhoConst
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) =
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)/
     &    ( 0.5 _d 0 *(_rA(i,j,bi,bj)+_rA(i-1,j,bi,bj)) )
     &  *(fZon(i,j  )          - fZon(i-1,j)
     &   +fMer(i,j+1)          - fMer(i  ,j)
     &   +fVerU(i,j,kUp)*rkFac - fVerU(i,j,kDown)*rkFac
     &   )
     &   +phxFac * pf(i,j)
       ENDDO
      ENDDO

C--   Forcing term
      IF ( k .EQ. 1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) 
     &    + uForcFac*fu(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C--   Metric terms for curvilinear grid systems
      IF ( usingSphericalPolarMTerms ) THEN
C      o Spherical polar grid metric terms
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = -1.* uVel(i,j,k,bi,bj)*recip_rSphere
     &    *0.25 _d 0*( rVel(i-1,j,kUp  )+rVel(i  ,j,kUp  )
     &                +rVel(i-1,j,KDown)+rVel(i  ,j,KDown)
     &              )
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = mT(i,j)+uVel(i,j,k,bi,bj)*recip_rSphere
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

C--   Set du/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

C---  Calculate mean and eddy fluxes between cells for meridional velocity.
C--   Zonal flux (fZon is at west face of "v" cell)
C     Mean flow component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (uTrans(i,j)+uTrans(i,j-1) )
     &  *(vVel(i,j,k,bi,bj)+vVel(i-1,j,k,bi,bj))*0.25 _d 0
     &  *_maskS(i,j,k,bi,bj)*_maskS(i-1,j,k,bi,bj)
C       Note!!!! The line "*maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
C     Eddy component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
CcnhNeedsMacro
         hFacROpen=min(_hFacW(i,j,k,bi,bj),_hFacW(i,j-1,k,bi,bj))
         hFacROpen=min(_hFacS(i,j,k,bi,bj),hFacROpen)
         hFacROpen=min(_hFacS(i-1,j,k,bi,bj),hFacROpen)
         hFacRClosed=1. _d 0 -hFacROpen
         hFacRClosed=1. _d 0
#ifdef NO_LATERAL_SLIP
#define _XAatVBX(i,j,k,bi,bj) ( XA(i,j)+XA(i,j-1) )* 0.5 _d 0
         vf(i,j) =
     &   -viscAh*_XAatVBX(i,j,k,bi,bj)*hFacRClosed*
     &   ( (2.-_maskS(i-1,j,k,bi,bj))*_maskS(i,j,k,bi,bj)*vVel(i  ,j,k,bi,bj)
     &    -(2.-_maskS(i,j,k,bi,bj))*_maskS(i-1,j,k,bi,bj)*vVel(i-1,j,k,bi,bj)
     &   )*_recip_dxV(i,j,bi,bj)
#else
        vf(i,j) =
     &  -viscAh*_dyU(i,j,bi,bj)*drF(K)*hFacROpen*
     &  (vVel(i,j,k,bi,bj)-vVel(i-1,j,k,bi,bj))
     &  *_recip_dxV(i,j,bi,bj)
#endif
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = uDvdxFac  * af(i,j)
     &            + AhDvdxFac * vf(i,j)
       ENDDO
      ENDDO
C--   Meridional flux (fMer is at north face of "v" cell)
C     Mean flow component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   (vTrans(i,j)+vTrans(i,j+1))
     &  *(vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))*0.25 _d 0
       ENDDO
      ENDDO
C     Eddy component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
#define _YAatP(i,j,k,bi,bj) ( YA(i,j) + YA(i,j+1) ) *0.5 _d 0
        vF(i,j) =
     &  -viscAh*_YAatP(i,j,k,bi,bj)
     &   *( vVel(i,j+1,k,bi,bj) - vVel(i,j,k,bi,bj) )*_recip_dyC(i,j,bi,bj)
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = vDvdyFac  * af(i,j) 
     &            + AhDvdyFac * vf(i,j)
       ENDDO
      ENDDO
C--   Vertical flux (fVer is at upper face of "v" cell)
C     Mean flow component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) =
     &   rVelMaskOverride*(rTrans(i,j)+rTrans(i,j-1))
     &  *( vVel(i,j,k,bi,bj)+vVel(i,j,kM1,bi,bj) )
     &  *0.25 _d 0
     &  *_maskS(i,j,k,bi,bj)*_maskS(i,j,kM1,bi,bj)
C       Note!!!! The line "*(maskS(i,j,k,bi,bj)*maskS(i,j,kM1,bi,bj))" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
C       Note!!!! Is wMaskOverridde right for implicit free-surface?

       ENDDO
      ENDDO
C     Eddy component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
#define _RAatV(i,j,k,bi,bj) ( _rA(i,j,bi,bj)*maskC(i,j) + _rA(i,j-1,bi,bj)*maskC(i,j-1) ) * 0.5 _d 0
#ifdef NO_BOTTOM_SLIP
        vf(i,j) =
     &   -viscAr*_RAatV(i,j,k,bi,bj)
     &   *( ( 2.-_maskS(i,j,k,bi,bj) )
     &     *vVel(i,j,kM1,bi,bj) - vVel(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
     &    )*rkFac*recip_drC(k)
#else
        vf(i,j) =
     &   -viscAr*_dyC(i,j,bi,bj)*_dxG(i,j,bi,bj)
     &   *(vVel(i,j,km1,bi,bj)-vVel(i,j,k,bi,bj))*rkFac*recip_drC(k)
     &    *_maskS(i,j,k,bi,bj)*wMaskOverride
#endif
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kUp) = rVelDvdrFac * af(i,j) 
     &                 + ArDvdrFac   * vf(i,j)
       ENDDO
      ENDDO
C     Bottom level boundary condition
#ifdef NO_BOTTOM_SLIP
      IF ( k .EQ. Nr ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
#define _RAatV(i,j,k,bi,bj) ( _rA(i,j,bi,bj) + _rA(i,j-1,bi,bj) ) * 0.5 _d 0
         vf(i,j) =
     &    -viscAr*_RAatV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
     &    *vVel(i,j,k,bi,bj)*recip_drF(k)*2. _d 0
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerV(i,j,kDown) = ArDvdrFac * vf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C---  Hydorstatic term (-1/rho . dp/dy )
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = - recip_dyC(i,j,bi,bj)
     &   *(phi_hyd(i,j,k)-phi_hyd(i,j-1,k))/rhoConst
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) =
     &   -recip_hFacS(i,j,k,bi,bj)*recip_drF(k)/
     &     ( 0.5 _d 0 *( _rA(i,j,bi,bj)+ _rA(i,j-1,bi,bj)) )
     &  *(fZon(i+1,j)          - fZon(i,j  )
     &   +fMer(i,j  )          - fMer(i,j-1)
     &   +fVerV(i,j,kUp)*rkFac - fVerV(i,j,kDown)*rkFac
     &   )
     &   +phyFac*pf(i,j)
       ENDDO
      ENDDO

C--   Forcing term
      IF ( k .EQ. 1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) 
     &    + foFacMom*fv(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C--   Metric terms for curvilinear grid systems
      IF ( usingSphericalPolarMTerms ) THEN
C      o Spherical polar grid metric terms
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = -vVel(i,j,k,bi,bj)*recip_rSphere
     &    *0.25 _d 0*( rVel(i,j,kUp  )+rVel(i,j-1,kUp  )
     &               + rVel(i,j,KDown)+rVel(i,j-1,KDown)
     &              )
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         mT(i,j) = mT(i,j)-recip_rSphere
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

C--   Set dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Coriolis term
C     Note. As coded here, coriolis will not work with "thin walls"
#ifdef ALLOW_CD
C     Pressure extrapolated forward in time
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = 
     &   ab15*(   cg2d_x(i,j,bi,bj)*gBaro )
     &  +ab05*(cg2d_xNM1(i,j,bi,bj)*gBaro )
       ENDDO
      ENDDO
#endif
C--   Zonal velocity coriolis term
C     Note. As coded here, coriolis will not work with "thin walls"
#ifdef ALLOW_CD
C--   Coriolis with CD scheme allowed
C     grady(p) + gV
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = -_maskS(i,j,k,bi,bj)*recip_dyC(i,j,bi,bj)
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
     &   -0.5  _d 0*(_fCori(i,j,bi,bj)+_fCori(i-1,j,bi,bj))
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
     &    0.5  _d 0 *( _fCori(i  ,j,bi,bj) + _fCori(i-1,j,bi,bj)  )
     &   *vVelD(i,j,k,bi,bj)
       ENDDO
      ENDDO
#else
C--   No CD scheme support
      DO j=jMin,jMax
       DO i=iMin,iMax
        cf(i,j) = 
     &    0.5  _d 0 *( _fCori(i  ,j,bi,bj) + _fCori(i-1,j,bi,bj)  )
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
#endif


C--   Meridional velocity coriolis term
#ifdef ALLOW_CD
C     gradx(p)+gU
      DO j=jMin,jMax
       DO i=iMin,iMax
        af(i,j) = -_maskW(i,j,k,bi,bj)*_recip_dxC(i,j,bi,bj)*
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
     &   +0.5  _d 0*(_fCori(i,j,bi,bj)+_fCori(i,j-1,bi,bj))
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
     &    -0.5  _d 0 *( _fCori(i  ,j,bi,bj) + _fCori(i,j-1,bi,bj)  )
     &   *uVelD(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
CcnhDebugStarts
C     WRITE(0,*) ' CG2D MAXVAL(ABS(gvCD))', MAXVAL(ABS(gvCD))
CcnhDebugEnds
#else
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
#endif

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

#ifdef ALLOW_CD
C--   Save "previous time level" variables
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uNM1(i,j,k,bi,bj) =  uVel(i,j,k,bi,bj)
         vNM1(i,j,k,bi,bj) =  vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO
#endif

      RETURN
      END
