C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_mom_rhs.F,v 1.7 1998/05/27 05:57:02 cnh Exp $

#include "CPP_EEOPTIONS.h"

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
     I        xA,yA,uTrans,vTrans,wTrans,maskC,
     I        pH,
     U        aFArg,vFArg,cFArg,mTArg,pFArg,
     U        fZonArg, fMerArg, fVerU, fVerV,
     I        myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"
#include "FFIELDS.h"
#include "PARAMS.h"
#include "GRID.h"

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
C     wTrans  - Vertical volume transport through cell upper face
C     pH - Hydrostatic pressure
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     kUp, kDown, kM1                - Index for upper and lower layers.
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      _RL      pH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
      _RS      xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  wTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
C                     pfFacMom - Pressure terms
C                     cfFacMom - Coriolis terms
C                     foFacMom - Forcing
C     uDudxFac, AhDudxFac, etc ... individual term tracer parameters
      _RL      aF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      cF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      INTEGER i,j,k

      REAL wMaskOverride
      REAL hFacZOpen
      REAL hFacZClosed

      REAL uDudxFac
      REAL AhDudxFac
      REAL vDudyFac
      REAL AhDudyFac
      REAL wDudzFac
      REAL AzDudzFac
      REAL fuFac
      REAL phxFac
      REAL uForcFac

      REAL uDvdxFac
      REAL AhDvdxFac
      REAL vDvdyFac
      REAL AhDvdyFac
      REAL wDvdzFac
      REAL AzDvdzFac
      REAL fvFac
      REAL phyFac
      REAL vForcFac

      _RS  ab05, ab15

      wMaskOverride=1.
      IF ( k .EQ. 1 ) wMaskOverride=freeSurfFac

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
      wDudzFac  = afFacMom
      AzDudzFac = vfFacMom
      fuFac     = cfFacMom
      phxFac    = pfFacMom
      uForcFac  = foFacMom
C     o V momentum equation
      uDvdxFac  = afFacMom 
      AhDvdxFac = vfFacMom
      vDvdyFac  = afFacMom
      AhDvdyFac = vfFacMom
      wDvdzFac  = afFacMom
      AzDvdzFac = vfFacMom
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
        vf(i,j) =
     &   -viscAh*_dyF(i,j,bi,bj)*dzF(k)*hFacC(i,j,k,bi,bj)
     &    *(uVel(i+1,j,k,bi,bj)-uVel(i,j,k,bi,bj))*rDxF(i,j,bi,bj)
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
     &  *maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
C       Note!!!! The line "*maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
C     Eddy component of meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
          hFacZOpen=min(hFacW(i,j,k,bi,bj),hFacW(i,j-1,k,bi,bj))
          hFacZOpen=min(hFacS(i,j,k,bi,bj),hFacZOpen)
          hFacZOpen=min(hFacS(i-1,j,k,bi,bj),hFacZOpen)
        hFacZClosed= 1. _d 0 - hFacZopen
#ifdef NO_LATERAL_SLIP
        vF(i,j) =
     &  -viscAh*_dxV(i,j,bi,bj)*dzF(k)*hFacZOpen
     &   *( uVel(i,j  ,k,bi,bj)-uVel(i,j-1,k,bi,bj) )
     &   *rDyU(i,j,bi,bj)
     &  -viscAh*_dxV(i,j,bi,bj)*dzF(k)*hFacZClosed
     &   *( (2.-maskW(i,j-1,k,bi,bj))*maskW(i,j,k,bi,bj)*uVel(i,j  ,k,bi,bj)
     &     -(2.-maskW(i,j,k,bi,bj))*maskW(i,j-1,k,bi,bj)*uVel(i,j-1,k,bi,bj)
     &    )*rDyU(i,j,bi,bj)
#else
        vF(i,j) =
     &  -viscAh*_dxV(i,j,bi,bj)*dzF(k)*hFacZOpen
     &   *( uVel(i,j  ,k,bi,bj)-uVel(i,j-1,k,bi,bj) )
     &   *rDyU(i,j,bi,bj)
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
     &   wMaskOverride*(wTrans(i,j)+wTrans(i-1,j))
     &  *( uVel(i,j,k,bi,bj)+uVel(i,j,kM1,bi,bj) )
     &  *0.25 _d 0
     &  *(maskW(i,j,k,bi,bj)*maskW(i,j,kM1,bi,bj))
C       Note!!!! The line "*(maskW(i,j,k,bi,bj)*maskW(i,j,kM1,bi,bj))" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
C       Note!!!! Is wMaskOverridde right for implicit free-surface?
       ENDDO
      ENDDO
C     Eddy component of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef NO_BOTTOM_SLIP
        vf(i,j) =
     &   -viscAz*_dxC(i,j,bi,bj)*_dyG(i,j,bi,bj)*wMaskOverride
     &   *( ( 2.-maskW(i,j,k,bi,bj) )
     &     *uVel(i,j,kM1,bi,bj) - uVel(i,j,k,bi,bj)*maskW(i,j,k,bi,bj)
     &    )*rDzC(k)
#else
        vf(i,j) =
     &   -viscAz*_dxC(i,j,bi,bj)*_dyG(i,j,bi,bj)
     &   *(uVel(i,j,km1,bi,bj)-uVel(i,j,k,bi,bj))
    &    *maskW(i,j,k,bi,bj)*wMaskOverride
#endif
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerU(i,j,kUp) = wDudzFac  * af(i,j) 
     &                 + AzDudzFac * vf(i,j)
       ENDDO
      ENDDO
C     Bottom boundary condition
#ifdef NO_BOTTOM_SLIP
      IF ( k .EQ. nZ ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &    -viscAz*_dxC(i,j,bi,bj)*_dyG(i,j,bi,bj)*maskW(i,j,k,bi,bj)
     &    *uVel(i,j,k,bi,bj)*rDzF(k)*2. _d 0
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerU(i,j,kDown) = AzDudzFac * vf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C---  Coriolis term(s)
      DO j=jMin,jMax
       DO i=iMin,iMax
        cf(i,j) = 
     &    0.5  _d 0 *( fCori(i  ,j,bi,bj) + fCori(i-1,j,bi,bj)  )
     &   *0.25 _d 0 *(
     &     vVel(i  ,j,k,bi,bj)+vVel(i  ,j+1,k,bi,bj)
     &    +vVel(i-1,j,k,bi,bj)+vVel(i-1,j+1,k,bi,bj)
     &   )
       ENDDO
      ENDDO

C---  Pressure term ( -1/rho . dp/dx )
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = -rdxC(i,j,bi,bj)
     &   *(pH(i,j,k)-pH(i-1,j,k))/rhonil
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) =
     &   -rHFacW(i,j,k,bi,bj)*rdzF(k)*rDxC(i,j,bi,bj)*rDyG(i,j,bi,bj)*(
     &    fZon(i,j  )    - fZon(i-1,j)
     &   +fMer(i,j+1)    - fMer(i  ,j)
     &   +fVerU(i,j,kUp) - fVerU(i,j,kDown)
     &   )
     &   +fuFac  * cf(i,j)
     &   +phxFac * pf(i,j)
       ENDDO
      ENDDO

C--   Forcing term
      IF ( k .EQ. 1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) 
     &    + uForcFac*fu(i,j,bi,bj)*maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C--   Set du/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*maskW(i,j,k,bi,bj)
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
     &  *maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)
C       Note!!!! The line "*maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)" is
C       Note!!!! boundary condition in the standard v010 CM5 code. The
C       Note!!!! FV paper used a different bc in which this line is
C       Note!!!! omitted.
       ENDDO
      ENDDO
C     Eddy component of zonal flux
      DO j=jMin,jMax
       DO i=iMin,iMax
          hFacZOpen=min(hFacW(i,j,k,bi,bj),hFacW(i,j-1,k,bi,bj))
          hFacZOpen=min(hFacS(i,j,k,bi,bj),hFacZOpen)
          hFacZOpen=min(hFacS(i-1,j,k,bi,bj),hFacZOpen)
        hFacZClosed=1. _d 0 -hFacZOpen
#ifdef NO_LATERAL_SLIP
        vf(i,j) =
     &  -viscAh*_dyU(i,j,bi,bj)*dzF(K)*hFacZOpen*
     &  ( vVel(i,j,k,bi,bj)-vVel(i-1,j,k,bi,bj)
     &  )*rDxV(i,j,bi,bj)
     &  -viscAh*_dyU(i,j,bi,bj)*dzF(K)*hFacZClosed*
     &  ( (2.-maskS(i-1,j,k,bi,bj))*maskS(i,j,k,bi,bj)*vVel(i  ,j,k,bi,bj)
     &   -(2.-maskS(i,j,k,bi,bj))*maskS(i-1,j,k,bi,bj)*vVel(i-1,j,k,bi,bj)
     &  )*rDxV(i,j,bi,bj)
#else
        vf(i,j) =
     &  -viscAh*_dyU(i,j,bi,bj)*dzF(K)*hFacZOpen*
     &  (vVel(i,j,k,bi,bj)-vVel(i-1,j,k,bi,bj))
     &  *rDxV(i,j,bi,bj)
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
        vF(i,j) =
     &  -viscAh*_dxF(i,j,bi,bj)*dzF(k)*hFacC(i,j,k,bi,bj)
     &   *( vVel(i,j+1,k,bi,bj) - vVel(i,j,k,bi,bj) )*rDyC(i,j,bi,bj)
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
     &   wMaskOverride*(wTrans(i,j)+wTrans(i,j-1))
     &  *( vVel(i,j,k,bi,bj)+vVel(i,j,kM1,bi,bj) )
     &  *0.25 _d 0
     &  *maskS(i,j,k,bi,bj)*maskS(i,j,kM1,bi,bj)
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
#ifdef NO_BOTTOM_SLIP
        vf(i,j) =
     &   -viscAz*_dyC(i,j,bi,bj)*_dxG(i,j,bi,bj)*wMaskOverride
     &   *( ( 2.-maskS(i,j,k,bi,bj) )
     &     *vVel(i,j,kM1,bi,bj) - vVel(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)
     &    )*rDzC(k)
#else
        vf(i,j) =
     &   -viscAz*_dyC(i,j,bi,bj)*_dxG(i,j,bi,bj)
     &   *(vVel(i,j,km1,bi,bj)-vVel(i,j,k,bi,bj))
     &    *maskS(i,j,k,bi,bj)*wMaskOverride
#endif
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kUp) = wDvdzFac  * af(i,j) 
     &                 + AzDvdzFac * vf(i,j)
       ENDDO
      ENDDO
C     Bottom level boundary condition
#ifdef NO_BOTTOM_SLIP
      IF ( k .EQ. nZ ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         vf(i,j) =
     &    -viscAz*_dyC(i,j,bi,bj)*_dxG(i,j,bi,bj)*maskS(i,j,k,bi,bj)
     &    *vVel(i,j,k,bi,bj)*rDzF(k)*2. _d 0
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerV(i,j,kDown) = AzDvdzFac * vf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

C---  Coriolis term(s)
      DO j=jMin,jMax
       DO i=iMin,iMax
        cf(i,j) =
     &   -0.5 _d 0 *(fCori(i,j  ,bi,bj)+fCori(i,j-1,bi,bj))
     &   *0.25 _d 0
     &   *( uVel(i,j  ,k,bi,bj)+uVel(i+1,j  ,k,bi,bj)
     &     +uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj)
     &    )
       ENDDO
      ENDDO

C---  Pressure term (-1/rho . dp/dy )
      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = -rdyC(i,j,bi,bj)
     &   *(pH(i,j,k)-pH(i,j-1,k))/rhonil
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) =
     &   -rHFacS(i,j,k,bi,bj)*rdzF(k)*rDyC(i,j,bi,bj)*rDxG(i,j,bi,bj)*(
     &    fZon(i+1,j)    - fZon(i,j  )
     &   +fMer(i,j  )    - fMer(i,j-1)
     &   +fVerV(i,j,kUp) - fVerV(i,j,kDown)
     &   )
     &   +cfFacMom*cf(i,j)
     &   +phyFac*pf(i,j)
       ENDDO
      ENDDO

C--   Forcing term
      IF ( k .EQ. 1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) 
     &    + foFacMom*fv(i,j,bi,bj)*maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C--   Set dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Pressure equation source term
C     Term is the vertical integral of the divergence of the 
C     time tendency terms along with a relaxation term that
C     pulls div(U) + dh/dt back toward zero.

      IF ( k .EQ. nZ ) THEN
C      Initialise source term on first pass
       DO j=jMin,jMax
        DO i=iMin,iMax
         cg2d_b(i,j,bi,bj) =
     &    -freeSurfFac*zA(i,j,bi,bj)*
     &     cg2d_x(I  ,J  ,bi,bj)/deltaTMom/deltaTMom
        ENDDO
       ENDDO
      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = uTrans(i,j)/deltatmom
     &   + xA(i,j)*(
     &       ab15*gU(i,j,k,bi,bj)+ab05*gUNM1(i,j,k,bi,bj)
     &     )
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        cg2d_b(i,j,bi,bj) =  cg2d_b(i,j,bi,bj) +
     &   pf(i+1,j)-pf(i,j)
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = vTrans(i,j)/deltatmom
     &   + yA(i,j)*(
     &      ab15*gV(i,j,k,bi,bj)+ab05*gVNM1(i,j,k,bi,bj)
     &     )
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
        cg2d_b(i,j,bi,bj) =  cg2d_b(i,j,bi,bj) +
     &   pf(i,j+1)-pf(i,j)
       ENDDO
      ENDDO

      RETURN
      END
