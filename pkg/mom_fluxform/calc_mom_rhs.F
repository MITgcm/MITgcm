C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/calc_mom_rhs.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

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
     I        bi,bj,iMin,iMax,jMin,jMax,k,kUp,kDown,
     I        phi_hyd,KappaRU,KappaRV,
     U        fVerU, fVerV,
     I        myCurrentTime, myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     == Routine arguments ==
C     fZon    - Work array for flux of momentum in the east-west
C               direction at the west face of a cell.
C     fMer    - Work array for flux of momentum in the north-south
C               direction at the south face of a cell.
C     fVerU   - Flux of momentum in the vertical
C     fVerV     direction out of the upper face of a cell K
C               ( flux into the cell above ).
C     phi_hyd - Hydrostatic pressure
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     kUp, kDown                     - Index for upper and lower layers.
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      _RL phi_hyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fVerU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      INTEGER kUp,kDown
      INTEGER myThid
      _RL     myCurrentTime
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
C     vF            - Temporary holding viscous term (Laplacian)
C     v4F           - Temporary holding viscous term (Biharmonic)
C     cF            - Temporary holding coriolis term.
C     mT            - Temporary holding metric terms(s).
C     pF            - Temporary holding pressure|potential gradient terms.
C     uDudxFac, AhDudxFac, etc ... individual term tracer parameters
      _RL      aF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      v4F(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vrF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      cF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mT (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fZon (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    fMer (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS    hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS  r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     I,J,K - Loop counters
      INTEGER i,j,k
C     rVelMaskOverride - Factor for imposing special surface boundary conditions
C                        ( set according to free-surface condition ).
C     hFacROpen        - Lopped cell factos used tohold fraction of open
C     hFacRClosed        and closed cell wall.
      _RL  rVelMaskOverride
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
      INTEGER km1,kp1
      _RL wVelBottomOverride
      LOGICAL bottomDragTerms
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)
      rVelMaskOverride=1.
      IF ( k .EQ. 1 ) rVelMaskOverride=freeSurfFac
      wVelBottomOverride=1.
      IF (k.EQ.Nr) wVelBottomOverride=0.

C     Initialise intermediate terms
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        aF(i,j)   = 0.
        vF(i,j)   = 0.
        v4F(i,j)  = 0.
        vrF(i,j)  = 0.
        cF(i,j)   = 0.
        mT(i,j)   = 0.
        pF(i,j)   = 0.
        fZon(i,j) = 0.
        fMer(i,j) = 0.
       ENDDO
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

      IF (no_slip_bottom) THEN
       bottomDragTerms=.TRUE.
      ELSE
       bottomDragTerms=.FALSE.
      ENDIF

C-- with stagger time stepping, grad Phi_Hyp is directly incoporated in TIMESTEP
      IF (staggerTimeStep) THEN
        phxFac = 0.
        phyFac = 0.
      ENDIF

C--   Adams-Bashforth weighting factors
      ab15   =  1.5 _d 0 + abEps
      ab05   = -0.5 _d 0 - abEps
  
C--   Calculate open water fraction at vorticity points
      CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

C---- Calculate common quantities used in both U and V equations
C     Calculate tracer cell face open areas
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j) = _dyG(i,j,bi,bj)
     &   *drF(k)*_hFacW(i,j,k,bi,bj)
        yA(i,j) = _dxG(i,j,bi,bj)
     &   *drF(k)*_hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Make local copies of horizontal flow field
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uFld(i,j) = uVel(i,j,k,bi,bj)
        vFld(i,j) = vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate velocity field "volume transports" through tracer cell faces.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uTrans(i,j) = uFld(i,j)*xA(i,j)
        vTrans(i,j) = vFld(i,j)*yA(i,j)
       ENDDO
      ENDDO

      CALL MOM_CALC_KE(bi,bj,k,uFld,vFld,KE,myThid)

C---- Zonal momentum equation starts here

C     Bi-harmonic term del^2 U -> v4F
      IF (momViscosity)
     & CALL MOM_U_DEL2U(bi,bj,k,uFld,hFacZ,v4f,myThid)

C---  Calculate mean and eddy fluxes between cells for zonal flow.

C--   Zonal flux (fZon is at east face of "u" cell)

C     Mean flow component of zonal flux -> aF
      IF (momAdvection)
     & CALL MOM_U_ADV_UU(bi,bj,k,uTrans,uFld,aF,myThid)

C     Laplacian and bi-harmonic terms -> vF
      IF (momViscosity)
     & CALL MOM_U_XVISCFLUX(bi,bj,k,uFld,v4F,vF,myThid)

C     Combine fluxes -> fZon
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = uDudxFac*aF(i,j) + AhDudxFac*vF(i,j)
       ENDDO
      ENDDO

C--   Meridional flux (fMer is at south face of "u" cell)

C     Mean flow component of meridional flux
      IF (momAdvection)
     & CALL MOM_U_ADV_VU(bi,bj,k,vTrans,uFld,aF,myThid)

C     Laplacian and bi-harmonic term
      IF (momViscosity)
     & CALL MOM_U_YVISCFLUX(bi,bj,k,uFld,v4F,hFacZ,vF,myThid)

C     Combine fluxes -> fMer
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = vDudyFac*aF(i,j) + AhDudyFac*vF(i,j)
       ENDDO
      ENDDO

C--   Vertical flux (fVer is at upper face of "u" cell)

C--   Free surface correction term (flux at k=1)
      IF (momAdvection.AND.k.EQ.1) THEN
       CALL MOM_U_ADV_WU(bi,bj,k,uVel,wVel,af,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerU(i,j,kUp) = af(i,j)
        ENDDO
       ENDDO
      ENDIF
C     Mean flow component of vertical flux (at k+1) -> aF
      IF (momAdvection)
     & CALL MOM_U_ADV_WU(bi,bj,k+1,uVel,wVel,af,myThid)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity)
     & CALL MOM_U_RVISCFLUX(bi,bj,k,uVel,KappaRU,vrF,myThid)

C     Combine fluxes
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerU(i,j,kDown) = rVelDudrFac*aF(i,j) + ArDudrFac*vrF(i,j)
       ENDDO
      ENDDO

C---  Hydrostatic term ( -1/rhoConst . dphi/dx )
      IF (momPressureForcing) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         pf(i,j) = - _recip_dxC(i,j,bi,bj)
     &    *(phi_hyd(i,j,k)-phi_hyd(i-1,j,k))
        ENDDO
       ENDDO
      ENDIF

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) =
#ifdef OLD_UV_GEOM
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)/
     &    ( 0.5 _d 0*(rA(i,j,bi,bj)+rA(i-1,j,bi,bj)) )
#else
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rAw(i,j,bi,bj)
#endif
     &  *(fZon(i,j  )          - fZon(i-1,j)
     &   +fMer(i,j+1)          - fMer(i  ,j)
     &   +fVerU(i,j,kUp)*rkFac - fVerU(i,j,kDown)*rkFac
     &   )
     & _PHM( +phxFac * pf(i,j) )
       ENDDO
      ENDDO

C-- No-slip and drag BCs appear as body forces in cell abutting topography 
      IF (momViscosity.AND.no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
       CALL MOM_U_SIDEDRAG(bi,bj,k,uFld,v4F,hFacZ,vF,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF
C-    No-slip BCs impose a drag at bottom
      IF (momViscosity.AND.bottomDragTerms) THEN
       CALL MOM_U_BOTTOMDRAG(bi,bj,k,uFld,KE,KappaRU,vF,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Forcing term
      IF (momForcing)
     &  CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myCurrentTime,myThid)

C--   Metric terms for curvilinear grid systems
      IF (usingSphericalPolarMTerms) THEN
C      o Spherical polar grid metric terms
       CALL MOM_U_METRIC_NH(bi,bj,k,uFld,wVel,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mTFacU*mT(i,j)
        ENDDO
       ENDDO
       CALL MOM_U_METRIC_SPHERE(bi,bj,k,uFld,vFld,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mTFacU*mT(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Set du/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO


C---- Meridional momentum equation starts here

C     Bi-harmonic term del^2 V -> v4F
      IF (momViscosity)
     & CALL MOM_V_DEL2V(bi,bj,k,vFld,hFacZ,v4f,myThid)

C---  Calculate mean and eddy fluxes between cells for meridional flow.

C--   Zonal flux (fZon is at west face of "v" cell)

C     Mean flow component of zonal flux -> aF
      IF (momAdvection)
     & CALL MOM_V_ADV_UV(bi,bj,k,uTrans,vFld,af,myThid)

C     Laplacian and bi-harmonic terms -> vF
      IF (momViscosity)
     & CALL MOM_V_XVISCFLUX(bi,bj,k,vFld,v4f,hFacZ,vf,myThid)

C     Combine fluxes -> fZon
      DO j=jMin,jMax
       DO i=iMin,iMax
        fZon(i,j) = uDvdxFac*aF(i,j) + AhDvdxFac*vF(i,j)
       ENDDO
      ENDDO

C--   Meridional flux (fMer is at north face of "v" cell)

C     Mean flow component of meridional flux
      IF (momAdvection)
     & CALL MOM_V_ADV_VV(bi,bj,k,vTrans,vFld,af,myThid)

C     Laplacian and bi-harmonic term
      IF (momViscosity)
     & CALL MOM_V_YVISCFLUX(bi,bj,k,vFld,v4f,vf,myThid)

C     Combine fluxes -> fMer
      DO j=jMin,jMax
       DO i=iMin,iMax
        fMer(i,j) = vDvdyFac*aF(i,j) + AhDvdyFac*vF(i,j)
       ENDDO
      ENDDO

C--   Vertical flux (fVer is at upper face of "v" cell)

C--   Free surface correction term (flux at k=1)
      IF (momAdvection.AND.k.EQ.1) THEN
       CALL MOM_V_ADV_WV(bi,bj,k,vVel,wVel,af,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerV(i,j,kUp) = af(i,j)
        ENDDO
       ENDDO
      ENDIF
C     o Mean flow component of vertical flux
      IF (momAdvection)
     & CALL MOM_V_ADV_WV(bi,bj,k+1,vVel,wVel,af,myThid)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity)
     & CALL MOM_V_RVISCFLUX(bi,bj,k,vVel,KappaRV,vrf,myThid)

C     Combine fluxes -> fVerV
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kDown) = rVelDvdrFac*aF(i,j) + ArDvdrFac*vrF(i,j)
       ENDDO
      ENDDO

C---  Hydorstatic term (-1/rhoConst . dphi/dy )
      IF (momPressureForcing) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         pF(i,j) = -_recip_dyC(i,j,bi,bj)
     &    *(phi_hyd(i,j,k)-phi_hyd(i,j-1,k))
        ENDDO
       ENDDO
      ENDIF

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) =
#ifdef OLD_UV_GEOM
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)/
     &    ( 0.5 _d 0*(_rA(i,j,bi,bj)+_rA(i,j-1,bi,bj)) )
#else
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rAs(i,j,bi,bj)
#endif
     &  *(fZon(i+1,j)          - fZon(i,j  )
     &   +fMer(i,j  )          - fMer(i,j-1)
     &   +fVerV(i,j,kUp)*rkFac - fVerV(i,j,kDown)*rkFac
     &   )
     & _PHM( +phyFac*pf(i,j) )
       ENDDO
      ENDDO

C-- No-slip and drag BCs appear as body forces in cell abutting topography 
      IF (momViscosity.AND.no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
       CALL MOM_V_SIDEDRAG(bi,bj,k,vFld,v4F,hFacZ,vF,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF
C-    No-slip BCs impose a drag at bottom
      IF (momViscosity.AND.bottomDragTerms) THEN
       CALL MOM_V_BOTTOMDRAG(bi,bj,k,vFld,KE,KappaRV,vF,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Forcing term
      IF (momForcing)
     & CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myCurrentTime,myThid)

C--   Metric terms for curvilinear grid systems
      IF (usingSphericalPolarMTerms) THEN
C      o Spherical polar grid metric terms
       CALL MOM_V_METRIC_NH(bi,bj,k,vFld,wVel,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mTFacV*mT(i,j)
        ENDDO
       ENDDO
       CALL MOM_V_METRIC_SPHERE(bi,bj,k,uFld,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mTFacV*mT(i,j)
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
#ifdef INCLUDE_CD_CODE
      CALL MOM_CDSCHEME(bi,bj,k,phi_hyd,myThid)
#else
      CALL MOM_U_CORIOLIS(bi,bj,k,vFld,cf,myThid)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+fuFac*cf(i,j)
       ENDDO
      ENDDO
      CALL MOM_V_CORIOLIS(bi,bj,k,uFld,cf,myThid)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+fvFac*cf(i,j)
       ENDDO
      ENDDO
#endif /* INCLUDE_CD_CODE */

      RETURN
      END
