C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_fluxform.F,v 1.22 2005/06/22 00:32:15 jmc Exp $
C $Name:  $

CBOI
C !TITLE: pkg/mom\_advdiff
C !AUTHORS: adcroft@mit.edu
C !INTRODUCTION: Flux-form Momentum Equations Package
C
C Package "mom\_fluxform" provides methods for calculating explicit terms
C in the momentum equation cast in flux-form:
C \begin{eqnarray*}
C G^u & = & -\frac{1}{\rho} \partial_x \phi_h
C           -\nabla \cdot {\bf v} u
C           -fv
C           +\frac{1}{\rho} \nabla \cdot {\bf \tau}^x
C           + \mbox{metrics}
C \\
C G^v & = & -\frac{1}{\rho} \partial_y \phi_h
C           -\nabla \cdot {\bf v} v
C           +fu
C           +\frac{1}{\rho} \nabla \cdot {\bf \tau}^y
C           + \mbox{metrics}
C \end{eqnarray*}
C where ${\bf v}=(u,v,w)$ and $\tau$, the stress tensor, includes surface
C stresses as well as internal viscous stresses.
CEOI

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_FLUXFORM

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_FLUXFORM( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kUp,kDown,
     I        dPhihydX,dPhiHydY,KappaRU,KappaRV,
     U        fVerU, fVerV,
     I        myTime,myIter,myThid)

C !DESCRIPTION:
C Calculates all the horizontal accelerations except for the implicit surface
C pressure gradient and implciit vertical viscosity.

C !USES: ===============================================================
C     == Global variables ==
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  iMin,iMax,jMin,jMAx  :: loop ranges
C  k                    :: vertical level
C  kUp                  :: =1 or 2 for consecutive k
C  kDown                :: =2 or 1 for consecutive k
C  dPhiHydX,Y           :: Gradient (X & Y dir.) of Hydrostatic Potential 
C  KappaRU              :: vertical viscosity
C  KappaRV              :: vertical viscosity
C  fVerU                :: vertical flux of U, 2 1/2 dim for pipe-lining
C  fVerV                :: vertical flux of V, 2 1/2 dim for pipe-lining
C  myTime               :: current time
C  myIter               :: current time-step number
C  myThid               :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k,kUp,kDown
      _RL dPhiHydX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fVerU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C None - updates gU() and gV() in common blocks

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  aF                   :: advective flux
C  vF                   :: viscous flux
C  v4F                  :: bi-harmonic viscous flux
C  vrF                  :: vertical viscous flux
C  cF                   :: Coriolis acceleration
C  mT                   :: Metric terms
C  pF                   :: Pressure gradient
C  fZon                 :: zonal fluxes
C  fMer                 :: meridional fluxes
      INTEGER i,j
      _RL aF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL v4F(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RS    hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS  r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS      yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rTransU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rTransV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     I,J,K - Loop counters
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
      INTEGER km1,kp1
      _RL wVelBottomOverride
      LOGICAL bottomDragTerms
CEOP

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
        rTransU(i,j) = 0.
        rTransV(i,j) = 0.
        strain(i,j) = 0.
        tension(i,j) = 0.
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

      IF (     no_slip_bottom
     &    .OR. bottomDragQuadratic.NE.0.
     &    .OR. bottomDragLinear.NE.0.) THEN
       bottomDragTerms=.TRUE.
      ELSE
       bottomDragTerms=.FALSE.
      ENDIF

C-- with stagger time stepping, grad Phi_Hyp is directly incoporated in TIMESTEP
      IF (staggerTimeStep) THEN
        phxFac = 0.
        phyFac = 0.
      ENDIF

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

      CALL MOM_CALC_KE(bi,bj,k,3,uFld,vFld,KE,myThid)

      IF (viscAstrain.NE.0. .OR. viscAtension.NE.0.) THEN
         CALL MOM_CALC_TENSION(bi,bj,k,uFld,vFld,
     O                         tension,
     I                         myThid)
         CALL MOM_CALC_STRAIN(bi,bj,k,uFld,vFld,hFacZ,
     O                        strain,
     I                        myThid)
      ENDIF

C---  First call (k=1): compute vertical adv. flux fVerU(kUp) & fVerV(kUp)
      IF (momAdvection.AND.k.EQ.1) THEN

C-    Calculate vertical transports above U & V points (West & South face):
       CALL MOM_CALC_RTRANS( k, bi, bj,
     O                       rTransU, rTransV,
     I                       myTime, myIter, myThid)

C-    Free surface correction term (flux at k=1)
       CALL MOM_U_ADV_WU(bi,bj,k,uVel,wVel,rTransU,af,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerU(i,j,kUp) = af(i,j)
        ENDDO
       ENDDO

       CALL MOM_V_ADV_WV(bi,bj,k,vVel,wVel,rTransV,af,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerV(i,j,kUp) = af(i,j)
        ENDDO
       ENDDO

C---  endif momAdvection & k=1
      ENDIF


C---  Calculate vertical transports (at k+1) below U & V points :
      IF (momAdvection) THEN
       CALL MOM_CALC_RTRANS( k+1, bi, bj,
     O                       rTransU, rTransV,
     I                       myTime, myIter, myThid)
      ENDIF

c     IF (momViscosity) THEN
c    &  CALL MOM_CALC_VISCOSITY(bi,bj,k,
c    I                         uFld,vFld,
c    O                         viscAh_D,viscAh_Z,myThid)

C---- Zonal momentum equation starts here

C     Bi-harmonic term del^2 U -> v4F
      IF (momViscosity .AND. viscA4.NE.0. ) 
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
      DO j=jMin,jMax+1
       DO i=iMin,iMax
        fMer(i,j) = vDudyFac*aF(i,j) + AhDudyFac*vF(i,j)
       ENDDO
      ENDDO

C--   Vertical flux (fVer is at upper face of "u" cell)

C     Mean flow component of vertical flux (at k+1) -> aF
      IF (momAdvection)
     & CALL MOM_U_ADV_WU(bi,bj,k+1,uVel,wVel,rTransU,af,myThid)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity)
     & CALL MOM_U_RVISCFLUX(bi,bj,k,uVel,KappaRU,vrF,myThid)

C     Combine fluxes
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerU(i,j,kDown) = rVelDudrFac*aF(i,j) + ArDudrFac*vrF(i,j)
       ENDDO
      ENDDO

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
     &   -fVerU(i,j,kUp)*rkSign + fVerU(i,j,kDown)*rkSign
     &   )
     &  - phxFac*dPhiHydX(i,j)
       ENDDO
      ENDDO

#ifdef NONLIN_FRSURF
C-- account for 3.D divergence of the flow in rStar coordinate:
      IF ( momAdvection .AND. select_rStar.GT.0 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     - (rStarExpW(i,j,bi,bj) - 1. _d 0)/deltaTfreesurf
     &       *uVel(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF
      IF ( momAdvection .AND. select_rStar.LT.0 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     - rStarDhWDt(i,j,bi,bj)*uVel(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF
#endif /* NONLIN_FRSURF */

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

C--   Forcing term (moved to timestep.F)
c     IF (momForcing)
c    &  CALL EXTERNAL_FORCING_U(
c    I     iMin,iMax,jMin,jMax,bi,bj,k,
c    I     myTime,myThid)

C--   Metric terms for curvilinear grid systems
      IF (useNHMTerms) THEN
C      o Non-hydrosatic metric terms
       CALL MOM_U_METRIC_NH(bi,bj,k,uFld,wVel,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mTFacU*mT(i,j)
        ENDDO
       ENDDO
      ENDIF
      IF (usingSphericalPolarMTerms) THEN
       CALL MOM_U_METRIC_SPHERE(bi,bj,k,uFld,vFld,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mTFacU*mT(i,j)
        ENDDO
       ENDDO
                                                                                
      ENDIF
      IF (usingCylindricalGrid) THEN
         CALL MOM_U_METRIC_CYLINDER(bi,bj,k,uFld,vFld,mT,myThid)
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
      IF (momViscosity .AND. viscA4.NE.0. ) 
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
       DO i=iMin,iMax+1
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

C     o Mean flow component of vertical flux
      IF (momAdvection)
     & CALL MOM_V_ADV_WV(bi,bj,k+1,vVel,wVel,rTransV,af,myThid)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity)
     & CALL MOM_V_RVISCFLUX(bi,bj,k,vVel,KappaRV,vrf,myThid)

C     Combine fluxes -> fVerV
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kDown) = rVelDvdrFac*aF(i,j) + ArDvdrFac*vrF(i,j)
       ENDDO
      ENDDO

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
     &   -fVerV(i,j,kUp)*rkSign + fVerV(i,j,kDown)*rkSign
     &   )
     &  - phyFac*dPhiHydY(i,j)
       ENDDO
      ENDDO

#ifdef NONLIN_FRSURF
C-- account for 3.D divergence of the flow in rStar coordinate:
      IF ( momAdvection .AND. select_rStar.GT.0 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     - (rStarExpS(i,j,bi,bj) - 1. _d 0)/deltaTfreesurf
     &       *vVel(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF
      IF ( momAdvection .AND. select_rStar.LT.0 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     - rStarDhSDt(i,j,bi,bj)*vVel(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF
#endif /* NONLIN_FRSURF */

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

C--   Forcing term (moved to timestep.F)
c     IF (momForcing)
c    & CALL EXTERNAL_FORCING_V(
c    I     iMin,iMax,jMin,jMax,bi,bj,k,
c    I     myTime,myThid)

C--   Metric terms for curvilinear grid systems
      IF (useNHMTerms) THEN
C      o Spherical polar grid metric terms
       CALL MOM_V_METRIC_NH(bi,bj,k,vFld,wVel,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mTFacV*mT(i,j)
        ENDDO
       ENDDO
      ENDIF
      IF (usingSphericalPolarMTerms) THEN
       CALL MOM_V_METRIC_SPHERE(bi,bj,k,uFld,mT,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mTFacV*mT(i,j)
        ENDDO
       ENDDO
      ENDIF
      IF (usingCylindricalGrid) THEN
         CALL MOM_V_METRIC_CYLINDER(bi,bj,k,uFld,vFld,mT,myThid)
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
c     IF (useCDscheme) THEN
c       CALL MOM_CDSCHEME(bi,bj,k,dPhiHydX,dPhiHydY,myThid)
c     ELSE
      IF (.NOT.useCDscheme) THEN
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
      ENDIF

      IF (nonHydrostatic.OR.quasiHydrostatic) THEN
       CALL MOM_U_CORIOLIS_NH(bi,bj,k,wVel,cf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+fuFac*cf(i,j)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
