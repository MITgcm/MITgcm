C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vecinv.F,v 1.16 2004/05/24 15:15:16 adcroft Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VECINV( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kUp,kDown,
     I        dPhiHydX,dPhiHydY,KappaRU,KappaRV,
     U        fVerU, fVerV,
     I        myTime, myIter, myThid)
C     /==========================================================\
C     | S/R MOM_VECINV                                           |
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
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif

C     == Routine arguments ==
C     fVerU   - Flux of momentum in the vertical
C     fVerV     direction out of the upper face of a cell K
C               ( flux into the cell above ).
C     dPhiHydX,Y :: Gradient (X & Y dir.) of Hydrostatic Potential
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     kUp, kDown                     - Index for upper and lower layers.
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      _RL dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fVerU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      INTEGER kUp,kDown
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER bi,bj,iMin,iMax,jMin,jMax

#ifdef ALLOW_MOM_VECINV

C     == Functions ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Local variables ==
      _RL      aF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vrF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      uCf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vCf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      mT (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      pF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RL wVelBottomOverride
      LOGICAL bottomDragTerms
      LOGICAL writeDiag
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kDown part of fverU/V is set in this subroutine
C--   the kUp is still required
C--   In the case of mom_fluxform Kup is set as well
C--   (at least in part)
      fVerU(1,1,kUp) = fVerU(1,1,kUp)
      fVerV(1,1,kUp) = fVerV(1,1,kUp)
#endif

      rVelMaskOverride=1.
      IF ( k .EQ. 1 ) rVelMaskOverride=freeSurfFac
      wVelBottomOverride=1.
      IF (k.EQ.Nr) wVelBottomOverride=0.
      writeDiag = DIFFERENT_MULTIPLE(diagFreq, myTime,
     &                                         myTime-deltaTClock)

C     Initialise intermediate terms
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        aF(i,j)   = 0.
        vF(i,j)   = 0.
        vrF(i,j)  = 0.
        uCf(i,j)   = 0.
        vCf(i,j)   = 0.
        mT(i,j)   = 0.
        pF(i,j)   = 0.
        del2u(i,j) = 0.
        del2v(i,j) = 0.
        dStar(i,j) = 0.
        zStar(i,j) = 0.
        uDiss(i,j) = 0.
        vDiss(i,j) = 0.
        vort3(i,j) = 0.
        omega3(i,j) = 0.
        ke(i,j) = 0.
#ifdef ALLOW_AUTODIFF_TAMC
        strain(i,j)  = 0. _d 0
        tension(i,j) = 0. _d 0
#endif
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

C note (jmc) : Dissipation and Vort3 advection do not necesary
C              use the same maskZ (and hFacZ)  => needs 2 call(s)
c     CALL MOM_VI_HFACZ_DISS(bi,bj,k,hFacZ,r_hFacZ,myThid)

      CALL MOM_CALC_KE(bi,bj,k,2,uFld,vFld,KE,myThid)

      CALL MOM_VI_CALC_HDIV(bi,bj,k,uFld,vFld,hDiv,myThid)

      CALL MOM_VI_CALC_RELVORT3(bi,bj,k,uFld,vFld,hFacZ,vort3,myThid)

c     CALL MOM_VI_CALC_ABSVORT3(bi,bj,k,vort3,omega3,myThid)

      IF (momViscosity) THEN
C      Calculate del^2 u and del^2 v for bi-harmonic term
       IF (viscA4.NE.0. .OR. viscA4Grid.NE.0.) THEN
         CALL MOM_VI_DEL2UV(bi,bj,k,hDiv,vort3,hFacZ,
     O                      del2u,del2v,
     &                      myThid)
         CALL MOM_VI_CALC_HDIV(bi,bj,k,del2u,del2v,dStar,myThid)
         CALL MOM_VI_CALC_RELVORT3(
     &                         bi,bj,k,del2u,del2v,hFacZ,zStar,myThid)
       ENDIF
C      Calculate dissipation terms for U and V equations
C      in terms of vorticity and divergence
       IF (viscAh.NE.0. .OR. viscA4.NE.0. .OR.
     &      viscAhGrid.NE.0. .OR. viscA4Grid.NE.0. ) THEN
         CALL MOM_VI_HDISSIP(bi,bj,k,hDiv,vort3,hFacZ,dStar,zStar,
     O                       uDiss,vDiss,
     &                       myThid)
       ENDIF
C      or in terms of tension and strain
       IF (viscAstrain.NE.0. .OR. viscAtension.NE.0.) THEN
         CALL MOM_CALC_TENSION(bi,bj,k,uFld,vFld,
     O                         tension,
     I                         myThid)
         CALL MOM_CALC_STRAIN(bi,bj,k,uFld,vFld,hFacZ,
     O                        strain,
     I                        myThid)
         CALL MOM_HDISSIP(bi,bj,k,
     I                    tension,strain,hFacZ,viscAtension,viscAstrain,
     O                    uDiss,vDiss,
     I                    myThid)
       ENDIF
      ENDIF

C-    Return to standard hfacZ (min-4) and mask vort3 accordingly:
c     CALL MOM_VI_MASK_VORT3(bi,bj,k,hFacZ,r_hFacZ,vort3,myThid)

C---- Zonal momentum equation starts here

C--   Vertical flux (fVer is at upper face of "u" cell)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity)
     & CALL MOM_U_RVISCFLUX(bi,bj,k,uVel,KappaRU,vrF,myThid)

C     Combine fluxes
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerU(i,j,kDown) = ArDudrFac*vrF(i,j)
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        gU(i,j,k,bi,bj) = uDiss(i,j)
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rAw(i,j,bi,bj)
     &  *(
     &   +fVerU(i,j,kUp)*rkFac - fVerU(i,j,kDown)*rkFac
     &   )
     &  - phxFac*dPhiHydX(i,j)
       ENDDO
      ENDDO

C-- No-slip and drag BCs appear as body forces in cell abutting topography 
      IF (momViscosity.AND.no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
       CALL MOM_U_SIDEDRAG(bi,bj,k,uFld,del2u,hFacZ,vF,myThid)
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

C--   Metric terms for curvilinear grid systems
c     IF (usingSphericalPolarMTerms) THEN
C      o Spherical polar grid metric terms
c      CALL MOM_U_METRIC_NH(bi,bj,k,uFld,wVel,mT,myThid)
c      DO j=jMin,jMax
c       DO i=iMin,iMax
c        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mTFacU*mT(i,j)
c       ENDDO
c      ENDDO
c     ENDIF

C---- Meridional momentum equation starts here

C--   Vertical flux (fVer is at upper face of "v" cell)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity)
     & CALL MOM_V_RVISCFLUX(bi,bj,k,vVel,KappaRV,vrf,myThid)

C     Combine fluxes -> fVerV
      DO j=jMin,jMax
       DO i=iMin,iMax
        fVerV(i,j,kDown) = ArDvdrFac*vrF(i,j)
       ENDDO
      ENDDO

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,k,bi,bj) = vDiss(i,j)
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rAs(i,j,bi,bj)
     &  *(
     &   +fVerV(i,j,kUp)*rkFac - fVerV(i,j,kDown)*rkFac
     &   )
     &  - phyFac*dPhiHydY(i,j)
       ENDDO
      ENDDO

C-- No-slip and drag BCs appear as body forces in cell abutting topography 
      IF (momViscosity.AND.no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
       CALL MOM_V_SIDEDRAG(bi,bj,k,vFld,del2v,hFacZ,vF,myThid)
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

C--   Metric terms for curvilinear grid systems
c     IF (usingSphericalPolarMTerms) THEN
C      o Spherical polar grid metric terms
c      CALL MOM_V_METRIC_NH(bi,bj,k,vFld,wVel,mT,myThid)
c      DO j=jMin,jMax
c       DO i=iMin,iMax
c        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mTFacV*mT(i,j)
c       ENDDO
c      ENDDO
c     ENDIF

C--   Horizontal Coriolis terms
      IF (useCoriolis .AND. .NOT.useCDscheme) THEN
       CALL MOM_VI_CORIOLIS(bi,bj,k,uFld,vFld,omega3,hFacZ,r_hFacZ,
     &                      uCf,vCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO
       IF ( writeDiag ) THEN
        CALL WRITE_LOCAL_RL('fV','I10',1,uCf,bi,bj,k,myIter,myThid)
        CALL WRITE_LOCAL_RL('fU','I10',1,vCf,bi,bj,k,myIter,myThid)
       ENDIF
      ENDIF

      IF (momAdvection) THEN
C--   Horizontal advection of relative vorticity
c      CALL MOM_VI_U_CORIOLIS(bi,bj,K,vFld,omega3,r_hFacZ,uCf,myThid)
       CALL MOM_VI_U_CORIOLIS(bi,bj,k,vFld,vort3,hFacZ,r_hFacZ,
     &                        uCf,myThid)
c      CALL MOM_VI_U_CORIOLIS_C4(bi,bj,K,vFld,vort3,r_hFacZ,uCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
        ENDDO
       ENDDO
c      CALL MOM_VI_V_CORIOLIS(bi,bj,K,uFld,omega3,r_hFacZ,vCf,myThid)
       CALL MOM_VI_V_CORIOLIS(bi,bj,k,uFld,vort3,hFacZ,r_hFacZ,
     &                        vCf,myThid)
c      CALL MOM_VI_V_CORIOLIS_C4(bi,bj,K,uFld,vort3,r_hFacZ,vCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO

       IF ( writeDiag ) THEN
        CALL WRITE_LOCAL_RL('zV','I10',1,uCf,bi,bj,k,myIter,myThid)
        CALL WRITE_LOCAL_RL('zU','I10',1,vCf,bi,bj,k,myIter,myThid)
       ENDIF
#ifdef ALLOW_TIMEAVE
#ifndef HRCUBE
       IF (taveFreq.GT.0.) THEN
         CALL TIMEAVE_CUMUL_1K1T(uZetatave,vCf,deltaTClock,
     &                           Nr, k, bi, bj, myThid)
         CALL TIMEAVE_CUMUL_1K1T(vZetatave,uCf,deltaTClock,
     &                           Nr, k, bi, bj, myThid)
       ENDIF
#endif /* ALLOW_TIMEAVE */
#endif /* ndef HRCUBE */

C--   Vertical shear terms (-w*du/dr & -w*dv/dr)
       IF ( .NOT. momImplVertAdv ) THEN
        CALL MOM_VI_U_VERTSHEAR(bi,bj,K,uVel,wVel,uCf,myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
         ENDDO
        ENDDO
        CALL MOM_VI_V_VERTSHEAR(bi,bj,K,vVel,wVel,vCf,myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
         ENDDO
        ENDDO
       ENDIF

C--   Bernoulli term
       CALL MOM_VI_U_GRAD_KE(bi,bj,K,KE,uCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
        ENDDO
       ENDDO
       CALL MOM_VI_V_GRAD_KE(bi,bj,K,KE,vCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO
       IF ( writeDiag ) THEN
        CALL WRITE_LOCAL_RL('KEx','I10',1,uCf,bi,bj,k,myIter,myThid)
        CALL WRITE_LOCAL_RL('KEy','I10',1,vCf,bi,bj,k,myIter,myThid)
       ENDIF

C--   end if momAdvection
      ENDIF

C--   Set du/dt & dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO


      IF ( writeDiag ) THEN
       CALL WRITE_LOCAL_RL('Ds','I10',1,strain,bi,bj,k,myIter,myThid)
       CALL WRITE_LOCAL_RL('Dt','I10',1,tension,bi,bj,k,myIter,myThid)
       CALL WRITE_LOCAL_RL('Du','I10',1,uDiss,bi,bj,k,myIter,myThid)
       CALL WRITE_LOCAL_RL('Dv','I10',1,vDiss,bi,bj,k,myIter,myThid)
       CALL WRITE_LOCAL_RL('Z3','I10',1,vort3,bi,bj,k,myIter,myThid)
c      CALL WRITE_LOCAL_RL('W3','I10',1,omega3,bi,bj,k,myIter,myThid)
       CALL WRITE_LOCAL_RL('KE','I10',1,KE,bi,bj,k,myIter,myThid)
       CALL WRITE_LOCAL_RL('D','I10',1,hdiv,bi,bj,k,myIter,myThid)
      ENDIF

#endif /* ALLOW_MOM_VECINV */

      RETURN
      END
