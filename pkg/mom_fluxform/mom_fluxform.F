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
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif
#ifdef ALLOW_GGL90
# include "GGL90_OPTIONS.h"
#endif
C- to switch back to (very) old accounting of geometry/Area factor
#undef OLD_UV_GEOM

CBOP
C !ROUTINE: MOM_FLUXFORM

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_FLUXFORM(
     I        bi,bj,k,iMin,iMax,jMin,jMax,
     I        kappaRU, kappaRV,
     U        fVerUkm, fVerVkm,
     O        fVerUkp, fVerVkp,
     O        guDiss, gvDiss,
     I        myTime, myIter, myThid )

C !DESCRIPTION:
C Calculates all the horizontal accelerations except for the implicit surface
C pressure gradient and implicit vertical viscosity.

C !USES: ===============================================================
C     == Global variables ==
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif
#if ( defined ALLOW_GGL90 && defined ALLOW_GGL90_LANGMUIR )
# include "GGL90.h"
#endif
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# include "MOM_FLUXFORM.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: current tile indices
C  k                    :: current vertical level
C  iMin,iMax,jMin,jMax  :: loop ranges
C  kappaRU              :: vertical viscosity
C  kappaRV              :: vertical viscosity
C  fVerUkm              :: vertical advective flux of U, interface above (k-1/2)
C  fVerVkm              :: vertical advective flux of V, interface above (k-1/2)
C  fVerUkp              :: vertical advective flux of U, interface below (k+1/2)
C  fVerVkp              :: vertical advective flux of V, interface below (k+1/2)
C  guDiss               :: dissipation tendency (all explicit terms), u component
C  gvDiss               :: dissipation tendency (all explicit terms), v component
C  myTime               :: current time
C  myIter               :: current time-step number
C  myThid               :: my Thread Id number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL fVerUkm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerVkm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerUkp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerVkp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL guDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gvDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C None - updates gU() and gV() in common blocks

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  gAdd                 :: individual momentum tendency term to add
C  del2                 :: horizontal second derivative (Laplacian)
C  uCf,vCf              :: Coriolis acceleration
C  vMT                  :: vertical/Non-Hyd. metric terms
C  fZon                 :: zonal fluxes
C  fMer                 :: meridional fluxes
C  fVrUp,fVrDw          :: vertical viscous fluxes at interface k & k+1
      INTEGER i,j
#ifdef ALLOW_AUTODIFF_TAMC
C     kkey :: tape key (depends on level and tile indices)
      INTEGER kkey
#endif
      _RL gAdd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  uCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vMT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVrUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVrDw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     afFacMom     :: Tracer parameters for turning terms on and off.
C     vfFacMom
C     pfFacMom        afFacMom - Advective terms
C     cfFacMom        vfFacMom - Eddy viscosity terms
C     mtFacMom        pfFacMom - Pressure terms
C                     cfFacMom - Coriolis terms
C                     foFacMom - Forcing
C                     mtFacMom - Metric term
C     uDudxFac, AhDudxFac, etc ... individual term parameters for switching terms off
      _RS    hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS   h0FacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS  r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS       xA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS       yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL   uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL   vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rTransU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  rTransV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#if ( defined ALLOW_GGL90 && defined ALLOW_GGL90_LANGMUIR )
      _RL     uRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
      _RL       KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    cDrag(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL   strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_LEITH_QG
      _RL  Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
      _RL  uDudxFac
      _RL  AhDudxFac
      _RL  vDudyFac
      _RL  AhDudyFac
      _RL  wDudrFac
      _RL  ArDudrFac
      _RL  fuFac
      _RL  mtFacU
      _RL  mtNHFacU
      _RL  uDvdxFac
      _RL  AhDvdxFac
      _RL  vDvdyFac
      _RL  AhDvdyFac
      _RL  wDvdrFac
      _RL  ArDvdrFac
      _RL  fvFac
      _RL  mtFacV
      _RL  mtNHFacV
      _RL  sideMaskFac
      _RL  rAdvDeepFac
      LOGICAL bottomDragTerms, diagFlag
CEOP
#ifdef MOM_BOUNDARY_CONSERVE
      COMMON / MOM_FLUXFORM_LOCAL / uBnd, vBnd
      _RL  uBnd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vBnd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* MOM_BOUNDARY_CONSERVE */

#ifdef ALLOW_AUTODIFF_TAMC
      kkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
      kkey = k  + (kkey-1)*Nr
#endif /* ALLOW_AUTODIFF_TAMC */

C     Initialise intermediate terms
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        gAdd(i,j) = 0.
        del2(i,j) = 0.
        uCf(i,j)  = 0.
        vCf(i,j)  = 0.
        vMT(i,j)  = 0.
        fZon(i,j) = 0.
        fMer(i,j) = 0.
        fVrUp(i,j)= 0.
        fVrDw(i,j)= 0.
        rTransU(i,j)= 0.
        rTransV(i,j)= 0.
c       KE(i,j)     = 0.
        hDiv(i,j)   = 0.
        vort3(i,j)  = 0.
        strain(i,j) = 0.
        tension(i,j)= 0.
        stretching(i,j) = 0.
#ifdef ALLOW_LEITH_QG
        Nsquare(i,j) = 0.
#endif
        guDiss(i,j) = 0.
        gvDiss(i,j) = 0.
#if ( defined ALLOW_GGL90 && defined ALLOW_GGL90_LANGMUIR )
c       uRes(i,j)   = 0.
c       vRes(i,j)   = 0.
#endif
       ENDDO
      ENDDO

C--   Term by term tracer parmeters
C     o U momentum equation
      uDudxFac     = afFacMom*1.
      AhDudxFac    = vfFacMom*1.
      vDudyFac     = afFacMom*1.
      AhDudyFac    = vfFacMom*1.
      wDudrFac     = afFacMom*1.
      ArDudrFac    = vfFacMom*1.
      mtFacU       = mtFacMom*1.
      mtNHFacU     = 1.
      fuFac        = cfFacMom*1.
C     o V momentum equation
      uDvdxFac     = afFacMom*1.
      AhDvdxFac    = vfFacMom*1.
      vDvdyFac     = afFacMom*1.
      AhDvdyFac    = vfFacMom*1.
      wDvdrFac     = afFacMom*1.
      ArDvdrFac    = vfFacMom*1.
      mtFacV       = mtFacMom*1.
      mtNHFacV     = 1.
      fvFac        = cfFacMom*1.

      IF (implicitViscosity) THEN
        ArDudrFac  = 0.
        ArDvdrFac  = 0.
      ENDIF

C note: using standard stencil (no mask) results in under-estimating
C       vorticity at a no-slip boundary by a factor of 2 = sideDragFactor
      IF ( no_slip_sides ) THEN
        sideMaskFac = sideDragFactor
      ELSE
        sideMaskFac = 0. _d 0
      ENDIF

      IF ( selectImplicitDrag.EQ.0 .AND.
     &      (  no_slip_bottom
     &    .OR. selectBotDragQuadr.GE.0
     &    .OR. bottomDragLinear.NE.0. ) ) THEN
       bottomDragTerms=.TRUE.
      ELSE
       bottomDragTerms=.FALSE.
      ENDIF

C--   Calculate open water fraction at vorticity points
      CALL MOM_CALC_HFACZ( bi,bj,k,hFacZ,r_hFacZ,myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hFacZ(:,:)  = comlev1_bibj_k, key = kkey, byte = isbyte
#endif

C---- Calculate common quantities used in both U and V equations
C     Calculate tracer cell face open areas
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &          *drF(k)*_hFacW(i,j,k,bi,bj)
        yA(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &          *drF(k)*_hFacS(i,j,k,bi,bj)
        h0FacZ(i,j) = hFacZ(i,j)
       ENDDO
      ENDDO
#ifdef NONLIN_FRSURF
      IF ( momViscosity .AND. no_slip_sides
     &                  .AND. nonlinFreeSurf.GT.0 ) THEN
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
          h0FacZ(i,j) = MIN(
     &       MIN( h0FacW(i,j,k,bi,bj), h0FacW(i,j-1,k,bi,bj) ),
     &       MIN( h0FacS(i,j,k,bi,bj), h0FacS(i-1,j,k,bi,bj) ) )
         ENDDO
        ENDDO
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE h0FacZ(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
# endif
#endif /* NONLIN_FRSURF */

C     Make local copies of horizontal flow field
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uFld(i,j) = uVel(i,j,k,bi,bj)
        vFld(i,j) = vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate velocity field "volume transports" through tracer cell faces.
C     anelastic: transports are scaled by rhoFacC (~ mass transport)
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uTrans(i,j) = uFld(i,j)*xA(i,j)*rhoFacC(k)
        vTrans(i,j) = vFld(i,j)*yA(i,j)*rhoFacC(k)
       ENDDO
      ENDDO

      CALL MOM_CALC_KE( bi,bj,k,2,uFld,vFld,KE,myThid )
      IF ( useVariableVisc ) THEN
        CALL MOM_CALC_HDIV( bi,bj,k,2,uFld,vFld,hDiv,myThid )
        CALL MOM_CALC_RELVORT3( bi,bj,k,uFld,vFld,hFacZ,vort3,myThid )
        CALL MOM_CALC_TENSION( bi,bj,k,uFld,vFld,tension,myThid )
        CALL MOM_CALC_STRAIN( bi,bj,k,uFld,vFld,hFacZ,strain,myThid )
#ifdef ALLOW_LEITH_QG
        IF ( viscC2LeithQG.NE.zeroRL ) THEN
          CALL MOM_VISC_QGL_STRETCH(bi,bj,k,
     O                            stretching, Nsquare,
     I                            myTime, myIter, myThid)
          CALL MOM_VISC_QGL_LIMIT(bi,bj,k,
     O                          stretching,
     I                          Nsquare, uFld,vFld,vort3,
     I                          myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_LEITH_QG */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           IF ( hFacZ(i,j).EQ.0. ) THEN
             vort3(i,j)  = sideMaskFac*vort3(i,j)
             strain(i,j) = sideMaskFac*strain(i,j)
           ENDIF
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(hDiv,   'momHDiv ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(vort3,  'momVort3',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(tension,'Tension ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(strain, 'Strain  ',k,1,2,bi,bj,myThid)
C     stretching will be zero, unless using QG Leith
          IF ( viscC2LeithQG.NE.zeroRL ) THEN
            CALL DIAGNOSTICS_FILL(stretching,
     I                            'Stretch ',k,1,2,bi,bj,myThid)
          ENDIF
        ENDIF
#endif
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
C     KE is used a lot, so we save here in order to avoid multiple
C     recomputations of this variable.
CADJ STORE KE(:,:)      = comlev1_bibj_k, key = kkey, byte = isbyte
C     Variables tension and strain are relatively cheap to recompute, so
C     we do not save them here (but saving them would clearly avoid
C     the recomputations).
cCADJ STORE tension(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
cCADJ STORE strain(:,:)  = comlev1_bibj_k, key = kkey, byte = isbyte
C     hDiv and vort3 are relatively expensive to recompute, so we save
C     them here.
CADJ STORE hDiv(:,:)    = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE vort3(:,:)   = comlev1_bibj_k, key = kkey, byte = isbyte
#endif

C---  First call (k=1): compute vertical adv. flux fVerUkm & fVerVkm
      IF (momAdvection.AND.k.EQ.1) THEN

#ifdef MOM_BOUNDARY_CONSERVE
        CALL MOM_UV_BOUNDARY( bi, bj, k,
     I                        uVel, vVel,
     O                        uBnd(1-OLx,1-OLy,k,bi,bj),
     O                        vBnd(1-OLx,1-OLy,k,bi,bj),
     I                        myTime, myIter, myThid )
#endif /* MOM_BOUNDARY_CONSERVE */

C-    Calculate vertical transports above U & V points (West & South face):

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
#  ifndef DISABLE_RSTAR_CODE
CADJ STORE dwtransc(:,:,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dwtransu(:,:,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dwtransv(:,:,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_AUTODIFF_TAMC */
        CALL MOM_CALC_RTRANS( k, bi, bj,
     O                        rTransU, rTransV,
     I                        myTime, myIter, myThid )

C-    Free surface correction term (flux at k=1)
        CALL MOM_U_ADV_WU( bi, bj, k, deepFacAdv,
     I                     uVel, wVel, rTransU,
     O                     fVerUkm, myThid )

        CALL MOM_V_ADV_WV( bi, bj, k, deepFacAdv,
     I                     vVel, wVel, rTransV,
     O                     fVerVkm, myThid )

C---  endif momAdvection & k=1
      ENDIF

C---  Calculate vertical transports (at k+1) below U & V points :
      IF (momAdvection) THEN
        CALL MOM_CALC_RTRANS( k+1, bi, bj,
     O                        rTransU, rTransV,
     I                        myTime, myIter, myThid )
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
C     This saves us from calling the relatively expensive
C     mom_calc_rtrans in AD mode.
CADJ STORE rTransU(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE rTransV(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif

#ifdef MOM_BOUNDARY_CONSERVE
      IF ( momAdvection .AND. k.LT.Nr ) THEN
        CALL MOM_UV_BOUNDARY( bi, bj, k+1,
     I                        uVel, vVel,
     O                        uBnd(1-OLx,1-OLy,k+1,bi,bj),
     O                        vBnd(1-OLx,1-OLy,k+1,bi,bj),
     I                        myTime, myIter, myThid )
      ENDIF
#endif /* MOM_BOUNDARY_CONSERVE */

      IF (momViscosity) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         viscAh_D(i,j) = viscAhD
         viscAh_Z(i,j) = viscAhZ
         viscA4_D(i,j) = viscA4D
         viscA4_Z(i,j) = viscA4Z
        ENDDO
       ENDDO
       IF ( useVariableVisc ) THEN
        CALL MOM_CALC_VISC( bi, bj, k,
     O           viscAh_Z, viscAh_D, viscA4_Z, viscA4_D,
     I           hDiv, vort3, tension, strain, stretching, KE, hFacZ,
     I           myThid )
       ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
# ifndef AUTODIFF_ALLOW_VISCFACADJ
C     These store directive must not be here, if you want to recompute
C     these viscosity coefficients with a modified viscFacAdj = viscFacInAd
C     because the store directives intentionally prevent the recomputation.
CADJ STORE viscAh_Z(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE viscAh_D(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE viscA4_Z(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE viscA4_D(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
# endif /* AUTODIFF_ALLOW_VISCFACADJ */
#endif /* ALLOW_AUTODIFF_TAMC */
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---- Zonal momentum equation starts here

      IF (momAdvection) THEN
C---  Calculate mean fluxes (advection)   between cells for zonal flow.

C--   For Deep-Model (deepAtmosphere=T), no need for NHMTerm w*u/r since
C     vertical advective flux (in MOM_U_ADV_WU) is changed to W*(u*deepFacC)
C     with rAdvDeepFac factor in flux divergence

#ifdef MOM_BOUNDARY_CONSERVE
        CALL MOM_U_ADV_UU( bi,bj,k,uTrans,uBnd(1-OLx,1-OLy,k,bi,bj),
     O                     fZon,myThid )
        CALL MOM_U_ADV_VU( bi,bj,k,vTrans,uBnd(1-OLx,1-OLy,k,bi,bj),
     O                     fMer,myThid )
        CALL MOM_U_ADV_WU( bi, bj, k+1, deepFacAdv,
     I                     uBnd, wVel, rTransU,
     O                     fVerUkp, myThid )
#else /* MOM_BOUNDARY_CONSERVE */
C--   Zonal flux (fZon is at east face of "u" cell)
C     Mean flow component of zonal flux -> fZon
        CALL MOM_U_ADV_UU( bi,bj,k,uTrans,uFld,fZon,myThid )

C--   Meridional flux (fMer is at south face of "u" cell)
C     Mean flow component of meridional flux -> fMer
        CALL MOM_U_ADV_VU( bi,bj,k,vTrans,uFld,fMer,myThid )

C--   Vertical flux (fVer is at upper face of "u" cell)
C     Mean flow component of vertical flux (at k+1) -> fVer
        CALL MOM_U_ADV_WU( bi, bj, k+1, deepFacAdv,
     I                     uVel, wVel, rTransU,
     O                     fVerUkp, myThid )
#endif /* MOM_BOUNDARY_CONSERVE */

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
        rAdvDeepFac = wDudrFac*rkSign
#ifndef MOM_USE_OLD_DEEP_VERT_ADV
        IF ( useNHMTerms ) rAdvDeepFac = rAdvDeepFac*recip_deepFacC(k)
#endif
        IF ( selectMetricTerms.EQ.3 ) THEN
C-      In this case fMer contains the advective flux vTrans*(u*dxC)
         DO j=jMin,jMax
          DO i=iMin,iMax
           gU(i,j,k,bi,bj) =
     &      -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      *recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
     &      *( ( fZon( i, j ) - fZon(i-1,j ) )*uDudxFac
     &        +( fMer( i,j+1) - fMer( i, j ) )*vDudyFac
     &                                        *recip_dxC(i,j,bi,bj)
     &        +( fVerUkp(i,j) - fVerUkm(i,j) )*rAdvDeepFac
     &       )
          ENDDO
         ENDDO
        ELSE
         DO j=jMin,jMax
          DO i=iMin,iMax
           gU(i,j,k,bi,bj) =
#ifdef OLD_UV_GEOM
     &      -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)/
     &      ( 0.5 _d 0*(rA(i,j,bi,bj)+rA(i-1,j,bi,bj)) )
#else
     &      -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &      *recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
#endif
     &      *( ( fZon( i, j ) - fZon(i-1,j ) )*uDudxFac
     &        +( fMer( i,j+1) - fMer( i, j ) )*vDudyFac
     &        +( fVerUkp(i,j) - fVerUkm(i,j) )*rAdvDeepFac
     &       )
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL( fZon,  'ADVx_Um ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL( fMer,  'ADVy_Um ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(fVerUkm,'ADVrE_Um',k,1,2,bi,bj,myThid)
        ENDIF
#endif

#ifdef NONLIN_FRSURF
C-- account for 3.D divergence of the flow in rStar coordinate:
# ifndef DISABLE_RSTAR_CODE
        IF ( select_rStar.GT.0 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     - (rStarExpW(i,j,bi,bj) - 1. _d 0)/deltaTFreeSurf
     &       *uVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        IF ( select_rStar.LT.0 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     - rStarDhWDt(i,j,bi,bj)*uVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
# endif /* DISABLE_RSTAR_CODE */
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_ADDFLUID
        IF ( selectAddFluid.GE.1 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     + uVel(i,j,k,bi,bj)*mass2rUnit*0.5 _d 0
     &      *( addMass(i-1,j,k,bi,bj) + addMass(i,j,k,bi,bj) )
     &      *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)*recip_rhoFacC(k)
     &      * recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_ADDFLUID */

      ELSE
C-    if momAdvection / else
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gU(i,j,k,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C-    endif momAdvection.
      ENDIF

      IF (momViscosity) THEN
C---  Calculate eddy fluxes (dissipation) between cells for zonal flow.

C     Bi-harmonic term del^2 U -> del2
        IF ( useBiharmonicVisc )
     &  CALL MOM_U_DEL2U( bi, bj, k, uFld, hFacZ, h0FacZ,
     O                    del2, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE del2(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif

C     Laplacian and bi-harmonic terms, Zonal  Fluxes -> fZon
        CALL MOM_U_XVISCFLUX( bi,bj,k,uFld,del2,fZon,
     I                        viscAh_D,viscA4_D,myThid )

C     Laplacian and bi-harmonic termis, Merid Fluxes -> fMer
        CALL MOM_U_YVISCFLUX( bi,bj,k,uFld,del2,hFacZ,fMer,
     I                        viscAh_Z,viscA4_Z,myThid )

C     Eddy component of vertical flux (interior component only) -> fVrUp & fVrDw
       IF (.NOT.implicitViscosity) THEN
        CALL MOM_U_RVISCFLUX( bi,bj, k, uVel,kappaRU,fVrUp,myThid )
        CALL MOM_U_RVISCFLUX( bi,bj,k+1,uVel,kappaRU,fVrDw,myThid )
       ENDIF

C--   Tendency is minus divergence of the fluxes
C     anelastic: hor.visc.fluxes are not scaled by rhoFac (by vert.visc.flx is)
        DO j=jMin,jMax
         DO i=iMin,iMax
          guDiss(i,j) =
#ifdef OLD_UV_GEOM
     &     -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)/
     &      ( 0.5 _d 0*(rA(i,j,bi,bj)+rA(i-1,j,bi,bj)) )
#else
     &     -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &     *recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)
#endif
     &     *( ( fZon(i,j  ) - fZon(i-1,j) )*AhDudxFac
     &       +( fMer(i,j+1) - fMer(i,  j) )*AhDudyFac
     &       +( fVrDw(i,j)  - fVrUp(i,j)  )*rkSign*ArDudrFac
     &                                     *recip_rhoFacC(k)
     &     )
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(fZon, 'VISCx_Um',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(fMer, 'VISCy_Um',k,1,2,bi,bj,myThid)
          IF (.NOT.implicitViscosity)
     &    CALL DIAGNOSTICS_FILL(fVrUp,'VISrE_Um',k,1,2,bi,bj,myThid)
        ENDIF
#endif

C-- No-slip and drag BCs appear as body forces in cell abutting topography
        IF (no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
         CALL MOM_U_SIDEDRAG( bi, bj, k,
     I        uFld, del2, h0FacZ,
     I        viscAh_Z, viscA4_Z,
     I        useHarmonicVisc, useBiharmonicVisc, useVariableVisc,
     O        gAdd,
     I        myThid )
         DO j=jMin,jMax
          DO i=iMin,iMax
           guDiss(i,j) = guDiss(i,j) + gAdd(i,j)
          ENDDO
         ENDDO
        ENDIF
C-    No-slip BCs impose a drag at bottom
        IF ( bottomDragTerms ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         CALL MOM_U_BOTDRAG_COEFF( bi, bj, k, .TRUE.,
     I                   uFld, vFld, kappaRU, KE,
     O                   cDrag,
     I                   myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cDrag(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         DO j=jMin,jMax
          DO i=iMin,iMax
            guDiss(i,j) = guDiss(i,j)
     &                  - cDrag(i,j)*uFld(i,j)
     &                  *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
          ENDDO
         ENDDO
         IF ( useDiagnostics ) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            botDragU(i,j,bi,bj) = botDragU(i,j,bi,bj)
     &                          - cDrag(i,j)*uFld(i,j)*rUnit2mass
           ENDDO
          ENDDO
         ENDIF
        ENDIF

#ifdef ALLOW_SHELFICE
        IF ( useShelfIce .AND. selectImplicitDrag.EQ.0 ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         CALL SHELFICE_U_DRAG_COEFF( bi, bj, k, .TRUE.,
     I                   uFld, vFld, kappaRU, KE,
     O                   cDrag,
     I                   myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cDrag(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         DO j=jMin,jMax
          DO i=iMin,iMax
            guDiss(i,j) = guDiss(i,j)
     &                  - cDrag(i,j)*uFld(i,j)
     &                  *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SHELFICE */

C-    endif momViscosity
      ENDIF

C--   Forcing term (moved to timestep.F)
c     IF (momForcing)
c    &  CALL EXTERNAL_FORCING_U(
c    I     iMin,iMax,jMin,jMax,bi,bj,k,
c    I     myTime,myThid)

C--   Metric terms for curvilinear grid systems
      diagFlag = .FALSE.
#ifdef MOM_USE_OLD_DEEP_VERT_ADV
      IF ( useNHMTerms ) THEN
#else
      IF ( useNHMTerms .AND. .NOT.deepAtmosphere ) THEN
#endif
C      o Non-Hydrostatic/vertical (spherical) metric terms
       CALL MOM_U_METRIC_NH( bi,bj,k,uFld,wVel,vMT,myThid )
       diagFlag = .TRUE.
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mtNHFacU*vMT(i,j)
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics .AND.
     &     ( selectMetricTerms.EQ.0 .OR. selectMetricTerms.EQ.3 ) ) THEN
        CALL DIAGNOSTICS_FILL( vMT, 'Um_Metr ', k,1,2, bi,bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF
      IF ( selectMetricTerms.GE.1 .AND. selectMetricTerms.NE.3 ) THEN
       IF ( usingSphericalPolarGrid ) THEN
C      o Spherical polar grid metric terms
        CALL MOM_U_METRIC_SPHERE( bi,bj,k,uFld,vFld,gAdd,myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mtFacU*gAdd(i,j)
         ENDDO
        ENDDO
       ENDIF
       IF ( usingCylindricalGrid ) THEN
C      o Cylindrical grid metric terms
        CALL MOM_U_METRIC_CYLINDER( bi,bj,k,uFld,vFld,gAdd,myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+mtFacU*gAdd(i,j)
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        IF ( diagFlag ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gAdd(i,j) = gAdd(i,j) + vMT(i,j)
          ENDDO
         ENDDO
        ENDIF
        CALL DIAGNOSTICS_FILL( gAdd, 'Um_Metr ', k,1,2, bi,bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---- Meridional momentum equation starts here

      IF (momAdvection) THEN

C--   For Deep-Model (deepAtmosphere=T), no need for NHMTerm w*v/r since
C     vertical advective flux (in MOM_V_ADV_WV) is changed to W*(v*deepFacC)
C     with rAdvDeepFac factor in flux divergence

#ifdef MOM_BOUNDARY_CONSERVE
        CALL MOM_V_ADV_UV( bi,bj,k,uTrans,vBnd(1-OLx,1-OLy,k,bi,bj),
     O                     fZon,myThid )
        CALL MOM_V_ADV_VV( bi,bj,k,vTrans,vBnd(1-OLx,1-OLy,k,bi,bj),
     O                     fMer,myThid )
        CALL MOM_V_ADV_WV( bi, bj, k+1, deepFacAdv,
     I                     vBnd, wVel, rTransV,
     O                     fVerVkp, myThid )
#else /* MOM_BOUNDARY_CONSERVE */
C---  Calculate mean fluxes (advection)   between cells for meridional flow.
C     Mean flow component of zonal flux -> fZon
        CALL MOM_V_ADV_UV( bi,bj,k,uTrans,vFld,fZon,myThid )

C--   Meridional flux (fMer is at north face of "v" cell)
C     Mean flow component of meridional flux -> fMer
        CALL MOM_V_ADV_VV( bi,bj,k,vTrans,vFld,fMer,myThid )

C--   Vertical flux (fVer is at upper face of "v" cell)
C     Mean flow component of vertical flux (at k+1) -> fVerV
        CALL MOM_V_ADV_WV( bi, bj, k+1, deepFacAdv,
     I                     vVel, wVel, rTransV,
     O                     fVerVkp, myThid )
#endif /* MOM_BOUNDARY_CONSERVE */

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
        rAdvDeepFac = wDvdrFac*rkSign
#ifndef MOM_USE_OLD_DEEP_VERT_ADV
        IF ( useNHMTerms ) rAdvDeepFac = rAdvDeepFac*recip_deepFacC(k)
#endif
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) =
#ifdef OLD_UV_GEOM
     &     -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)/
     &      ( 0.5 _d 0*(_rA(i,j,bi,bj)+_rA(i,j-1,bi,bj)) )
#else
     &     -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &     *recip_rAs(i,j,bi,bj)*recip_deepFac2C(k)*recip_rhoFacC(k)
#endif
     &     *( ( fZon(i+1,j)  - fZon(i,j  )  )*uDvdxFac
     &       +( fMer(i,  j)  - fMer(i,j-1)  )*vDvdyFac
     &       +( fVerVkp(i,j) - fVerVkm(i,j) )*rAdvDeepFac
     &     )
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL( fZon,  'ADVx_Vm ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL( fMer,  'ADVy_Vm ',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(fVerVkm,'ADVrE_Vm',k,1,2,bi,bj,myThid)
        ENDIF
#endif

#ifdef NONLIN_FRSURF
C-- account for 3.D divergence of the flow in rStar coordinate:
# ifndef DISABLE_RSTAR_CODE
        IF ( select_rStar.GT.0 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     - (rStarExpS(i,j,bi,bj) - 1. _d 0)/deltaTFreeSurf
     &       *vVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        IF ( select_rStar.LT.0 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     - rStarDhSDt(i,j,bi,bj)*vVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
# endif /* DISABLE_RSTAR_CODE */
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_ADDFLUID
        IF ( selectAddFluid.GE.1 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     + vVel(i,j,k,bi,bj)*mass2rUnit*0.5 _d 0
     &      *( addMass(i,j-1,k,bi,bj) + addMass(i,j,k,bi,bj) )
     &      *_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)*recip_rhoFacC(k)
     &      * recip_rAs(i,j,bi,bj)*recip_deepFac2C(k)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_ADDFLUID */

      ELSE
C-    if momAdvection / else
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gV(i,j,k,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C-    endif momAdvection.
      ENDIF

      IF (momViscosity) THEN
C---  Calculate eddy fluxes (dissipation) between cells for meridional flow.
C     Bi-harmonic term del^2 V -> del2
        IF ( useBiharmonicVisc )
     &  CALL MOM_V_DEL2V( bi, bj, k, vFld, hFacZ, h0FacZ,
     O                    del2, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE del2(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif

C     Laplacian and bi-harmonic terms, Zonal  Fluxes -> fZon
        CALL MOM_V_XVISCFLUX( bi,bj,k,vFld,del2,hFacZ,fZon,
     I                        viscAh_Z,viscA4_Z,myThid )

C     Laplacian and bi-harmonic termis, Merid Fluxes -> fMer
        CALL MOM_V_YVISCFLUX( bi,bj,k,vFld,del2,fMer,
     I                        viscAh_D,viscA4_D,myThid )

C     Eddy component of vertical flux (interior component only) -> fVrUp & fVrDw
       IF (.NOT.implicitViscosity) THEN
        CALL MOM_V_RVISCFLUX( bi,bj, k, vVel,KappaRV,fVrUp,myThid )
        CALL MOM_V_RVISCFLUX( bi,bj,k+1,vVel,KappaRV,fVrDw,myThid )
       ENDIF

C--   Tendency is minus divergence of the fluxes + coriolis + pressure term
C     anelastic: hor.visc.fluxes are not scaled by rhoFac (by vert.visc.flx is)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gvDiss(i,j) =
#ifdef OLD_UV_GEOM
     &     -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)/
     &      ( 0.5 _d 0*(_rA(i,j,bi,bj)+_rA(i,j-1,bi,bj)) )
#else
     &     -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &      *recip_rAs(i,j,bi,bj)*recip_deepFac2C(k)
#endif
     &     *( ( fZon(i+1,j)  - fZon(i,j  ) )*AhDvdxFac
     &       +( fMer(i,  j)  - fMer(i,j-1) )*AhDvdyFac
     &       +( fVrDw(i,j)   - fVrUp(i,j) )*rkSign*ArDvdrFac
     &                                     *recip_rhoFacC(k)
     &     )
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(fZon, 'VISCx_Vm',k,1,2,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(fMer, 'VISCy_Vm',k,1,2,bi,bj,myThid)
          IF (.NOT.implicitViscosity)
     &    CALL DIAGNOSTICS_FILL(fVrUp,'VISrE_Vm',k,1,2,bi,bj,myThid)
        ENDIF
#endif

C-- No-slip and drag BCs appear as body forces in cell abutting topography
        IF (no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
         CALL MOM_V_SIDEDRAG( bi, bj, k,
     I        vFld, del2, h0FacZ,
     I        viscAh_Z, viscA4_Z,
     I        useHarmonicVisc, useBiharmonicVisc, useVariableVisc,
     O        gAdd,
     I        myThid )
         DO j=jMin,jMax
          DO i=iMin,iMax
           gvDiss(i,j) = gvDiss(i,j) + gAdd(i,j)
          ENDDO
         ENDDO
        ENDIF
C-    No-slip BCs impose a drag at bottom
        IF ( bottomDragTerms ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         CALL MOM_V_BOTDRAG_COEFF( bi, bj, k, .TRUE.,
     I                   uFld, vFld, kappaRV, KE,
     O                   cDrag,
     I                   myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cDrag(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         DO j=jMin,jMax
          DO i=iMin,iMax
            gvDiss(i,j) = gvDiss(i,j)
     &                  - cDrag(i,j)*vFld(i,j)
     &                  *_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
          ENDDO
         ENDDO
         IF ( useDiagnostics ) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
            botDragV(i,j,bi,bj) = botDragV(i,j,bi,bj)
     &                          - cDrag(i,j)*vFld(i,j)*rUnit2mass
           ENDDO
          ENDDO
         ENDIF
        ENDIF

#ifdef ALLOW_SHELFICE
        IF ( useShelfIce .AND. selectImplicitDrag.EQ.0 ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         CALL SHELFICE_V_DRAG_COEFF( bi, bj, k, .TRUE.,
     I                   uFld, vFld, kappaRV, KE,
     O                   cDrag,
     I                   myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cDrag(:,:) = comlev1_bibj_k, key = kkey, byte = isbyte
#endif
         DO j=jMin,jMax
          DO i=iMin,iMax
            gvDiss(i,j) = gvDiss(i,j)
     &                  - cDrag(i,j)*vFld(i,j)
     &                  *_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SHELFICE */

C-    endif momViscosity
      ENDIF

C--   Forcing term (moved to timestep.F)
c     IF (momForcing)
c    & CALL EXTERNAL_FORCING_V(
c    I     iMin,iMax,jMin,jMax,bi,bj,k,
c    I     myTime,myThid)

C--   Metric terms for curvilinear grid systems
      diagFlag = .FALSE.
#ifdef MOM_USE_OLD_DEEP_VERT_ADV
      IF ( useNHMTerms ) THEN
#else
      IF ( useNHMTerms .AND. .NOT.deepAtmosphere ) THEN
#endif
C      o Non-Hydrostatic/vertical (spherical) metric terms
       CALL MOM_V_METRIC_NH( bi,bj,k,vFld,wVel,vMT,myThid )
       diagFlag = .TRUE.
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mtNHFacV*vMT(i,j)
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics .AND. selectMetricTerms.EQ.0 ) THEN
        CALL DIAGNOSTICS_FILL( vMT, 'Vm_Metr ', k,1,2, bi,bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF
      IF ( selectMetricTerms.GE.1 ) THEN
       IF ( usingSphericalPolarGrid ) THEN
C      o Spherical polar grid metric terms
        CALL MOM_V_METRIC_SPHERE( bi,bj,k,uFld,gAdd,myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mtFacV*gAdd(i,j)
         ENDDO
        ENDDO
       ENDIF
       IF ( usingCylindricalGrid ) THEN
C      o Cylindrical grid metric terms
        CALL MOM_V_METRIC_CYLINDER( bi,bj,k,uFld,vFld,gAdd,myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mtFacV*gAdd(i,j)
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        IF ( diagFlag ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gAdd(i,j) = gAdd(i,j) + vMT(i,j)
          ENDDO
         ENDDO
        ENDIF
        CALL DIAGNOSTICS_FILL( gAdd, 'Vm_Metr ', k,1,2, bi,bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coriolis term (call to CD_CODE_SCHEME has been moved to timestep.F)
      IF ( .NOT.useCDscheme ) THEN
#if ( defined ALLOW_GGL90 && defined ALLOW_GGL90_LANGMUIR )
       IF ( useLANGMUIR ) THEN
        CALL GGL90_ADD_STOKESDRIFT(
     O                 uRes, vRes,
     I                 uFld, vFld, k, bi, bj, myThid )
        CALL MOM_U_CORIOLIS( bi,bj,k,vRes,uCf,myThid )
        CALL MOM_V_CORIOLIS( bi,bj,k,uRes,vCf,myThid )
       ELSE
#endif
        CALL MOM_U_CORIOLIS( bi,bj,k,vFld,uCf,myThid )
        CALL MOM_V_CORIOLIS( bi,bj,k,uFld,vCf,myThid )
#if ( defined ALLOW_GGL90 && defined ALLOW_GGL90_LANGMUIR )
       ENDIF
#endif
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) + fuFac*uCf(i,j)
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) + fvFac*vCf(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL( uCf,'Um_Cori ',k,1,2,bi,bj,myThid )
          CALL DIAGNOSTICS_FILL( vCf,'Vm_Cori ',k,1,2,bi,bj,myThid )
        ENDIF
#endif
      ENDIF

C--   3.D Coriolis term (horizontal momentum, Eastward component: -fprime*w)
      IF ( select3dCoriScheme.GE.1 ) THEN
        CALL MOM_U_CORIOLIS_NH( bi,bj,k,wVel,uCf,myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) + fuFac*uCf(i,j)
         ENDDO
        ENDDO
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
C-     presently, non zero angleSinC array only supported with Curvilinear-Grid
        CALL MOM_V_CORIOLIS_NH( bi,bj,k,wVel,vCf,myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) + fvFac*vCf(i,j)
         ENDDO
        ENDDO
       ENDIF
      ENDIF

C--   Set du/dt & dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
        guDiss(i,j)     = guDiss(i,j)    *_maskW(i,j,k,bi,bj)
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
        gvDiss(i,j)     = gvDiss(i,j)    *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(KE,    'momKE   ',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gU(1-OLx,1-OLy,k,bi,bj),
     &                               'Um_Advec',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gV(1-OLx,1-OLy,k,bi,bj),
     &                               'Vm_Advec',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
