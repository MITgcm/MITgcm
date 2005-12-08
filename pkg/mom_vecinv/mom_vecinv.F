C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vecinv.F,v 1.55 2005/12/08 15:44:34 heimbach Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VECINV( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kUp,kDown,
     I        KappaRU, KappaRV,
     U        fVerU, fVerV,
     O        guDiss, gvDiss,
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
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif

C     == Routine arguments ==
C     fVerU  :: Flux of momentum in the vertical direction, out of the upper
C     fVerV  :: face of a cell K ( flux into the cell above ).
C     guDiss :: dissipation tendency (all explicit terms), u component
C     gvDiss :: dissipation tendency (all explicit terms), v component
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     kUp, kDown                     - Index for upper and lower layers.
C     myThid :: my Thread Id number
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL fVerU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL guDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gvDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RL      vF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vrF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      uCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL      mT (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dStar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zStar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     i,j,k  :: Loop counters
      INTEGER i,j,k
C     xxxFac - On-off tracer parameters used for switching terms off.
      _RL  ArDudrFac
c     _RL  mtFacU
      _RL  ArDvdrFac
c     _RL  mtFacV
      _RL  sideMaskFac
      LOGICAL bottomDragTerms
      LOGICAL writeDiag
      LOGICAL harmonic,biharmonic,useVariableViscosity

#ifdef ALLOW_MNC
      INTEGER offsets(9)
      CHARACTER*(1) pf
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kDown part of fverU/V is set in this subroutine
C--   the kUp is still required
C--   In the case of mom_fluxform Kup is set as well
C--   (at least in part)
      fVerU(1,1,kUp) = fVerU(1,1,kUp)
      fVerV(1,1,kUp) = fVerV(1,1,kUp)
#endif

      writeDiag = DIFFERENT_MULTIPLE(diagFreq, myTime, deltaTClock)

#ifdef ALLOW_MNC
      IF (useMNC .AND. snapshot_mnc .AND. writeDiag) THEN
        IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
          pf(1:1) = 'D'
        ELSE
          pf(1:1) = 'R'
        ENDIF
        IF ((bi .EQ. 1).AND.(bj .EQ. 1).AND.(k .EQ. 1)) THEN
          CALL MNC_CW_SET_UDIM('mom_vi', -1, myThid)
          CALL MNC_CW_RL_W_S('D','mom_vi',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('mom_vi', 0, myThid)
          CALL MNC_CW_I_W_S('I','mom_vi',0,0,'iter',myIter,myThid)
        ENDIF
        DO i = 1,9
          offsets(i) = 0
        ENDDO
        offsets(3) = k
C       write(*,*) 'offsets = ',(offsets(i),i=1,9)
      ENDIF
#endif /*  ALLOW_MNC  */

C     Initialise intermediate terms
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        vF(i,j)    = 0.
        vrF(i,j)   = 0.
        uCf(i,j)   = 0.
        vCf(i,j)   = 0.
c       mT(i,j)    = 0.
        del2u(i,j) = 0.
        del2v(i,j) = 0.
        dStar(i,j) = 0.
        zStar(i,j) = 0.
        guDiss(i,j)= 0.
        gvDiss(i,j)= 0.
        vort3(i,j) = 0.
        omega3(i,j)= 0.
        KE(i,j)    = 0.
        viscAh_Z(i,j) = 0.
        viscAh_D(i,j) = 0.
        viscA4_Z(i,j) = 0.
        viscA4_D(i,j) = 0.

#ifdef ALLOW_AUTODIFF_TAMC
        strain(i,j)  = 0. _d 0
        tension(i,j) = 0. _d 0
        hFacZ(i,j)   = 0. _d 0
#endif
       ENDDO
      ENDDO

C--   Term by term tracer parmeters
C     o U momentum equation
      ArDudrFac    = vfFacMom*1.
c     mTFacU       = mtFacMom*1.
C     o V momentum equation
      ArDvdrFac    = vfFacMom*1.
c     mTFacV       = mtFacMom*1.

C note: using standard stencil (no mask) results in under-estimating
C       vorticity at a no-slip boundary by a factor of 2 = sideDragFactor
      IF ( no_slip_sides ) THEN
        sideMaskFac = sideDragFactor
      ELSE
        sideMaskFac = 0. _d 0
      ENDIF

      IF (     no_slip_bottom
     &    .OR. bottomDragQuadratic.NE.0.
     &    .OR. bottomDragLinear.NE.0.) THEN
       bottomDragTerms=.TRUE.
      ELSE
       bottomDragTerms=.FALSE.
      ENDIF

C--   Calculate open water fraction at vorticity points
      CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

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

      CALL MOM_CALC_KE(bi,bj,k,selectKEscheme,uFld,vFld,KE,myThid)

      CALL MOM_CALC_RELVORT3(bi,bj,k,uFld,vFld,hFacZ,vort3,myThid)

      IF (momViscosity) THEN
C--    For viscous term, compute horizontal divergence, tension & strain 
C      and mask relative vorticity (free-slip case):

       CALL MOM_CALC_HDIV(bi,bj,k,2,uFld,vFld,hDiv,myThid)

       CALL MOM_CALC_TENSION(bi,bj,k,uFld,vFld,tension,myThid)

       CALL MOM_CALC_STRAIN(bi,bj,k,uFld,vFld,hFacZ,strain,myThid)

C-     account for no-slip / free-slip BC:
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          IF ( hFacZ(i,j).EQ.0. ) THEN
            vort3(i,j)  = sideMaskFac*vort3(i,j)
            strain(i,j) = sideMaskFac*strain(i,j)
          ENDIF
        ENDDO
       ENDDO

C--    Calculate Viscosities
       CALL MOM_CALC_VISC(
     I        bi,bj,k,
     O        viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     O        harmonic,biharmonic,useVariableViscosity,
     I        hDiv,vort3,tension,strain,KE,hfacZ,
     I        myThid)

C      Calculate del^2 u and del^2 v for bi-harmonic term
       IF (biharmonic) THEN
         CALL MOM_VI_DEL2UV(bi,bj,k,hDiv,vort3,hFacZ,
     O                      del2u,del2v,
     &                      myThid)
         CALL MOM_CALC_HDIV(bi,bj,k,2,del2u,del2v,dStar,myThid)
         CALL MOM_CALC_RELVORT3(bi,bj,k,
     &                          del2u,del2v,hFacZ,zStar,myThid)
       ENDIF

C-    Strain diagnostics:
       IF ( writeDiag ) THEN
        IF (snapshot_mdsio) THEN
          CALL WRITE_LOCAL_RL('Ds','I10',1,strain,bi,bj,k,myIter,myThid)
        ENDIF
#ifdef ALLOW_MNC
        IF (useMNC .AND. snapshot_mnc) THEN
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Ds',strain,
     &          offsets, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(strain, 'Strain  ',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C---   Calculate dissipation terms for U and V equations

C      in terms of tension and strain
       IF (useStrainTensionVisc) THEN
C        mask strain as if free-slip since side-drag is computed separately
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            IF ( hFacZ(i,j).EQ.0. ) strain(i,j) = 0. _d 0
          ENDDO
         ENDDO
         CALL MOM_HDISSIP(bi,bj,k,hDiv,vort3,tension,strain,KE,
     I                    hFacZ,
     I                    viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     I                    harmonic,biharmonic,useVariableViscosity,
     O                    guDiss,gvDiss,
     I                    myThid)
       ELSE
C      in terms of vorticity and divergence
         CALL MOM_VI_HDISSIP(bi,bj,k,hDiv,vort3,tension,strain,KE,
     I                       hFacZ,dStar,zStar,
     I                       viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     I                       harmonic,biharmonic,useVariableViscosity,
     O                       guDiss,gvDiss,
     &                       myThid)         
       ENDIF
C--   if (momViscosity) end of block.
      ENDIF

C-    Return to standard hfacZ (min-4) and mask vort3 accordingly:
c     CALL MOM_VI_MASK_VORT3(bi,bj,k,hFacZ,r_hFacZ,vort3,myThid)

C---  Other dissipation terms in Zonal momentum equation

C--   Vertical flux (fVer is at upper face of "u" cell)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity) THEN
       CALL MOM_U_RVISCFLUX(bi,bj,k+1,uVel,KappaRU,vrF,myThid)

C     Combine fluxes
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerU(i,j,kDown) = ArDudrFac*vrF(i,j)
        ENDDO
       ENDDO

C--   Tendency is minus divergence of the fluxes
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
         guDiss(i,j) = guDiss(i,j)
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rAw(i,j,bi,bj)
     &  *(
     &    fVerU(i,j,kDown) - fVerU(i,j,kUp)
     &   )*rkSign
        ENDDO
       ENDDO
      ENDIF

C-- No-slip and drag BCs appear as body forces in cell abutting topography 
      IF (momViscosity.AND.no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
       CALL MOM_U_SIDEDRAG(
     I        bi,bj,k,
     I        uFld, del2u, hFacZ,
     I        viscAh_Z,viscA4_Z,
     I        harmonic,biharmonic,useVariableViscosity,
     O        vF,
     I        myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         guDiss(i,j) = guDiss(i,j)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF
C-    No-slip BCs impose a drag at bottom
      IF (momViscosity.AND.bottomDragTerms) THEN
       CALL MOM_U_BOTTOMDRAG(bi,bj,k,uFld,KE,KappaRU,vF,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         guDiss(i,j) = guDiss(i,j)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF

C---  Other dissipation terms in Meridional momentum equation

C--   Vertical flux (fVer is at upper face of "v" cell)

C     Eddy component of vertical flux (interior component only) -> vrF
      IF (momViscosity.AND..NOT.implicitViscosity) THEN
       CALL MOM_V_RVISCFLUX(bi,bj,k+1,vVel,KappaRV,vrF,myThid)

C     Combine fluxes -> fVerV
       DO j=jMin,jMax
        DO i=iMin,iMax
         fVerV(i,j,kDown) = ArDvdrFac*vrF(i,j)
        ENDDO
       ENDDO

C--   Tendency is minus divergence of the fluxes
       DO j=jMin,jMax
        DO i=iMin,iMax
         gvDiss(i,j) = gvDiss(i,j)
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rAs(i,j,bi,bj)
     &  *(
     &    fVerV(i,j,kDown) - fVerV(i,j,kUp)
     &   )*rkSign
        ENDDO
       ENDDO
      ENDIF

C-- No-slip and drag BCs appear as body forces in cell abutting topography 
      IF (momViscosity.AND.no_slip_sides) THEN
C-     No-slip BCs impose a drag at walls...
       CALL MOM_V_SIDEDRAG(
     I        bi,bj,k,
     I        vFld, del2v, hFacZ,
     I        viscAh_Z,viscA4_Z,
     I        harmonic,biharmonic,useVariableViscosity,
     O        vF,
     I        myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gvDiss(i,j) = gvDiss(i,j)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF
C-    No-slip BCs impose a drag at bottom
      IF (momViscosity.AND.bottomDragTerms) THEN
       CALL MOM_V_BOTTOMDRAG(bi,bj,k,vFld,KE,KappaRV,vF,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gvDiss(i,j) = gvDiss(i,j)+vF(i,j)
        ENDDO
       ENDDO
      ENDIF

C-    Vorticity diagnostics:
      IF ( writeDiag ) THEN
        IF (snapshot_mdsio) THEN
          CALL WRITE_LOCAL_RL('Z3','I10',1,vort3, bi,bj,k,myIter,myThid)
        ENDIF
#ifdef ALLOW_MNC
        IF (useMNC .AND. snapshot_mnc) THEN
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Z3',vort3,
     &          offsets, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */
      ENDIF
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(vort3,  'momVort3',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  Prepare for Advection & Coriolis terms:
C-    Mask relative vorticity and calculate absolute vorticity
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
         IF ( hFacZ(i,j).EQ.0. ) vort3(i,j) = 0.
       ENDDO
      ENDDO
      IF (useAbsVorticity)
     &  CALL MOM_CALC_ABSVORT3(bi,bj,k,vort3,omega3,myThid)

C--   Horizontal Coriolis terms
c     IF (useCoriolis .AND. .NOT.useCDscheme
c    &    .AND. .NOT. useAbsVorticity) THEN
C- jmc: change it to keep the Coriolis terms when useAbsVorticity=T & momAdvection=F
      IF ( useCoriolis .AND.
     &     .NOT.( useCDscheme .OR. useAbsVorticity.AND.momAdvection )
     &   ) THEN
       IF (useAbsVorticity) THEN
        CALL MOM_VI_U_CORIOLIS(bi,bj,K,vFld,omega3,hFacZ,r_hFacZ,
     &                         uCf,myThid)
        CALL MOM_VI_V_CORIOLIS(bi,bj,K,uFld,omega3,hFacZ,r_hFacZ,
     &                         vCf,myThid)
       ELSE
        CALL MOM_VI_CORIOLIS(bi,bj,k,uFld,vFld,hFacZ,r_hFacZ,
     &                       uCf,vCf,myThid)
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = uCf(i,j)
         gV(i,j,k,bi,bj) = vCf(i,j)
        ENDDO
       ENDDO

       IF ( writeDiag ) THEN
         IF (snapshot_mdsio) THEN
           CALL WRITE_LOCAL_RL('fV','I10',1,uCf,bi,bj,k,myIter,myThid)
           CALL WRITE_LOCAL_RL('fU','I10',1,vCf,bi,bj,k,myIter,myThid)
         ENDIF
#ifdef ALLOW_MNC
         IF (useMNC .AND. snapshot_mnc) THEN
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'fV', uCf,
     &          offsets, myThid)
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'fU', vCf,
     &          offsets, myThid)
         ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(uCf,'Um_Cori ',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(vCf,'Vm_Cori ',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = 0. _d 0
         gV(i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

      IF (momAdvection) THEN
C--   Horizontal advection of relative (or absolute) vorticity
       IF (highOrderVorticity.AND.useAbsVorticity) THEN
        CALL MOM_VI_U_CORIOLIS_C4(bi,bj,k,vFld,omega3,r_hFacZ,
     &                         uCf,myThid)
       ELSEIF (highOrderVorticity) THEN
        CALL MOM_VI_U_CORIOLIS_C4(bi,bj,k,vFld,vort3, r_hFacZ,
     &                         uCf,myThid)
       ELSEIF (useAbsVorticity) THEN
        CALL MOM_VI_U_CORIOLIS(bi,bj,K,vFld,omega3,hFacZ,r_hFacZ,
     &                         uCf,myThid)
       ELSE
        CALL MOM_VI_U_CORIOLIS(bi,bj,k,vFld,vort3, hFacZ,r_hFacZ,
     &                         uCf,myThid)
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
        ENDDO
       ENDDO
       IF (highOrderVorticity.AND.useAbsVorticity) THEN
        CALL MOM_VI_V_CORIOLIS_C4(bi,bj,K,uFld,omega3,r_hFacZ,
     &                         vCf,myThid)
       ELSEIF (highOrderVorticity) THEN
        CALL MOM_VI_V_CORIOLIS_C4(bi,bj,K,uFld,vort3, r_hFacZ,
     &                         vCf,myThid)
       ELSEIF (useAbsVorticity) THEN
        CALL MOM_VI_V_CORIOLIS(bi,bj,K,uFld,omega3,hFacZ,r_hFacZ,
     &                         vCf,myThid)
       ELSE
        CALL MOM_VI_V_CORIOLIS(bi,bj,k,uFld,vort3, hFacZ,r_hFacZ,
     &                         vCf,myThid)
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO

       IF ( writeDiag ) THEN
         IF (snapshot_mdsio) THEN
           CALL WRITE_LOCAL_RL('zV','I10',1,uCf,bi,bj,k,myIter,myThid)
           CALL WRITE_LOCAL_RL('zU','I10',1,vCf,bi,bj,k,myIter,myThid)
         ENDIF
#ifdef ALLOW_MNC
         IF (useMNC .AND. snapshot_mnc) THEN
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'zV', uCf,
     &          offsets, myThid)
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'zU', vCf,
     &          offsets, myThid)
         ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF

#ifdef ALLOW_TIMEAVE
       IF (taveFreq.GT.0.) THEN
         CALL TIMEAVE_CUMUL_1K1T(uZetatave,vCf,deltaTClock,
     &                           Nr, k, bi, bj, myThid)
         CALL TIMEAVE_CUMUL_1K1T(vZetatave,uCf,deltaTClock,
     &                           Nr, k, bi, bj, myThid)
       ENDIF
#endif /* ALLOW_TIMEAVE */
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(uCf,'Um_AdvZ3',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(vCf,'Vm_AdvZ3',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

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
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(uCf,'Um_AdvRe',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(vCf,'Vm_AdvRe',k,1,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
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
         IF (snapshot_mdsio) THEN
           CALL WRITE_LOCAL_RL('KEx','I10',1,uCf,bi,bj,k,myIter,myThid)
           CALL WRITE_LOCAL_RL('KEy','I10',1,vCf,bi,bj,k,myIter,myThid)
         ENDIF
#ifdef ALLOW_MNC
         IF (useMNC .AND. snapshot_mnc) THEN
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'KEx', uCf,
     &          offsets, myThid)
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'KEy', vCf,
     &          offsets, myThid)
         ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF

C--   end if momAdvection
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
c      CALL MOM_V_METRIC_NH(bi,bj,k,vFld,wVel,mT,myThid)
c      DO j=jMin,jMax
c       DO i=iMin,iMax
c        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+mTFacV*mT(i,j)
c       ENDDO
c      ENDDO
c     ENDIF

C--   Set du/dt & dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB
     &   .AND. k.EQ.4 .AND. myIter.EQ.nIter0
     &   .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &   .AND. useCubedSphereExchange ) THEN
        CALL DEBUG_CS_CORNER_UV( ' uDiss,vDiss from MOM_VECINV',
     &             guDiss,gvDiss, k, standardMessageUnit,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

      IF ( writeDiag ) THEN
        IF (snapshot_mdsio) THEN
         CALL WRITE_LOCAL_RL('W3','I10',1,omega3, bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('KE','I10',1,KE,     bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('D', 'I10',1,hDiv,   bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('Dt','I10',1,tension,bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('Du','I10',1,guDiss, bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('Dv','I10',1,gvDiss, bi,bj,k,myIter,myThid)
        ENDIF
#ifdef ALLOW_MNC
        IF (useMNC .AND. snapshot_mnc) THEN
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'W3',omega3,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'KE',KE,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'D', hDiv,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Dt',tension,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Du',guDiss,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Dv',gvDiss,
     &          offsets, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(KE,     'momKE   ',k,1,2,bi,bj,myThid)
       IF (momViscosity) THEN
        CALL DIAGNOSTICS_FILL(hDiv,   'momHDiv ',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(tension,'Tension ',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(guDiss, 'Um_Diss ',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gvDiss, 'Vm_Diss ',k,1,2,bi,bj,myThid)
       ENDIF
        CALL DIAGNOSTICS_FILL(gU(1-Olx,1-Oly,k,bi,bj),
     &                                'Um_Advec',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gV(1-Olx,1-Oly,k,bi,bj),
     &                                'Vm_Advec',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MOM_VECINV */

      RETURN
      END
