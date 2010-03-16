C $Header: /u/gcmpack/MITgcm/model/src/calc_phi_hyd.F,v 1.40 2010/03/16 00:08:27 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_PHI_HYD
C     !INTERFACE:
      SUBROUTINE CALC_PHI_HYD(
     I                         bi, bj, iMin, iMax, jMin, jMax, k,
     I                         tFld, sFld,
     U                         phiHydF,
     O                         phiHydC, dPhiHydX, dPhiHydY,
     I                         myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_PHI_HYD                                  |
C     | o Integrate the hydrostatic relation to find the Hydros. |
C     *==========================================================*
C     |    Potential (ocean: Pressure/rho ; atmos = geopotential)
C     | On entry:
C     |   tFld,sFld     are the current thermodynamics quantities
C     |                 (unchanged on exit)
C     |   phiHydF(i,j) is the hydrostatic Potential anomaly
C     |                at middle between tracer points k-1,k
C     | On exit:
C     |   phiHydC(i,j) is the hydrostatic Potential anomaly
C     |                at cell centers (tracer points), level k
C     |   phiHydF(i,j) is the hydrostatic Potential anomaly
C     |                at middle between tracer points k,k+1
C     |   dPhiHydX,Y   hydrostatic Potential gradient (X&Y dir)
C     |                at cell centers (tracer points), level k
C     | integr_GeoPot allows to select one integration method
C     |    1= Finite volume form ; else= Finite difference form
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */
#include "SURFACE.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj, k  :: tile and level indices
C     iMin,iMax,jMin,jMax :: computational domain
C     tFld       :: potential temperature
C     sFld       :: salinity
C     phiHydF    :: hydrostatic potential anomaly at middle between
C                   2 centers (entry: Interf_k ; output: Interf_k+1)
C     phiHydC    :: hydrostatic potential anomaly at cell center
C     dPhiHydX,Y :: gradient (X & Y dir.) of hydrostatic potential anom.
C     myTime     :: current time
C     myIter     :: current iteration number
C     myThid     :: thread number for this instance of the routine.
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL phiHydF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiHydC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL zero, one, half
      _RL alphaRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dRlocM,dRlocP, ddRloc, locAlpha
      _RL ddPIm, ddPIp, rec_dRm, rec_dRp
      _RL surfPhiFac
      PARAMETER ( zero= 0. _d 0 , one= 1. _d 0 , half= .5 _d 0 )
      LOGICAL useDiagPhiRlow, addSurfPhiAnom
CEOP
      useDiagPhiRlow = .TRUE.
      addSurfPhiAnom = select_rStar.EQ.0 .AND. nonlinFreeSurf.GT.3
      surfPhiFac = 0.
      IF (addSurfPhiAnom) surfPhiFac = 1.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  Atmosphere:
C integr_GeoPot => select one option for the integration of the Geopotential:
C   = 0 : Energy Conserving Form, accurate with Topo full cell;
C   = 1 : Finite Volume Form, with Part-Cell, linear in P by Half level;
C   =2,3: Finite Difference Form, with Part-Cell,
C         linear in P between 2 Tracer levels.
C       can handle both cases: Tracer lev at the middle of InterFace_W
C                          and InterFace_W at the middle of Tracer lev;
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1

          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1

          act3 = myThid - 1
          max3 = nTx*nTy

          act4 = ikey_dynamics - 1

          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Initialize phiHydF to zero :
C     note: atmospheric_loading or Phi_topo anomaly are incorporated
C           later in S/R calc_grad_phi_hyd
      IF (k.EQ.1) THEN
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           phiHydF(i,j) = 0.
         ENDDO
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( buoyancyRelation .EQ. 'OCEANIC' ) THEN
C       This is the hydrostatic pressure calculation for the Ocean
C       which uses the FIND_RHO() routine to calculate density
C       before integrating g*rho over the current layer/interface
#ifdef ALLOW_AUTODIFF_TAMC
CADJ GENERAL
#endif /* ALLOW_AUTODIFF_TAMC */

        IF ( implicitIntGravWave .OR. myIter.LT.0 ) THEN
C---    Calculate density
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (ikey-1)*Nr + k
CADJ STORE tFld (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte,
CADJ &     kind = isbyte
CADJ STORE sFld (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              tFld(1-OLx,1-OLy,k,bi,bj),
     I              sFld(1-OLx,1-OLy,k,bi,bj),
     O              alphaRho,
     I              k, bi, bj, myThid )
        ELSE
          DO j=jMin,jMax
           DO i=iMin,iMax
             alphaRho(i,j) = rhoInSitu(i,j,k,bi,bj)
           ENDDO
          ENDDO
        ENDIF

#ifdef ALLOW_SHELFICE
C     mask rho, so that there is no contribution of phiHyd from
C     overlying shelfice (whose density we do not know)
        IF ( useShelfIce .AND. useDOWN_SLOPE ) THEN
C- note: does not work for down_slope pkg which needs rho below the bottom.
C    setting rho=0 above the ice-shelf base is enough (and works in both cases)
C    but might be slower (--> keep original masking if not using down_slope pkg)
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( k.LT.kSurfC(i,j,bi,bj) ) alphaRho(i,j) = 0. _d 0
          ENDDO
         ENDDO
        ELSEIF ( useShelfIce ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           alphaRho(i,j) = alphaRho(i,j)*maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_MOM_COMMON
C Quasi-hydrostatic terms are added in as if they modify the buoyancy
        IF (quasiHydrostatic) THEN
         CALL MOM_QUASIHYDROSTATIC(bi,bj,k,uVel,vVel,alphaRho,myThid)
        ENDIF
#endif /* ALLOW_MOM_COMMON */

#ifdef NONLIN_FRSURF
        IF (k.EQ.1 .AND. addSurfPhiAnom) THEN
          DO j=jMin,jMax
            DO i=iMin,iMax
              phiHydF(i,j) = surfPhiFac*etaH(i,j,bi,bj)
     &                      *gravity*alphaRho(i,j)*recip_rhoConst
            ENDDO
          ENDDO
        ENDIF
#endif /* NONLIN_FRSURF */

C----  Hydrostatic pressure at cell centers

       IF (integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form

         DO j=jMin,jMax
          DO i=iMin,iMax

C---------- This discretization is the "finite volume" form
C           which has not been used to date since it does not
C           conserve KE+PE exactly even though it is more natural
C
           phiHydC(i,j)=phiHydF(i,j)
     &       + half*drF(k)*gravity*alphaRho(i,j)*recip_rhoConst
           phiHydF(i,j)=phiHydF(i,j)
     &            + drF(k)*gravity*alphaRho(i,j)*recip_rhoConst
          ENDDO
         ENDDO

       ELSE
C  --  Finite Difference Form

         dRlocM=half*drC(k)
         IF (k.EQ.1) dRlocM=rF(k)-rC(k)
         IF (k.EQ.Nr) THEN
           dRlocP=rC(k)-rF(k+1)
         ELSE
           dRlocP=half*drC(k+1)
         ENDIF

         DO j=jMin,jMax
          DO i=iMin,iMax

C---------- This discretization is the "energy conserving" form
C           which has been used since at least Adcroft et al., MWR 1997
C
            phiHydC(i,j)=phiHydF(i,j)
     &        +dRlocM*gravity*alphaRho(i,j)*recip_rhoConst
            phiHydF(i,j)=phiHydC(i,j)
     &        +dRlocP*gravity*alphaRho(i,j)*recip_rhoConst
          ENDDO
         ENDDO

C  --  end if integr_GeoPot = ...
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
C       This is the hydrostatic pressure calculation for the Ocean
C       which uses the FIND_RHO() routine to calculate density before
C       integrating (1/rho)_prime*dp over the current layer/interface
#ifdef      ALLOW_AUTODIFF_TAMC
CADJ GENERAL
#endif      /* ALLOW_AUTODIFF_TAMC */

        IF ( implicitIntGravWave .OR. myIter.LT.0 ) THEN
C--     Calculate density
#ifdef ALLOW_AUTODIFF_TAMC
          kkey = (ikey-1)*Nr + k
CADJ STORE tFld (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte,
CADJ &     kind = isbyte
CADJ STORE sFld (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              tFld(1-OLx,1-OLy,k,bi,bj),
     I              sFld(1-OLx,1-OLy,k,bi,bj),
     O              alphaRho,
     I              k, bi, bj, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE alphaRho (:,:) = comlev1_bibj_k, key=kkey, byte=isbyte,
CADJ &     kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        ELSE
          DO j=jMin,jMax
           DO i=iMin,iMax
             alphaRho(i,j) = rhoInSitu(i,j,k,bi,bj)
           ENDDO
          ENDDO
        ENDIF

C--     Calculate specific volume anomaly : alpha_prime = 1/rho - alpha_Cst
        DO j=jMin,jMax
          DO i=iMin,iMax
            locAlpha=alphaRho(i,j)+rhoConst
            alphaRho(i,j)=maskC(i,j,k,bi,bj)*
     &              (one/locAlpha - recip_rhoConst)
          ENDDO
        ENDDO

C----  Hydrostatic pressure at cell centers

       IF (integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form

         DO j=jMin,jMax
          DO i=iMin,iMax

C---------- This discretization is the "finite volume" form
C           which has not been used to date since it does not
C           conserve KE+PE exactly even though it is more natural
C
           IF (k.EQ.kSurfC(i,j,bi,bj)) THEN
             ddRloc = Ro_surf(i,j,bi,bj)-rC(k)
#ifdef NONLIN_FRSURF
             ddRloc = ddRloc + surfPhiFac*etaH(i,j,bi,bj)
#endif
             phiHydC(i,j) = ddRloc*alphaRho(i,j)
c--to reproduce results of c48d_post: uncomment those 4+1 lines
c            phiHydC(i,j)=phiHydF(i,j)
c    &          +(hFacC(i,j,k,bi,bj)-half)*drF(k)*alphaRho(i,j)
c            phiHydF(i,j)=phiHydF(i,j)
c    &          + hFacC(i,j,k,bi,bj)*drF(k)*alphaRho(i,j)
           ELSE
             phiHydC(i,j) = phiHydF(i,j) + half*drF(k)*alphaRho(i,j)
c            phiHydF(i,j) = phiHydF(i,j) +      drF(k)*alphaRho(i,j)
           ENDIF
c-- and comment this last one:
             phiHydF(i,j) = phiHydC(i,j) + half*drF(k)*alphaRho(i,j)
c-----
          ENDDO
         ENDDO

       ELSE
C  --  Finite Difference Form, with Part-Cell Bathy

         dRlocM=half*drC(k)
         IF (k.EQ.1) dRlocM=rF(k)-rC(k)
         IF (k.EQ.Nr) THEN
           dRlocP=rC(k)-rF(k+1)
         ELSE
           dRlocP=half*drC(k+1)
         ENDIF
         rec_dRm = one/(rF(k)-rC(k))
         rec_dRp = one/(rC(k)-rF(k+1))

         DO j=jMin,jMax
          DO i=iMin,iMax

C---------- This discretization is the "energy conserving" form

           IF (k.EQ.kSurfC(i,j,bi,bj)) THEN
             ddRloc = Ro_surf(i,j,bi,bj)-rC(k)
#ifdef NONLIN_FRSURF
             ddRloc = ddRloc + surfPhiFac*etaH(i,j,bi,bj)
#endif
             phiHydC(i,j) =( MAX(zero,ddRloc)*rec_dRm*dRlocM
     &                      +MIN(zero,ddRloc)*rec_dRp*dRlocP
     &                     )*alphaRho(i,j)
           ELSE
             phiHydC(i,j) = phiHydF(i,j) + dRlocM*alphaRho(i,j)
           ENDIF
             phiHydF(i,j) = phiHydC(i,j) + dRlocP*alphaRho(i,j)
          ENDDO
         ENDDO

C  --  end if integr_GeoPot = ...
       ENDIF

      ELSEIF ( buoyancyRelation .EQ. 'ATMOSPHERIC' ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C       This is the hydrostatic geopotential calculation for the Atmosphere
C       The ideal gas law is used implicitly here rather than calculating
C       the specific volume, analogous to the oceanic case.

C--     virtual potential temperature anomaly (including water vapour effect)
        DO j=jMin,jMax
         DO i=iMin,iMax
          alphaRho(i,j)=maskC(i,j,k,bi,bj)
     &             *( tFld(i,j,k,bi,bj)*(sFld(i,j,k,bi,bj)*atm_Rq+one)
     &               -tRef(k) )
         ENDDO
        ENDDO

C---    Integrate d Phi / d pi

       IF (integr_GeoPot.EQ.0) THEN
C  --  Energy Conserving Form, accurate with Full cell topo  --
C------------ The integration for the first level phi(k=1) is the same
C             for both the "finite volume" and energy conserving methods.
C    *NOTE* o Working with geopotential Anomaly, the geopotential boundary
C             condition is simply Phi-prime(Ro_surf)=0.
C           o convention ddPI > 0 (same as drF & drC)
C-----------------------------------------------------------------------
         IF (k.EQ.1) THEN
           ddPIm=atm_Cp*( ((rF( k )/atm_Po)**atm_kappa)
     &                   -((rC( k )/atm_Po)**atm_kappa) )
         ELSE
           ddPIm=atm_Cp*( ((rC(k-1)/atm_Po)**atm_kappa)
     &                   -((rC( k )/atm_Po)**atm_kappa) )*half
         ENDIF
         IF (k.EQ.Nr) THEN
           ddPIp=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                   -((rF(k+1)/atm_Po)**atm_kappa) )
         ELSE
           ddPIp=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                   -((rC(k+1)/atm_Po)**atm_kappa) )*half
         ENDIF
C-------- This discretization is the energy conserving form
         DO j=jMin,jMax
          DO i=iMin,iMax
             phiHydC(i,j) = phiHydF(i,j) +ddPIm*alphaRho(i,j)
             phiHydF(i,j) = phiHydC(i,j) +ddPIp*alphaRho(i,j)
          ENDDO
         ENDDO
C end: Energy Conserving Form, No hFac  --
C-----------------------------------------------------------------------

       ELSEIF (integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form, with Part-Cell Topo, linear in P by Half level
C---------
C  Finite Volume formulation consistent with Partial Cell, linear in p by piece
C   Note: a true Finite Volume form should be linear between 2 Interf_W :
C     phi_C = (phi_W_k+ phi_W_k+1)/2 ; but not accurate in Stratosphere (low p)
C   also: if Interface_W at the middle between tracer levels, this form
C     is close to the Energy Cons. form in the Interior, except for the
C     non-linearity in PI(p)
C---------
           ddPIm=atm_Cp*( ((rF( k )/atm_Po)**atm_kappa)
     &                   -((rC( k )/atm_Po)**atm_kappa) )
           ddPIp=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                   -((rF(k+1)/atm_Po)**atm_kappa) )
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (k.EQ.kSurfC(i,j,bi,bj)) THEN
             ddRloc = Ro_surf(i,j,bi,bj)-rC(k)
#ifdef NONLIN_FRSURF
             ddRloc = ddRloc + surfPhiFac*etaH(i,j,bi,bj)
#endif
             phiHydC(i,j) = ddRloc*recip_drF(k)*2. _d 0
     &          *ddPIm*alphaRho(i,j)
           ELSE
             phiHydC(i,j) = phiHydF(i,j) +ddPIm*alphaRho(i,j)
           ENDIF
             phiHydF(i,j) = phiHydC(i,j) +ddPIp*alphaRho(i,j)
          ENDDO
         ENDDO
C end: Finite Volume Form, with Part-Cell Topo, linear in P by Half level
C-----------------------------------------------------------------------

       ELSEIF ( integr_GeoPot.EQ.2
     &     .OR. integr_GeoPot.EQ.3 ) THEN
C  --  Finite Difference Form, with Part-Cell Topo,
C       works with Interface_W  at the middle between 2.Tracer_Level
C        and  with Tracer_Level at the middle between 2.Interface_W.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  Finite Difference formulation consistent with Partial Cell,
C   Valid & accurate if Interface_W at middle between tracer levels
C   linear in p between 2 Tracer levels ; conserve energy in the Interior
C---------
         IF (k.EQ.1) THEN
           ddPIm=atm_Cp*( ((rF( k )/atm_Po)**atm_kappa)
     &                   -((rC( k )/atm_Po)**atm_kappa) )
         ELSE
           ddPIm=atm_Cp*( ((rC(k-1)/atm_Po)**atm_kappa)
     &                   -((rC( k )/atm_Po)**atm_kappa) )*half
         ENDIF
         IF (k.EQ.Nr) THEN
           ddPIp=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                   -((rF(k+1)/atm_Po)**atm_kappa) )
         ELSE
           ddPIp=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                   -((rC(k+1)/atm_Po)**atm_kappa) )*half
         ENDIF
         rec_dRm = one/(rF(k)-rC(k))
         rec_dRp = one/(rC(k)-rF(k+1))
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (k.EQ.kSurfC(i,j,bi,bj)) THEN
             ddRloc = Ro_surf(i,j,bi,bj)-rC(k)
#ifdef NONLIN_FRSURF
             ddRloc = ddRloc + surfPhiFac*etaH(i,j,bi,bj)
#endif
             phiHydC(i,j) =( MAX(zero,ddRloc)*rec_dRm*ddPIm
     &                      +MIN(zero,ddRloc)*rec_dRp*ddPIp )
     &                    *alphaRho(i,j)
           ELSE
             phiHydC(i,j) = phiHydF(i,j) +ddPIm*alphaRho(i,j)
           ENDIF
             phiHydF(i,j) = phiHydC(i,j) +ddPIp*alphaRho(i,j)
          ENDDO
         ENDDO
C end: Finite Difference Form, with Part-Cell Topo
C-----------------------------------------------------------------------

       ELSE
         STOP 'CALC_PHI_HYD: Bad integr_GeoPot option !'
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
        STOP 'CALC_PHI_HYD: Bad value of buoyancyRelation !'
      ENDIF

C---   Diagnose Phi at boundary r=R_low :
C       = Ocean bottom pressure (Ocean, Z-coord.)
C       = Sea-surface height    (Ocean, P-coord.)
C       = Top atmosphere height (Atmos, P-coord.)
      IF (useDiagPhiRlow) THEN
        CALL DIAGS_PHI_RLOW(
     I                      k, bi, bj, iMin,iMax, jMin,jMax,
     I                      phiHydF, phiHydC, alphaRho, tFld, sFld,
     I                      myTime, myIter, myThid)
      ENDIF

C---   Diagnose Full Hydrostatic Potential at cell center level
        CALL DIAGS_PHI_HYD(
     I                      k, bi, bj, iMin,iMax, jMin,jMax,
     I                      phiHydC,
     I                      myTime, myIter, myThid)

      IF (momPressureForcing) THEN
        CALL CALC_GRAD_PHI_HYD(
     I                         k, bi, bj, iMin,iMax, jMin,jMax,
     I                         phiHydC, alphaRho, tFld, sFld,
     O                         dPhiHydX, dPhiHydY,
     I                         myTime, myIter, myThid)
      ENDIF

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
