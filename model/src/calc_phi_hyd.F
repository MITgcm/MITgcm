C $Header: /u/gcmpack/MITgcm/model/src/calc_phi_hyd.F,v 1.17 2001/09/27 18:14:20 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_PHI_HYD
C     !INTERFACE:
      SUBROUTINE CALC_PHI_HYD(
     I                         bi, bj, iMin, iMax, jMin, jMax, K,
     I                         theta, salt,
     U                         phiHyd,
     I                         myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_PHI_HYD                                  |
C     | o Integrate the hydrostatic relation to find the Hydros. | 
C     *==========================================================*
C     |    Potential (ocean: Pressure/rho ; atmos = geopotential)|
C     | On entry:                                                |
C     |   theta,salt    are the current thermodynamics quantities|
C     |                 (unchanged on exit)                      |
C     |   phiHyd(i,j,1:k-1) is the hydrostatic Potential         |
C     |                 at cell centers (tracer points)          |
C     |                 - 1:k-1 layers are valid                 |
C     |                 - k:Nr layers are invalid                |
C     |   phiHyd(i,j,k) is the hydrostatic Potential             |
C     |  (ocean only_^) at cell the interface k (w point above)  |
C     | On exit:                                                 |
C     |   phiHyd(i,j,1:k) is the hydrostatic Potential           |
C     |                 at cell centers (tracer points)          |
C     |                 - 1:k layers are valid                   |
C     |                 - k+1:Nr layers are invalid              |
C     |   phiHyd(i,j,k+1) is the hydrostatic Potential (P/rho)   |
C     |  (ocean only-^) at cell the interface k+1 (w point below)|
C     | Atmosphere:                                              |
C     |   Integr_GeoPot allows to select one integration method  |
C     |    (see the list below)                                  |
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid
      
#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j, Kp1
      _RL zero, one, half
      _RL alphaRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dRloc,dRlocKp1
      _RL ddPI, ddPIm, ddPIp, ratioRp, ratioRm
CEOP

      zero = 0. _d 0
      one  = 1. _d 0
      half = .5 _d 0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  Atmosphere:  
C Integr_GeoPot => select one option for the integration of the Geopotential:
C   = 0 : Energy Conserving Form, No hFac ;
C   = 1 : Finite Volume Form, with hFac, linear in P by Half level;
C   =2,3: Finite Difference Form, with hFac, linear in P between 2 Tracer levels
C     2 : case Tracer level at the middle of InterFace_W; 
C     3 : case InterFace_W  at the middle of Tracer levels;
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

      IF ( buoyancyRelation .eq. 'OCEANIC' ) THEN
C       This is the hydrostatic pressure calculation for the Ocean
C       which uses the FIND_RHO() routine to calculate density
C       before integrating g*rho over the current layer/interface

        dRloc=drC(k)
        IF (k.EQ.1) dRloc=drF(1)
        IF (k.EQ.Nr) THEN
          dRlocKp1=0.
        ELSE
          dRlocKp1=drC(k+1)
        ENDIF

C--     If this is the top layer we impose the boundary condition
C       P(z=eta) = P(atmospheric_loading)
        IF (k.EQ.1) THEN
          DO j=jMin,jMax
            DO i=iMin,iMax
C             *NOTE* The loading should go here but has not been implemented yet
              phiHyd(i,j,k)=0.
            ENDDO
          ENDDO
        ENDIF

C       Calculate density
#ifdef ALLOW_AUTODIFF_TAMC
            kkey = (ikey-1)*Nr + k
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        CALL FIND_RHO( bi, bj, iMin, iMax, jMin, jMax, k, k, eosType,
     &                 theta, salt,
     &                 alphaRho, myThid)

C       Hydrostatic pressure at cell centers
        DO j=jMin,jMax
          DO i=iMin,iMax
#ifdef      ALLOW_AUTODIFF_TAMC
c           Patrick, is this directive correct or even necessary in
c           this new code?
c           Yes, because of phiHyd(i,j,k+1)=phiHyd(i,j,k)+...
c           within the k-loop.
CADJ GENERAL
#endif      /* ALLOW_AUTODIFF_TAMC */

C---------- This discretization is the "finite volume" form
C           which has not been used to date since it does not
C           conserve KE+PE exactly even though it is more natural
C
c           IF (k.LT.Nr) phiHyd(i,j,k+1)=phiHyd(i,j,k)+
c    &              drF(K)*gravity*alphaRho(i,j)*recip_rhoConst
c           phiHyd(i,j,k)=phiHyd(i,j,k)+
c    &          0.5*drF(K)*gravity*alphaRho(i,j)*recip_rhoConst
C-----------------------------------------------------------------------

C---------- This discretization is the "energy conserving" form
C           which has been used since at least Adcroft et al., MWR 1997
C
            phiHyd(i,j,k)=phiHyd(i,j,k)+
     &          0.5*dRloc*gravity*alphaRho(i,j)*recip_rhoConst
            IF (k.LT.Nr) phiHyd(i,j,k+1)=phiHyd(i,j,k)+
     &          0.5*dRlocKp1*gravity*alphaRho(i,j)*recip_rhoConst
C-----------------------------------------------------------------------
          ENDDO
        ENDDO
        


      ELSEIF ( buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C       This is the hydrostatic geopotential calculation for the Atmosphere
C       The ideal gas law is used implicitly here rather than calculating
C       the specific volume, analogous to the oceanic case.

C       Integrate d Phi / d pi

      IF (Integr_GeoPot.EQ.0) THEN
C  --  Energy Conserving Form, No hFac  --
C------------ The integration for the first level phi(k=1) is the same
C             for both the "finite volume" and energy conserving methods.
Ci    *NOTE* o Working with geopotential Anomaly, the geopotential boundary 
C             condition is simply Phi-prime(Ro_surf)=0.
C           o convention ddPI > 0 (same as drF & drC)
C-----------------------------------------------------------------------
        IF (K.EQ.1) THEN
          ddPIp=atm_cp*( ((rF(K)/atm_po)**atm_kappa)
     &                  -((rC(K)/atm_po)**atm_kappa) )
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K)=
     &          ddPIp*maskC(i,j,K,bi,bj)
     &               *(theta(I,J,K,bi,bj)-tRef(K))
           ENDDO
          ENDDO
        ELSE
C-------- This discretization is the energy conserving form
          ddPI=atm_cp*( ((rC(K-1)/atm_po)**atm_kappa)
     &                 -((rC( K )/atm_po)**atm_kappa) )*0.5
          DO j=jMin,jMax
           DO i=iMin,iMax
              phiHyd(i,j,K)=phiHyd(i,j,K-1)
     &           +ddPI*maskC(i,j,K-1,bi,bj)
     &                *(theta(I,J,K-1,bi,bj)-tRef(K-1))
     &           +ddPI*maskC(i,j, K ,bi,bj)
     &                *(theta(I,J, K ,bi,bj)-tRef( K ))
C             Old code (atmos-exact) looked like this
Cold          phiHyd(i,j,K)=phiHyd(i,j,K-1) - ddPI*
Cold &      (theta(I,J,K-1,bi,bj)+theta(I,J,K,bi,bj)-2.*tRef(K))
           ENDDO
          ENDDO
        ENDIF
C end: Energy Conserving Form, No hFac  --
C-----------------------------------------------------------------------

      ELSEIF (Integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form, with hFac, linear in P by Half level  --
C---------
C  Finite Volume formulation consistent with Partial Cell, linear in p by piece
C   Note: a true Finite Volume form should be linear between 2 Interf_W :
C     phi_C = (phi_W_k+ phi_W_k+1)/2 ; but not accurate in Stratosphere (low p)
C   also: if Interface_W at the middle between tracer levels, this form
C     is close to the Energy Cons. form in the Interior, except for the 
C     non-linearity in PI(p)
C---------
        IF (K.EQ.1) THEN
          ddPIp=atm_cp*( ((rF(K)/atm_po)**atm_kappa)
     &                  -((rC(K)/atm_po)**atm_kappa) )
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K) =
     &          ddPIp*hFacC(I,J, K ,bi,bj)
     &               *(theta(I,J, K ,bi,bj)-tRef( K ))
           ENDDO
          ENDDO
        ELSE
          ddPIm=atm_cp*( ((rC(K-1)/atm_po)**atm_kappa)
     &                  -((rF( K )/atm_po)**atm_kappa) )
          ddPIp=atm_cp*( ((rF( K )/atm_po)**atm_kappa)
     &                  -((rC( K )/atm_po)**atm_kappa) )
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K) = phiHyd(i,j,K-1)
     &         +ddPIm*hFacC(I,J,K-1,bi,bj)
     &               *(theta(I,J,K-1,bi,bj)-tRef(K-1))
     &         +ddPIp*hFacC(I,J, K ,bi,bj)
     &               *(theta(I,J, K ,bi,bj)-tRef( K ))
           ENDDO
          ENDDO
        ENDIF
C end: Finite Volume Form, with hFac, linear in P by Half level  --
C-----------------------------------------------------------------------

      ELSEIF (Integr_GeoPot.EQ.2) THEN
C  --  Finite Difference Form, with hFac, Tracer Lev. = middle  --
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  Finite Difference formulation consistent with Partial Cell,
C    case Tracer level at the middle of InterFace_W 
C    linear between 2 Tracer levels ; conserve energy in the Interior 
C---------
        Kp1 = min(Nr,K+1)
        IF (K.EQ.1) THEN
          ddPIm=atm_cp*( ((rF( K )/atm_po)**atm_kappa)
     &                  -((rC( K )/atm_po)**atm_kappa) ) * 2. _d 0
          ddPIp=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                  -((rC(Kp1)/atm_po)**atm_kappa) )  
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K) =
     &        ( ddPIm*max(zero, hFacC(i,j,K,bi,bj)-half)
     &         +ddPIp*min(zero, hFacC(i,j,K,bi,bj)-half) )
     &               *(theta(i,j, K ,bi,bj)-tRef( K ))
     &               * maskC(i,j, K ,bi,bj)
           ENDDO
          ENDDO
        ELSE
          ddPIm=atm_cp*( ((rC(K-1)/atm_po)**atm_kappa)
     &                  -((rC( K )/atm_po)**atm_kappa) )
          ddPIp=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                  -((rC(Kp1)/atm_po)**atm_kappa) ) 
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K) = phiHyd(i,j,K-1)
     &        + ddPIm*0.5
     &               *(theta(i,j,K-1,bi,bj)-tRef(K-1))
     &               * maskC(i,j,K-1,bi,bj)
     &        +(ddPIm*max(zero, hFacC(i,j,K,bi,bj)-half)
     &         +ddPIp*min(zero, hFacC(i,j,K,bi,bj)-half) )
     &               *(theta(i,j, K ,bi,bj)-tRef( K ))
     &               * maskC(i,j, K ,bi,bj)
           ENDDO
          ENDDO
        ENDIF
C end: Finite Difference Form, with hFac, Tracer Lev. = middle  --
C-----------------------------------------------------------------------

      ELSEIF (Integr_GeoPot.EQ.3) THEN
C  --  Finite Difference Form, with hFac, Interface_W = middle  --
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C  Finite Difference formulation consistent with Partial Cell,
C   Valid & accurate if Interface_W at middle between tracer levels
C   linear in p between 2 Tracer levels ; conserve energy in the Interior 
C---------
        Kp1 = min(Nr,K+1)
        IF (K.EQ.1) THEN
          ratioRm=0.5*drF(K)/(rF(k)-rC(K))
          ratioRp=drF(K)*recip_drC(Kp1)
          ddPIm=atm_cp*( ((rF( K )/atm_po)**atm_kappa)
     &                  -((rC( K )/atm_po)**atm_kappa) ) * 2. _d 0
          ddPIp=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                  -((rC(Kp1)/atm_po)**atm_kappa) )  
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K) =
     &        ( ddPIm*max(zero,(hFacC(i,j,K,bi,bj)-one)*ratioRm+half)
     &         +ddPIp*min(zero, hFacC(i,j,K,bi,bj)*ratioRp     -half) )
     &               *(theta(i,j, K ,bi,bj)-tRef( K ))
     &               * maskC(i,j, K ,bi,bj)
           ENDDO
          ENDDO
        ELSE
          ratioRm=drF(K)*recip_drC(K)
          ratioRp=drF(K)*recip_drC(Kp1)
          ddPIm=atm_cp*( ((rC(K-1)/atm_po)**atm_kappa)
     &                  -((rC( K )/atm_po)**atm_kappa) )
          ddPIp=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                  -((rC(Kp1)/atm_po)**atm_kappa) ) 
          DO j=jMin,jMax
           DO i=iMin,iMax
             phiHyd(i,j,K) = phiHyd(i,j,K-1)
     &        + ddPIm*0.5
     &               *(theta(i,j,K-1,bi,bj)-tRef(K-1))
     &               * maskC(i,j,K-1,bi,bj)
     &        +(ddPIm*max(zero,(hFacC(i,j,K,bi,bj)-one)*ratioRm+half)
     &         +ddPIp*min(zero, hFacC(i,j,K,bi,bj)*ratioRp     -half) )
     &               *(theta(i,j, K ,bi,bj)-tRef( K ))
     &               * maskC(i,j, K ,bi,bj)
           ENDDO
          ENDDO
        ENDIF
C end: Finite Difference Form, with hFac, Interface_W = middle  --
C-----------------------------------------------------------------------

      ELSE
        STOP 'CALC_PHI_HYD: Bad Integr_GeoPot option !'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
        STOP 'CALC_PHI_HYD: We should never reach this point!'
      ENDIF

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
