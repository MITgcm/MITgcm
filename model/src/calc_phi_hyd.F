C $Header: /u/gcmpack/MITgcm/model/src/calc_phi_hyd.F,v 1.11 2001/03/08 20:21:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CALC_PHI_HYD(
     I                         bi, bj, iMin, iMax, jMin, jMax, K,
     I                         theta, salt,
     U                         phiHyd,
     I                         myThid)
C     /==========================================================\
C     | SUBROUTINE CALC_PHI_HYD                                  |
C     | o Integrate the hydrostatic relation to find the Hydros. | 
C     |    Potential (ocean: Pressure/rho ; atmos = geopotential)|
C     | On entry:                                                |
C     |   theta,salt    are the current thermodynamics quantities|
C     |                 (unchanged on exit)                      |
C     |   phiHyd(i,j,1:k-1) is the hydrostatic Potential         |
C     |                 at cell centers (tracer points)          |
C     |                 - 1:k-1 layers are valid                 |
C     |                 - k:Nr layers are invalid                |
C     |   phiHyd(i,j,k) is the hydrostatic Potential             |
C     |                 at cell the interface k (w point above)  |
C     | On exit:                                                 |
C     |   phiHyd(i,j,1:k) is the hydrostatic Potential           |
C     |                 at cell centers (tracer points)          |
C     |                 - 1:k layers are valid                   |
C     |                 - k+1:Nr layers are invalid              |
C     |   phiHyd(i,j,k+1) is the hydrostatic Potential (P/rho)   |
C     |                 at cell the interface k+1 (w point below)|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     == Local variables ==
      INTEGER i,j
      _RL alphaRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dRloc,dRlocKp1
      _RL ddRm1, ddRp1, ddRm, ddRp
      _RL atm_cp, atm_kappa, atm_po

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
        CALL FIND_RHO( bi, bj, iMin, iMax, jMin, jMax, k, k, eosType,
     &                 theta, salt,
     &                 alphaRho, myThid)

C       Hydrostatic pressure at cell centers
        DO j=jMin,jMax
          DO i=iMin,iMax
#ifdef      ALLOW_AUTODIFF_TAMC
            Is this directive correct or even necessary in this new code?
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
C       This is the hydrostatic geopotential calculation for the Atmosphere
C       The ideal gas law is used implicitly here rather than calculating
C       the specific volume, analogous to the oceanic case.

C       Integrate d Phi / d pi

C *NOTE* These constants should be in the data file and PARAMS.h
        atm_cp=1004. _d 0
        atm_kappa=2. _d 0/7. _d 0
        atm_po=1. _d 5
        IF (K.EQ.1) THEN
          ddRp1=atm_cp*( ((rC(K)/atm_po)**atm_kappa)
     &                  -((rF(K)/atm_po)**atm_kappa) )
          DO j=jMin,jMax
            DO i=iMin,iMax
              ddRp=ddRp1
              IF (hFacC(I,J, K ,bi,bj).EQ.0.) ddRp=0.
C------------ The integration for the first level phi(k=1) is the
C             same for both the "finite volume" and energy conserving
C             methods.
C             *NOTE* The geopotential boundary condition should go
C                    here but has not been implemented yet
              phiHyd(i,j,K)=0.
     &          -ddRp*(theta(I,J,K,bi,bj)-tRef(K))
C-----------------------------------------------------------------------
           ENDDO
          ENDDO
        ELSE

C-------- This discretization is the "finite volume" form which
C         integrates the hydrostatic equation of each half/sub-layer.
C         This seems most natural and could easily allow for lopped cells
C         by replacing rF(K) with the height of the surface (not implemented).
C         in the lower layers (e.g. at k=1).
C
c         ddRm1=atm_cp*( ((rF( K )/atm_po)**atm_kappa)
c    &                  -((rC(K-1)/atm_po)**atm_kappa) )
c         ddRp1=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
c    &                  -((rF( K )/atm_po)**atm_kappa) )
C-----------------------------------------------------------------------


C-------- This discretization is the energy conserving form
          ddRp1=atm_cp*( ((rC( K )/atm_po)**atm_kappa)
     &                  -((rC(K-1)/atm_po)**atm_kappa) )*0.5
          ddRm1=ddRp1
C-----------------------------------------------------------------------

          DO j=jMin,jMax
            DO i=iMin,iMax
              ddRp=ddRp1
              ddRm=ddRm1
              IF (hFacC(I,J, K ,bi,bj).EQ.0.) ddRp=0.
              IF (hFacC(I,J,K-1,bi,bj).EQ.0.) ddRm=0.
              phiHyd(i,j,K)=phiHyd(i,j,K-1)
     &           -( ddRm*(theta(I,J,K-1,bi,bj)-tRef(K-1))
     &             +ddRp*(theta(I,J, K ,bi,bj)-tRef( K )) )
C             Old code (atmos-exact) looked like this
Cold          phiHyd(i,j,K)=phiHyd(i,j,K-1) - ddRm1*
Cold &      (theta(I,J,K-1,bi,bj)+theta(I,J,K,bi,bj)-2.*tRef(K))
            ENDDO
          ENDDO
        ENDIF


      ELSE
        STOP 'CALC_PHI_HYD: We should never reach this point!'
      ENDIF

#endif

      RETURN
      END
