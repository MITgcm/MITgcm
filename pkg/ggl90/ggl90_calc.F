C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_calc.F,v 1.5 2005/08/05 23:46:09 ce107 Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

CBOP
C !ROUTINE: GGL90_CALC

C !INTERFACE: ======================================================
      subroutine GGL90_CALC(
     I     bi, bj, myTime, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE GGL90_CALC                                    |
C     | o Compute all GGL90 fields defined in GGL90.h            |
C     |==========================================================|
C     | Equation numbers refer to                                |
C     | Gaspar et al. (1990), JGR 95 (C9), pp 16,179             |
C     | Some parts of the implementation follow Blanke and       |
C     | Delecuse (1993), JPO, and OPA code, in particular the    |
C     | computation of the                                       |
C     | mixing length = max(min(lk,depth),lkmin)                 |
C     \==========================================================/
      IMPLICIT NONE
C
C--------------------------------------------------------------------

C global parameters updated by ggl90_calc
C     GGL90TKE     - sub-grid turbulent kinetic energy           (m^2/s^2)
C     GGL90viscAz  - GGL90 eddy viscosity coefficient              (m^2/s)
C     GGL90diffKzT - GGL90 diffusion coefficient for temperature   (m^2/s)
C
C \ev

C !USES: ============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GGL90.h"
#include "FFIELDS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER bi, bj
      INTEGER myThid
      _RL     myTime

#ifdef ALLOW_GGL90

C !LOCAL VARIABLES: ====================================================
c Local constants
C     iMin, iMax, jMin, jMax, I, J - array computation indices
C     K, Kp1, km1, kSurf, kBottom  - vertical loop indices
C     ab15, ab05       - weights for implicit timestepping
C     uStarSquare      - square of friction velocity
C     verticalShear    - (squared) vertical shear of horizontal velocity
C     Nsquare          - squared buoyancy freqency 
C     RiNumber         - local Richardson number
C     KappaM           - (local) viscosity parameter (eq.10)
C     KappaH           - (local) diffusivity parameter for temperature (eq.11)
C     KappaE           - (local) diffusivity parameter for TKE (eq.15)
C     buoyFreq         - buoyancy freqency
C     TKEdissipation   - dissipation of TKE
C     GGL90mixingLength- mixing length of scheme following Banke+Delecuse
C     totalDepth       - thickness of water column (inverse of recip_Rcol)
C     TKEPrandtlNumber - here, an empirical function of the Richardson number
C     rhoK, rhoKm1     - density at layer K and Km1 (relative to K)
C     gTKE             - right hand side of implicit equation
      INTEGER iMin ,iMax ,jMin ,jMax
      INTEGER I, J, K, Kp1, Km1, kSurf, kBottom
      _RL     ab15, ab05
      _RL     uStarSquare
      _RL     verticalShear
      _RL     KappaM, KappaH
      _RL     Nsquare
      _RL     deltaTggl90
      _RL     SQRTTKE
      _RL     RiNumber
      _RL     TKEdissipation
      _RL     tempU, tempV, prTemp
      _RL     TKEPrandtlNumber (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     GGL90mixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     KappaE           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     rhoK             (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoKm1           (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     totalDepth       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gTKE             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     tri-diagonal matrix
      _RL     a(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     c(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_GGL90_HORIZDIFF
C     xA, yA   - area of lateral faces
C     dfx, dfy - diffusive flux across lateral faces
      _RL     xA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     yA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dfy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_GGL90_HORIZDIFF */
CEOP
      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

C     set separate time step (should be deltaTtracer)
      deltaTggl90 = dTtracerLev(1)
C     
      kSurf = 1
C     implicit timestepping weights for dissipation
      ab15 =  1.5 _d 0
      ab05 = -0.5 _d 0
      ab15 =  1. _d 0
      ab05 =  0. _d 0

C     Initialize local fields
      DO K = 1, Nr
       DO J=1-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx
         gTKE(I,J,K)              = 0. _d 0
         KappaE(I,J,K)            = 0. _d 0
         TKEPrandtlNumber(I,J,K)  = 0. _d 0
         GGL90mixingLength(I,J,K) = 0. _d 0
        ENDDO
       ENDDO	
      ENDDO
      DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
	rhoK    (I,J) = 0. _d 0
	rhoKm1  (I,J) = 0. _d 0
        totalDepth(I,J)   = 0. _d 0
        IF ( recip_Rcol(I,J,bi,bj) .NE. 0. ) 
     &       totalDepth(I,J) = 1./recip_Rcol(I,J,bi,bj)
       ENDDO
      ENDDO

C     start k-loop
      DO K = 2, Nr
       Km1 = K-1
       Kp1 = MIN(Nr,K+1)
       CALL FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax, Km1, K,
     I      theta, salt,
     O      rhoKm1,
     I      myThid )
       CALL FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax, K, K,
     I      theta, salt,
     O      rhoK,
     I      myThid )
       DO J=jMin,jMax
        DO I=iMin,iMax
         SQRTTKE=SQRT( GGL90TKE(I,J,K,bi,bj) )
C
C     buoyancy frequency
C
         Nsquare = - gravity*recip_rhoConst*recip_drC(K)
     &        * ( rhoKm1(I,J) - rhoK(I,J) )*maskC(I,J,K,bi,bj)
C     vertical shear term (dU/dz)^2+(dV/dz)^2
         tempu= .5*(  uVel(I,J,Km1,bi,bj)+uVel(I+1,J,Km1,bi,bj)
     &             - (uVel(I,J,K  ,bi,bj)+uVel(I+1,J,K  ,bi,bj)) )
     &        *recip_drC(K)
         tempv= .5*(  vVel(I,J,Km1,bi,bj)+vVel(I,J+1,Km1,bi,bj)
     &             - (vVel(I,J,K  ,bi,bj)+vVel(I,J+1,K  ,bi,bj)) )
     &        *recip_drC(K)
         verticalShear = tempU*tempU + tempV*tempV
         RiNumber   = MAX(Nsquare,0. _d 0)/(verticalShear+GGL90eps)
C     compute Prandtl number (always greater than 0)
         prTemp = 1. _d 0
         IF ( RiNumber .GE. 0.2 ) prTemp = 5.0 * RiNumber
         TKEPrandtlNumber(I,J,K) = MIN(10.0 _d 0,prTemp)
C     mixing length
         GGL90mixingLength(I,J,K) = 
     &        SQRTTKE/SQRT( MAX(Nsquare,GGL90eps) ) 
C     impose upper bound for mixing length (total depth)
         GGL90mixingLength(I,J,K) = MIN(GGL90mixingLength(I,J,K), 
     &        totalDepth(I,J))
C     impose minimum mixing length (to avoid division by zero)
         GGL90mixingLength(I,J,K) = MAX(GGL90mixingLength(I,J,K), 
     &        GGL90mixingLengthMin)
C     viscosity of last timestep
         KappaM = GGL90ck*GGL90mixingLength(I,J,K)*SQRTTKE
         KappaE(I,J,K) = KappaM*GGL90alpha
C     dissipation term
         TKEdissipation = ab05*GGL90ceps
     &        *SQRTTKE/GGL90mixingLength(I,J,K)
     &        *GGL90TKE(I,J,K,bi,bj)      
C     sum up contributions to form the right hand side
         gTKE(I,J,K) = GGL90TKE(I,J,K,bi,bj) 
     &        + deltaTggl90*(
     &        + KappaM*verticalShear
     &        - KappaM*Nsquare/TKEPrandtlNumber(I,J,K)
     &        - TKEdissipation 
     &        )
        ENDDO	
       ENDDO
      ENDDO
C     horizontal diffusion of TKE (requires an exchange in 
C	do_fields_blocking_exchanges)
#ifdef ALLOW_GGL90_HORIZDIFF
      IF ( GGL90diffTKEh .GT. 0. _d 0 ) THEN
       DO K=2,Nr
C     common factors
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xA(i,j) = _dyG(i,j,bi,bj)
     &         *drF(k)*_hFacW(i,j,k,bi,bj)
          yA(i,j) = _dxG(i,j,bi,bj)
     &         *drF(k)*_hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO	
C     Compute diffusive fluxes
C     ... across x-faces
        DO j=1-Oly,sNy+Oly
         dfx(1-Olx,j)=0.
         DO i=1-Olx+1,sNx+Olx
          dfx(i,j) = -GGL90diffTKEh*xA(i,j)
     &      *_recip_dxC(i,j,bi,bj)
     &      *(GGL90TKE(i,j,k,bi,bj)-GGL90TKE(i-1,j,k,bi,bj))
     &      *CosFacU(j,bi,bj)
         ENDDO
        ENDDO
C     ... across y-faces
        DO i=1-Olx,sNx+Olx
         dfy(i,1-Oly)=0.
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx,sNx+Olx
          dfy(i,j) = -GGL90diffTKEh*yA(i,j)
     &      *_recip_dyC(i,j,bi,bj)
     &      *(GGL90TKE(i,j,k,bi,bj)-GGL90TKE(i,j-1,k,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &      *CosFacV(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */
         ENDDO
        ENDDO	
C     Compute divergence of fluxes
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          gTKE(i,j,k)=gTKE(i,j,k)
     &   -_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)*recip_rA(i,j,bi,bj)
     &         *( (dfx(i+1,j)-dfx(i,j))
     &           +(dfy(i,j+1)-dfy(i,j)) 
     &           )
         ENDDO	
        ENDDO
C	end of k-loop
       ENDDO
C     end if GGL90diffTKEh .eq. 0.
      ENDIF
#endif /* ALLOW_GGL90_HORIZDIFF */

C     ============================================
C     Implicit time step to update TKE for k=1,Nr;
C     TKE(Nr+1)=0 by default
C     ============================================
C     set up matrix
C--   Lower diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         a(i,j,1) = 0. _d 0 
       ENDDO
      ENDDO
      DO k=2,Nr
       km1=MAX(1,k-1)
       DO j=jMin,jMax
        DO i=iMin,iMax
         a(i,j,k) = -deltaTggl90
     &        *recip_drF(km1)*recip_hFacI(i,j,k,bi,bj)
     &        *.5*(KappaE(i,j, k )+KappaE(i,j,km1))
     &        *recip_drC(k)
          IF (recip_hFacI(i,j,km1,bi,bj).EQ.0.) a(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO
C--   Upper diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         c(i,j,1)  = 0. _d 0
         c(i,j,Nr) = 0. _d 0
       ENDDO
      ENDDO
CML      DO k=1,Nr-1
      DO k=2,Nr-1
       kp1=min(Nr,k+1)
       DO j=jMin,jMax
        DO i=iMin,iMax
          c(i,j,k) = -deltaTggl90
     &        *recip_drF( k )*recip_hFacI(i,j,k,bi,bj)
     &               *.5*(KappaE(i,j,k)+KappaE(i,j,kp1))
     &        *recip_drC(k)
          IF (recip_hFacI(i,j,kp1,bi,bj).EQ.0.) c(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO
C--   Center diagonal
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          b(i,j,k) = 1. _d 0 - c(i,j,k) - a(i,j,k)
     &        + ab15*deltaTggl90*GGL90ceps*SQRT(GGL90TKE(I,J,K,bi,bj))
     &        /GGL90mixingLength(I,J,K)
         ENDDO
       ENDDO
      ENDDO
C     end set up matrix

C
C     Apply boundary condition
C     
      DO J=jMin,jMax
       DO I=iMin,iMax
C     estimate friction velocity uStar from surface forcing
        uStarSquare = SQRT( 
     &         ( .5*( surfaceForcingU(I,  J,  bi,bj)
     &              + surfaceForcingU(I+1,J,  bi,bj) ) )**2
     &       + ( .5*( surfaceForcingV(I,  J,  bi,bj) 
     &              + surfaceForcingV(I,  J+1,bi,bj) ) )**2
     &                     )
C     Dirichlet surface boundary condition for TKE
        gTKE(I,J,kSurf) = MAX(GGL90TKEmin,GGL90m2*uStarSquare)
     &                     *maskC(I,J,kSurf,bi,bj)
C     Dirichlet bottom boundary condition for TKE = GGL90TKEbottom
        kBottom   = MIN(MAX(kLowC(I,J,bi,bj),1),Nr)
        gTKE(I,J,kBottom) = gTKE(I,J,kBottom) 
     &       - GGL90TKEbottom*c(I,J,kBottom)
       ENDDO
      ENDDO	
C
C     solve tri-diagonal system, and store solution on gTKE (previously rhs)
C
      CALL GGL90_SOLVE( bi, bj, iMin, iMax, jMin, jMax,
     I     a, b, c,
     U     gTKE,
     I     myThid )
C
C     now update TKE
C     
      DO K=1,Nr
       DO J=jMin,jMax
        DO I=iMin,iMax
C     impose minimum TKE to avoid numerical undershoots below zero
         GGL90TKE(I,J,K,bi,bj) = MAX( gTKE(I,J,K), GGL90TKEmin ) 
     &        * maskC(I,J,K,bi,bj)
        ENDDO
       ENDDO
      ENDDO	
C
C     end of time step
C     ===============================
C     compute viscosity coefficients
C     
      DO K=2,Nr
       DO J=jMin,jMax
        DO I=iMin,iMax
C     Eq. (11), (18) and (21)
         KappaM = GGL90ck*GGL90mixingLength(I,J,K)*
     &                  SQRT( GGL90TKE(I,J,K,bi,bj) )
         KappaH = KappaM/TKEPrandtlNumber(I,J,K)
C     Set a minium (= background) value
         KappaM = MAX(KappaM,viscAr)
         KappaH = MAX(KappaH,diffKrNrT(k))
C     Set a maximum and mask land point
         GGL90viscAr(I,J,K,bi,bj) = MIN(KappaM,GGL90viscMax)
     &        * maskC(I,J,K,bi,bj)
         GGL90diffKr(I,J,K,bi,bj) = MIN(KappaH,GGL90diffMax)
     &        * maskC(I,J,K,bi,bj)
        ENDDO
       ENDDO 
C     end third k-loop
      ENDDO	

#endif /* ALLOW_GGL90 */

      RETURN
      END

