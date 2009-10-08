C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_calc.F,v 1.12 2009/10/08 20:07:18 jmc Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

CBOP
C !ROUTINE: GGL90_CALC

C !INTERFACE: ======================================================
      subroutine GGL90_CALC(
     I     bi, bj, myTime, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GGL90_CALC                                    |
C     | o Compute all GGL90 fields defined in GGL90.h            |
C     *==========================================================*
C     | Equation numbers refer to                                |
C     | Gaspar et al. (1990), JGR 95 (C9), pp 16,179             |
C     | Some parts of the implementation follow Blanke and       |
C     | Delecuse (1993), JPO, and OPA code, in particular the    |
C     | computation of the                                       |
C     | mixing length = max(min(lk,depth),lkmin)                 |
C     *==========================================================*

C global parameters updated by ggl90_calc
C     GGL90TKE     :: sub-grid turbulent kinetic energy          (m^2/s^2)
C     GGL90viscAz  :: GGL90 eddy viscosity coefficient             (m^2/s)
C     GGL90diffKzT :: GGL90 diffusion coefficient for temperature  (m^2/s)
C
C \ev

C !USES: ============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GGL90.h"
#include "FFIELDS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C Routine arguments
C     bi, bj :: array indices on which to apply calculations
C     myTime :: Current time in simulation
C     myThid :: My Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_GGL90

C !LOCAL VARIABLES: ====================================================
C Local constants
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
C     TKEdissipation   - dissipation of TKE
C     GGL90mixingLength- mixing length of scheme following Banke+Delecuse
C         rMixingLength- inverse of mixing length
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
c     _RL     Nsquare
      _RL     Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     deltaTggl90
c     _RL     SQRTTKE
      _RL     SQRTTKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     RiNumber
      _RL     TKEdissipation
      _RL     tempU, tempV, prTemp
      _RL     MaxLength
      _RL     TKEPrandtlNumber (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     GGL90mixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL         rMixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
#ifdef ALLOW_GGL90_SMOOTH
      _RL p4, p8, p16, tmpdiffKrS
      p4=0.25 _d 0
      p8=0.125 _d 0
      p16=0.0625 _d 0
#endif
      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

C     set separate time step (should be deltaTtracer)
      deltaTggl90 = dTtracerLev(1)

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
         GGL90mixingLength(I,J,K) = GGL90mixingLengthMin
             rMixingLength(I,J,K) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
      DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
        rhoK    (I,J) = 0. _d 0
        rhoKm1  (I,J) = 0. _d 0
        totalDepth(I,J) = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
       ENDDO
      ENDDO

C     start k-loop
      DO K = 2, Nr
       Km1 = K-1
c      Kp1 = MIN(Nr,K+1)
       CALL FIND_RHO_2D(
     I      iMin, iMax, jMin, jMax, K,
     I      theta(1-OLx,1-OLy,Km1,bi,bj), salt(1-OLx,1-OLy,Km1,bi,bj),
     O      rhoKm1,
     I      Km1, bi, bj, myThid )

       CALL FIND_RHO_2D(
     I      iMin, iMax, jMin, jMax, K,
     I      theta(1-OLx,1-OLy,K,bi,bj), salt(1-OLx,1-OLy,K,bi,bj),
     O      rhoK,
     I      K, bi, bj, myThid )
       DO J=jMin,jMax
        DO I=iMin,iMax
         SQRTTKE(i,j,k)=SQRT( GGL90TKE(I,J,K,bi,bj) )
C
C     buoyancy frequency
C
         Nsquare(i,j,k) = - gravity*recip_rhoConst*recip_drC(K)
     &        * ( rhoKm1(I,J) - rhoK(I,J) )*maskC(I,J,K,bi,bj)
cC     vertical shear term (dU/dz)^2+(dV/dz)^2
c         tempU= .5 _d 0*( uVel(I,J,Km1,bi,bj)+uVel(I+1,J,Km1,bi,bj)
c     &                 -( uVel(I,J,K  ,bi,bj)+uVel(I+1,J,K  ,bi,bj)) )
c     &        *recip_drC(K)
c         tempV= .5 _d 0*( vVel(I,J,Km1,bi,bj)+vVel(I,J+1,Km1,bi,bj)
c     &                 -( vVel(I,J,K  ,bi,bj)+vVel(I,J+1,K  ,bi,bj)) )
c     &        *recip_drC(K)
c         verticalShear = tempU*tempU + tempV*tempV
c         RiNumber   = MAX(Nsquare(i,j,k),0. _d 0)/(verticalShear+GGL90eps)
cC     compute Prandtl number (always greater than 0)
c         prTemp = 1. _d 0
c         IF ( RiNumber .GE. 0.2 _d 0 ) prTemp = 5. _d 0 * RiNumber
c         TKEPrandtlNumber(I,J,K) = MIN(10. _d 0,prTemp)
C     mixing length
         GGL90mixingLength(I,J,K) = SQRTTWO *
     &        SQRTTKE(i,j,k)/SQRT( MAX(Nsquare(i,j,k),GGL90eps) )
        ENDDO
       ENDDO
      ENDDO

C- Impose upper bound for mixing length (total depth)
      IF ( mxlMaxFlag .EQ. 0 ) THEN
       DO k=2,Nr
        DO J=jMin,jMax
         DO I=iMin,iMax
          MaxLength=totalDepth(I,J)
          GGL90mixingLength(I,J,K) = MIN(GGL90mixingLength(I,J,K),
     &        MaxLength)
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( mxlMaxFlag .EQ. 1 ) THEN
       DO k=2,Nr
        DO J=jMin,jMax
         DO I=iMin,iMax
          MaxLength=MIN(Ro_surf(I,J,bi,bj)-rF(k),rF(k)-R_low(I,J,bi,bj))
c         MaxLength=MAX(MaxLength,20. _d 0)
          GGL90mixingLength(I,J,K) = MIN(GGL90mixingLength(I,J,K),
     &        MaxLength)
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( mxlMaxFlag .EQ. 2 ) THEN
       DO k=2,Nr
        DO J=jMin,jMax
         DO I=iMin,iMax
          GGL90mixingLength(I,J,K) = MIN(GGL90mixingLength(I,J,K),
     &        GGL90mixingLength(I,J,K-1)+drF(k-1))
         ENDDO
        ENDDO
       ENDDO
       DO J=jMin,jMax
        DO I=iMin,iMax
          GGL90mixingLength(I,J,Nr) = MIN(GGL90mixingLength(I,J,Nr),
     &       GGL90mixingLengthMin+drF(Nr))
        ENDDO
       ENDDO
       DO k=Nr-1,2,-1
        DO J=jMin,jMax
         DO I=iMin,iMax
          GGL90mixingLength(I,J,K) = MIN(GGL90mixingLength(I,J,K),
     &        GGL90mixingLength(I,J,K+1)+drF(k))
         ENDDO
        ENDDO
       ENDDO
      ELSE
       STOP 'GGL90_CALC: Wrong mxlMaxFlag (mixing lenght limit)'
      ENDIF

C- Impose minimum mixing length (to avoid division by zero)
      DO k=2,Nr
       DO J=jMin,jMax
        DO I=iMin,iMax
         GGL90mixingLength(I,J,K) = MAX(GGL90mixingLength(I,J,K),
     &        GGL90mixingLengthMin)
         rMixingLength(I,J,K) = 1. _d 0 /GGL90mixingLength(I,J,K)
        ENDDO
       ENDDO
      ENDDO

      DO k=2,Nr
       Km1 = K-1
       DO J=jMin,jMax
        DO I=iMin,iMax
C     vertical shear term (dU/dz)^2+(dV/dz)^2
         tempU= .5 _d 0*( uVel(I,J,Km1,bi,bj)+uVel(I+1,J,Km1,bi,bj)
     &                 -( uVel(I,J,K  ,bi,bj)+uVel(I+1,J,K  ,bi,bj)) )
     &        *recip_drC(K)
         tempV= .5 _d 0*( vVel(I,J,Km1,bi,bj)+vVel(I,J+1,Km1,bi,bj)
     &                 -( vVel(I,J,K  ,bi,bj)+vVel(I,J+1,K  ,bi,bj)) )
     &        *recip_drC(K)
         verticalShear = tempU*tempU + tempV*tempV
         RiNumber = MAX(Nsquare(i,j,k),0. _d 0)/(verticalShear+GGL90eps)
C     compute Prandtl number (always greater than 0)
         prTemp = 1. _d 0
         IF ( RiNumber .GE. 0.2 _d 0 ) prTemp = 5. _d 0 * RiNumber
         TKEPrandtlNumber(I,J,K) = MIN(10. _d 0,prTemp)

C     viscosity and diffusivity
         KappaM = GGL90ck*GGL90mixingLength(I,J,K)*SQRTTKE(i,j,k)
         KappaH = KappaM/TKEPrandtlNumber(I,J,K)

C     Set a minium (= background) and maximum value
         KappaM = MAX(KappaM,viscArNr(k))
         KappaH = MAX(KappaH,diffKrNrT(k))
         KappaM = MIN(KappaM,GGL90viscMax)
         KappaH = MIN(KappaH,GGL90diffMax)

C     Mask land points and storage
         GGL90viscAr(I,J,K,bi,bj) = KappaM * maskC(I,J,K,bi,bj)
         GGL90diffKr(I,J,K,bi,bj) = KappaH * maskC(I,J,K,bi,bj)
         KappaE(I,J,K) = GGL90alpha * GGL90viscAr(I,J,K,bi,bj)

C     dissipation term
         TKEdissipation = ab05*GGL90ceps
     &        *SQRTTKE(i,j,k)*rMixingLength(I,J,K)
     &        *GGL90TKE(I,J,K,bi,bj)
C     sum up contributions to form the right hand side
         gTKE(I,J,K) = GGL90TKE(I,J,K,bi,bj)
     &        + deltaTggl90*(
     &        + KappaM*verticalShear
     &        - KappaH*Nsquare(i,j,k)
     &        - TKEdissipation
     &        )
        ENDDO
       ENDDO
      ENDDO

C     horizontal diffusion of TKE (requires an exchange in
C      do_fields_blocking_exchanges)
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
         dfx(1-Olx,j)=0. _d 0
         DO i=1-Olx+1,sNx+Olx
          dfx(i,j) = -GGL90diffTKEh*xA(i,j)
     &      *_recip_dxC(i,j,bi,bj)
     &      *(GGL90TKE(i,j,k,bi,bj)-GGL90TKE(i-1,j,k,bi,bj))
     &      *CosFacU(j,bi,bj)
         ENDDO
        ENDDO
C     ... across y-faces
        DO i=1-Olx,sNx+Olx
         dfy(i,1-Oly)=0. _d 0
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
C       end of k-loop
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
       km1=max(2,k-1)
       DO j=jMin,jMax
        DO i=iMin,iMax
         a(i,j,k) = -deltaTggl90
c     &        *recip_drF(km1)*recip_hFacI(i,j,k,bi,bj)
     &        *recip_drF(k-1)*recip_hFacC(i,j,k-1,bi,bj)
     &        *.5 _d 0*(KappaE(i,j, k )+KappaE(i,j,km1))
     &        *recip_drC(k)*maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C--   Upper diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         c(i,j,1)  = 0. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          kp1=min(klowC(i,j,bi,bj),k+1)
          c(i,j,k) = -deltaTggl90
c     &        *recip_drF( k )*recip_hFacI(i,j,k,bi,bj)
     &        *recip_drF( k ) * recip_hFacC(i,j,k,bi,bj)
     &        *.5 _d 0*(KappaE(i,j,k)+KappaE(i,j,kp1))
     &        *recip_drC(k)*maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C--   Center diagonal
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          b(i,j,k) = 1. _d 0 - c(i,j,k) - a(i,j,k)
     &        + ab15*deltaTggl90*GGL90ceps*SQRT(GGL90TKE(I,J,K,bi,bj))
     &        *rMixingLength(I,J,K)*maskC(i,j,k,bi,bj)
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
     &    ( .5 _d 0*( surfaceForcingU(I,  J,  bi,bj)
     &              + surfaceForcingU(I+1,J,  bi,bj) ) )**2
     &  + ( .5 _d 0*( surfaceForcingV(I,  J,  bi,bj)
     &              + surfaceForcingV(I,  J+1,bi,bj) ) )**2
     &                     )
C     Dirichlet surface boundary condition for TKE
        gTKE(I,J,kSurf) = MAX(GGL90TKEsurfMin,GGL90m2*uStarSquare)
     &                     *maskC(I,J,kSurf,bi,bj)
C     Dirichlet bottom boundary condition for TKE = GGL90TKEbottom
        kBottom   = MAX(kLowC(I,J,bi,bj),1)
        gTKE(I,J,kBottom) = gTKE(I,J,kBottom)
     &                    - GGL90TKEbottom*c(I,J,kBottom)
        c(I,J,kBottom) = 0. _d 0
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

C     end of time step
C     ===============================

#ifdef ALLOW_GGL90_SMOOTH
      DO K=1,Nr
       DO J=jMin,jMax
        DO I=iMin,iMax
         tmpdiffKrS=
     &  (
     &   p4 *  GGL90viscAr(i  ,j  ,k,bi,bj) * mskCor(i  ,j  ,bi,bj)
     &  +p8 *( GGL90viscAr(i-1,j  ,k,bi,bj) * mskCor(i-1,j  ,bi,bj)
     &       + GGL90viscAr(i  ,j-1,k,bi,bj) * mskCor(i  ,j-1,bi,bj)
     &       + GGL90viscAr(i+1,j  ,k,bi,bj) * mskCor(i+1,j  ,bi,bj)
     &       + GGL90viscAr(i  ,j+1,k,bi,bj) * mskCor(i  ,j+1,bi,bj))
     &  +p16*( GGL90viscAr(i+1,j+1,k,bi,bj) * mskCor(i+1,j+1,bi,bj)
     &       + GGL90viscAr(i+1,j-1,k,bi,bj) * mskCor(i+1,j-1,bi,bj)
     &       + GGL90viscAr(i-1,j+1,k,bi,bj) * mskCor(i-1,j+1,bi,bj)
     &       + GGL90viscAr(i-1,j-1,k,bi,bj) * mskCor(i-1,j-1,bi,bj))
     &  )
     & /(p4
     &  +p8 *(       maskC(i-1,j  ,k,bi,bj) * mskCor(i-1,j  ,bi,bj)
     &       +       maskC(i  ,j-1,k,bi,bj) * mskCor(i  ,j-1,bi,bj)
     &       +       maskC(i+1,j  ,k,bi,bj) * mskCor(i+1,j  ,bi,bj)
     &       +       maskC(i  ,j+1,k,bi,bj) * mskCor(i  ,j+1,bi,bj))
     &  +p16*(       maskC(i+1,j+1,k,bi,bj) * mskCor(i+1,j+1,bi,bj)
     &       +       maskC(i+1,j-1,k,bi,bj) * mskCor(i+1,j-1,bi,bj)
     &       +       maskC(i-1,j+1,k,bi,bj) * mskCor(i-1,j+1,bi,bj)
     &       +       maskC(i-1,j-1,k,bi,bj) * mskCor(i-1,j-1,bi,bj))
     &  )*maskC(i,j,k,bi,bj)*mskCor(i,j,bi,bj)
     &   /TKEPrandtlNumber(i,j,k)
         GGL90diffKrS(I,J,K,bi,bj)= MAX( tmpdiffKrS , diffKrNrT(k) )
        ENDDO
       ENDDO
      ENDDO
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL( GGL90TKE   ,'GGL90TKE',
     &                          0,Nr, 1, bi, bj, myThid )
         CALL DIAGNOSTICS_FILL( GGL90viscAr,'GGL90Ar ',
     &                          0,Nr, 1, bi, bj, myThid )
         CALL DIAGNOSTICS_FILL( GGL90diffKr,'GGL90Kr ',
     &                          0,Nr, 1, bi, bj, myThid )
         CALL DIAGNOSTICS_FILL( TKEPrandtlNumber ,'GGL90Prl',
     &                          0,Nr, 2, bi, bj, myThid )
         CALL DIAGNOSTICS_FILL( GGL90mixingLength,'GGL90Lmx',
     &                          0,Nr, 2, bi, bj, myThid )
      ENDIF
#endif

#endif /* ALLOW_GGL90 */

      RETURN
      END
