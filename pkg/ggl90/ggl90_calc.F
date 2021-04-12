#include "GGL90_OPTIONS.h"

CBOP
C !ROUTINE: GGL90_CALC

C !INTERFACE: ======================================================
      SUBROUTINE GGL90_CALC(
     I                 bi, bj, sigmaR, myTime, myIter, myThid )

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
C \ev

C !USES: ============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "GGL90.h"

C !INPUT PARAMETERS: ===================================================
C Routine arguments
C     bi, bj :: Current tile indices
C     sigmaR :: Vertical gradient of iso-neutral density
C     myTime :: Current time in simulation
C     myIter :: Current time-step number
C     myThid :: My Thread Id number
      INTEGER bi, bj
      _RL     sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

C !LOCAL VARIABLES: ====================================================
C Local constants
C     iMin,iMax,jMin,jMax :: index boundaries of computation domain
C     i, j, k, kp1,km1 :: array computation indices
C     kSurf, kBottom   :: vertical indices of domain boundaries
C     hFac/hFacI       :: fractional thickness of W-cell
C     explDissFac      :: explicit Dissipation Factor (in [0-1])
C     implDissFac      :: implicit Dissipation Factor (in [0-1])
C     uStarSquare      :: square of friction velocity
C     verticalShear    :: (squared) vertical shear of horizontal velocity
C     Nsquare          :: squared buoyancy freqency
C     RiNumber         :: local Richardson number
C     KappaM           :: (local) viscosity parameter (eq.10)
C     KappaH           :: (local) diffusivity parameter for temperature (eq.11)
C     KappaE           :: (local) diffusivity parameter for TKE (eq.15)
C     TKEdissipation   :: dissipation of TKE
C     GGL90mixingLength:: mixing length of scheme following Banke+Delecuse
C         rMixingLength:: inverse of mixing length
C     totalDepth       :: thickness of water column (inverse of recip_Rcol)
C     TKEPrandtlNumber :: here, an empirical function of the Richardson number
      INTEGER iMin ,iMax ,jMin ,jMax
      INTEGER i, j, k, kp1, km1, kSurf, kBottom
      _RL     explDissFac, implDissFac
      _RL     uStarSquare
      _RL     verticalShear(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     KappaM(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     KappaH
c     _RL     Nsquare
      _RL     Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     deltaTggl90
c     _RL     SQRTTKE
      _RL     SQRTTKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     RiNumber
#ifdef ALLOW_GGL90_IDEMIX
      _RL     IDEMIX_RiNumber
#endif
      _RL     TKEdissipation
      _RL     tempU, tempUp, tempV, tempVp, prTemp
      _RL     MaxLength, tmpmlx, tmpVisc
      _RL     TKEPrandtlNumber (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     GGL90mixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     rMixingLength    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     mxLength_Dn      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     KappaE           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     totalDepth       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     GGL90visctmp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_DIAGNOSTICS
      _RL     surf_flx_tke     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_DIAGNOSTICS */
C     hFac(I)  :: fractional thickness of W-cell
      _RL       hFac
#ifdef ALLOW_GGL90_IDEMIX
      _RL       hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif /* ALLOW_GGL90_IDEMIX */
      _RL recip_hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C-    tri-diagonal matrix
      _RL     a3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     b3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     c3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER errCode
#ifdef ALLOW_GGL90_HORIZDIFF
C     xA, yA   :: area of lateral faces
C     dfx, dfy :: diffusive flux across lateral faces
C     gTKE     :: right hand side of diffusion equation
      _RL     xA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     yA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dfy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL    gTKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_GGL90_HORIZDIFF */
#ifdef ALLOW_GGL90_SMOOTH
      _RL p4, p8, p16
#endif
CEOP

      PARAMETER( iMin = 2-OLx, iMax = sNx+OLx-1 )
      PARAMETER( jMin = 2-OLy, jMax = sNy+OLy-1 )
#ifdef ALLOW_GGL90_SMOOTH
      p4  = 0.25   _d 0
      p8  = 0.125  _d 0
      p16 = 0.0625 _d 0
#endif

C     set separate time step (should be deltaTtracer)
      deltaTggl90 = dTtracerLev(1)

      kSurf = 1
C     explicit/implicit timestepping weights for dissipation
      explDissFac = 0. _d 0
      implDissFac = 1. _d 0 - explDissFac

C     For nonlinear free surface and especially with r*-coordinates, the
C     hFacs change every timestep, so we need to update them here in the
C     case of using IDEMIX.
       DO K=1,Nr
        Km1 = MAX(K-1,1)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          hFac =
     &         MIN(.5 _d 0,_hFacC(i,j,km1,bi,bj) ) +
     &         MIN(.5 _d 0,_hFacC(i,j,k  ,bi,bj) )
          recip_hFacI(I,J,K)=0. _d 0
          IF ( hFac .NE. 0. _d 0 )
     &         recip_hFacI(I,J,K)=1. _d 0/hFac
#ifdef ALLOW_GGL90_IDEMIX
          hFacI(i,j,k) = hFac
#endif /* ALLOW_GGL90_IDEMIX */
         ENDDO
        ENDDO
       ENDDO

C     Initialize local fields
      DO k = 1, Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         rMixingLength(i,j,k)     = 0. _d 0
         mxLength_Dn(i,j,k)       = 0. _d 0
         GGL90visctmp(i,j,k)      = 0. _d 0
         KappaE(i,j,k)            = 0. _d 0
         TKEPrandtlNumber(i,j,k)  = 1. _d 0
         GGL90mixingLength(i,j,k) = GGL90mixingLengthMin
         GGL90visctmp(i,j,k)      = 0. _d 0
#ifndef SOLVE_DIAGONAL_LOWMEMORY
         a3d(i,j,k) = 0. _d 0
         b3d(i,j,k) = 1. _d 0
         c3d(i,j,k) = 0. _d 0
#endif
         Nsquare(i,j,k) = 0. _d 0
         SQRTTKE(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        KappaM(i,j)        = 0. _d 0
        verticalShear(i,j) = 0. _d 0
        totalDepth(i,j)    = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
        rMixingLength(i,j,1) = 0. _d 0
        mxLength_Dn(i,j,1) = GGL90mixingLengthMin
#ifdef ALLOW_AUTODIFF
        IF (maskC(i,j,1,bi,bj).GT.0) THEN
#endif
         SQRTTKE(i,j,1) = SQRT( GGL90TKE(i,j,1,bi,bj) )
#ifdef ALLOW_AUTODIFF
        ELSE
         SQRTTKE(i,j,1) = 0. _d 0
        ENDIF
#endif
#ifdef ALLOW_GGL90_HORIZDIFF
        xA(i,j)  = 0. _d 0
        yA(i,j)  = 0. _d 0
        dfx(i,j) = 0. _d 0
        dfy(i,j) = 0. _d 0
        gTKE(i,j) = 0. _d 0
#endif /* ALLOW_GGL90_HORIZDIFF */
       ENDDO
      ENDDO

#ifdef ALLOW_GGL90_IDEMIX
      IF ( useIDEMIX) CALL GGL90_IDEMIX(
     &   bi, bj, hFacI, recip_hFacI, sigmaR, myTime, myIter, myThid )
#endif /* ALLOW_GGL90_IDEMIX */

      DO k = 2, Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
#ifdef ALLOW_AUTODIFF
         IF (maskC(i,j,k,bi,bj).GT.0) THEN
#endif
          SQRTTKE(i,j,k)=SQRT( GGL90TKE(i,j,k,bi,bj) )
#ifdef ALLOW_AUTODIFF
         ELSE
          SQRTTKE(i,j,k)=0. _d 0
         ENDIF
#endif

C     buoyancy frequency
         Nsquare(i,j,k) = gravity*gravitySign*recip_rhoConst
     &                  * sigmaR(i,j,k)
C     vertical shear term (dU/dz)^2+(dV/dz)^2 is computed later
C     to save some memory
C     mixing length
         GGL90mixingLength(i,j,k) = SQRTTWO *
     &        SQRTTKE(i,j,k)/SQRT( MAX(Nsquare(i,j,k),GGL90eps) )
        ENDDO
       ENDDO
      ENDDO

C- ensure mixing between first and second level
      IF (mxlSurfFlag) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         GGL90mixingLength(i,j,2)=drF(1)
        ENDDO
       ENDDO
      ENDIF

C--   Impose upper and lower bound for mixing length
C--   Impose minimum mixing length to avoid division by zero
      IF ( mxlMaxFlag .EQ. 0 ) THEN

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          MaxLength=totalDepth(i,j)
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &                                   MaxLength)
         ENDDO
        ENDDO
       ENDDO

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MAX(GGL90mixingLength(i,j,k),
     &                                   GGL90mixingLengthMin)
          rMixingLength(i,j,k) = 1. _d 0 / GGL90mixingLength(i,j,k)
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( mxlMaxFlag .EQ. 1 ) THEN

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          MaxLength=MIN(Ro_surf(i,j,bi,bj)-rF(k),rF(k)-R_low(i,j,bi,bj))
c         MaxLength=MAX(MaxLength,20. _d 0)
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &                                   MaxLength)
         ENDDO
        ENDDO
       ENDDO

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MAX(GGL90mixingLength(i,j,k),
     &                                   GGL90mixingLengthMin)
          rMixingLength(i,j,k) = 1. _d 0 / GGL90mixingLength(i,j,k)
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( mxlMaxFlag .EQ. 2 ) THEN

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &        GGL90mixingLength(i,j,k-1)+drF(k-1))
         ENDDO
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
          GGL90mixingLength(i,j,Nr) = MIN(GGL90mixingLength(i,j,Nr),
     &       GGL90mixingLengthMin+drF(Nr))
        ENDDO
       ENDDO
       DO k=Nr-1,2,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &        GGL90mixingLength(i,j,k+1)+drF(k))
         ENDDO
        ENDDO
       ENDDO

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MAX(GGL90mixingLength(i,j,k),
     &                                   GGL90mixingLengthMin)
          rMixingLength(i,j,k) = 1. _d 0 / GGL90mixingLength(i,j,k)
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( mxlMaxFlag .EQ. 3 ) THEN

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          mxLength_Dn(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &        mxLength_Dn(i,j,k-1)+drF(k-1))
         ENDDO
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
          GGL90mixingLength(i,j,Nr) = MIN(GGL90mixingLength(i,j,Nr),
     &       GGL90mixingLengthMin+drF(Nr))
        ENDDO
       ENDDO
       DO k=Nr-1,2,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &        GGL90mixingLength(i,j,k+1)+drF(k))
         ENDDO
        ENDDO
       ENDDO

       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90mixingLength(i,j,k) = MIN(GGL90mixingLength(i,j,k),
     &                                  mxLength_Dn(i,j,k))
          tmpmlx = SQRT( GGL90mixingLength(i,j,k)*mxLength_Dn(i,j,k) )
          tmpmlx = MAX( tmpmlx, GGL90mixingLengthMin)
          rMixingLength(i,j,k) = 1. _d 0 / tmpmlx
         ENDDO
        ENDDO
       ENDDO

      ELSE
       STOP 'GGL90_CALC: Wrong mxlMaxFlag (mixing length limit)'
      ENDIF

C     start "proper" k-loop (the code above was moved out and up to
C     implemement various mixing parameters efficiently)
      DO k=2,Nr
       km1 = k-1

#ifdef ALLOW_GGL90_HORIZDIFF
      IF ( GGL90diffTKEh .GT. 0. _d 0 ) THEN
C     horizontal diffusion of TKE (requires an exchange in
C      do_fields_blocking_exchanges)
C     common factors
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j) = _dyG(i,j,bi,bj)*drC(k)*
     &                 (min(.5 _d 0,_hFacW(i,j,k-1,bi,bj) ) +
     &                  min(.5 _d 0,_hFacW(i,j,k  ,bi,bj) ) )
          yA(i,j) = _dxG(i,j,bi,bj)*drC(k)*
     &                 (min(.5 _d 0,_hFacS(i,j,k-1,bi,bj) ) +
     &                  min(.5 _d 0,_hFacS(i,j,k  ,bi,bj) ) )
         ENDDO
        ENDDO
C     Compute diffusive fluxes
C     ... across x-faces
        DO j=1-OLy,sNy+OLy
         dfx(1-OLx,j)=0. _d 0
         DO i=1-OLx+1,sNx+OLx
          dfx(i,j) = -GGL90diffTKEh*xA(i,j)
     &      *_recip_dxC(i,j,bi,bj)
     &      *(GGL90TKE(i,j,k,bi,bj)-GGL90TKE(i-1,j,k,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &      *CosFacU(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */
         ENDDO
        ENDDO
C     ... across y-faces
        DO i=1-OLx,sNx+OLx
         dfy(i,1-OLy)=0. _d 0
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dfy(i,j) = -GGL90diffTKEh*yA(i,j)
     &      *_recip_dyC(i,j,bi,bj)
     &      *(GGL90TKE(i,j,k,bi,bj)-GGL90TKE(i,j-1,k,bi,bj))
#ifdef ISOTROPIC_COS_SCALING
     &      *CosFacV(j,bi,bj)
#endif /* ISOTROPIC_COS_SCALING */
         ENDDO
        ENDDO
C     Compute divergence of fluxes
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          gTKE(i,j) = -recip_drC(k)*recip_rA(i,j,bi,bj)
     &         *recip_hFacI(i,j,k)
     &         *((dfx(i+1,j)-dfx(i,j))
     &         + (dfy(i,j+1)-dfy(i,j)) )
         ENDDO
        ENDDO
C     end if GGL90diffTKEh .eq. 0.
       ENDIF
#endif /* ALLOW_GGL90_HORIZDIFF */

C     viscosity and diffusivity
       DO j=jMin,jMax
        DO i=iMin,iMax
         KappaM(i,j) = GGL90ck*GGL90mixingLength(i,j,k)*SQRTTKE(i,j,k)
         GGL90visctmp(i,j,k) = MAX(KappaM(i,j),diffKrNrS(k))
     &                            * maskC(i,j,k,bi,bj)
C        note: storing GGL90visctmp like this, and using it later to compute
C              GGL9rdiffKr etc. is robust in case of smoothing (e.g. see OPA)
         KappaM(i,j) = MAX(KappaM(i,j),viscArNr(k)) * maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO

C     compute vertical shear (dU/dz)^2+(dV/dz)^2
       IF ( calcMeanVertShear ) THEN
C     by averaging (@ grid-cell center) the 4 vertical shear compon @ U,V pos.
        DO j=jMin,jMax
         DO i=iMin,iMax
          tempU  = ( uVel( i ,j,km1,bi,bj) - uVel( i ,j,k,bi,bj) )
          tempUp = ( uVel(i+1,j,km1,bi,bj) - uVel(i+1,j,k,bi,bj) )
          tempV  = ( vVel(i, j ,km1,bi,bj) - vVel(i, j ,k,bi,bj) )
          tempVp = ( vVel(i,j+1,km1,bi,bj) - vVel(i,j+1,k,bi,bj) )
          verticalShear(i,j) = (
     &                 ( tempU*tempU + tempUp*tempUp )*halfRL
     &               + ( tempV*tempV + tempVp*tempVp )*halfRL
     &                         )*recip_drC(k)*recip_drC(k)
         ENDDO
        ENDDO
       ELSE
C     from the averaged flow at grid-cell center (2 compon x 2 pos.)
        DO j=jMin,jMax
         DO i=iMin,iMax
          tempU = ( ( uVel(i,j,km1,bi,bj) + uVel(i+1,j,km1,bi,bj) )
     &             -( uVel(i,j,k  ,bi,bj) + uVel(i+1,j,k  ,bi,bj) )
     &            )*halfRL*recip_drC(k)
          tempV = ( ( vVel(i,j,km1,bi,bj) + vVel(i,j+1,km1,bi,bj) )
     &             -( vVel(i,j,k  ,bi,bj) + vVel(i,j+1,k  ,bi,bj) )
     &            )*halfRL*recip_drC(k)
          verticalShear(i,j) = tempU*tempU + tempV*tempV
         ENDDO
        ENDDO
       ENDIF

C     compute Prandtl number (always greater than 0)
#ifdef ALLOW_GGL90_IDEMIX
       IF ( useIDEMIX ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
C     account for partical cell factor in vertical shear:
          verticalShear(i,j) = verticalShear(i,j)
     &                       * recip_hFacI(i,j,k)*recip_hFacI(i,j,k)
          RiNumber = MAX(Nsquare(i,j,k),0. _d 0)
     &         /(verticalShear(i,j)+GGL90eps)
CML         IDEMIX_RiNumber = 1./GGL90eps
          IDEMIX_RiNumber = MAX( KappaM(i,j)*Nsquare(i,j,k), 0. _d 0)/
     &     (GGL90eps+IDEMIX_tau_d(i,j,k,bi,bj)*IDEMIX_E(i,j,k,bi,bj)**2)
          prTemp         = MIN(5.*RiNumber, 6.6 _d 0*IDEMIX_RiNumber)
          TKEPrandtlNumber(i,j,k) = MIN(10. _d 0,prTemp)
          TKEPrandtlNumber(i,j,k) = MAX( 1. _d 0,TKEPrandtlNumber(i,j,k))
         ENDDO
        ENDDO
       ELSE
#endif /* ALLOW_GGL90_IDEMIX */
        DO j=jMin,jMax
         DO i=iMin,iMax
          RiNumber = MAX(Nsquare(i,j,k),0. _d 0)
     &         /(verticalShear(i,j)+GGL90eps)
          prTemp = 1. _d 0
          IF ( RiNumber .GE. 0.2 _d 0 ) prTemp = 5. _d 0 * RiNumber
          TKEPrandtlNumber(i,j,k) = MIN(10. _d 0,prTemp)
         ENDDO
        ENDDO
#ifdef ALLOW_GGL90_IDEMIX
       ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

       DO j=jMin,jMax
        DO i=iMin,iMax
C        diffusivity
         KappaH = KappaM(i,j)/TKEPrandtlNumber(i,j,k)
         KappaE(i,j,k) = GGL90alpha * KappaM(i,j) * maskC(i,j,k,bi,bj)

C     dissipation term
         TKEdissipation = explDissFac*GGL90ceps
     &        *SQRTTKE(i,j,k)*rMixingLength(i,j,k)
     &        *GGL90TKE(i,j,k,bi,bj)
C     partial update with sum of explicit contributions
         GGL90TKE(i,j,k,bi,bj) = GGL90TKE(i,j,k,bi,bj)
     &        + deltaTggl90*(
     &        + KappaM(i,j)*verticalShear(i,j)
     &        - KappaH*Nsquare(i,j,k)
     &        - TKEdissipation
     &        )
        ENDDO
       ENDDO

#ifdef ALLOW_GGL90_IDEMIX
       IF ( useIDEMIX ) THEN
C     add IDEMIX contribution to the turbulent kinetic energy
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90TKE(i,j,k,bi,bj) = GGL90TKE(i,j,k,bi,bj)
     &         + deltaTggl90*(
     &         + IDEMIX_tau_d(i,j,k,bi,bj)*IDEMIX_E(i,j,k,bi,bj)**2
     &         )
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

#ifdef ALLOW_GGL90_HORIZDIFF
       IF ( GGL90diffTKEh .GT. 0. _d 0 ) THEN
C--    Add horiz. diffusion tendency
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90TKE(i,j,k,bi,bj) = GGL90TKE(i,j,k,bi,bj)
     &                          + gTKE(i,j)*deltaTggl90
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_GGL90_HORIZDIFF */

C--   end of k loop
      ENDDO

C     ============================================
C     Implicit time step to update TKE for k=1,Nr;
C     TKE(Nr+1)=0 by default
C     ============================================
C     set up matrix
C--   Lower diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         a3d(i,j,1) = 0. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
       km1=MAX(2,k-1)
       DO j=jMin,jMax
        DO i=iMin,iMax
C-    We keep recip_hFacC in the diffusive flux calculation,
C-    but no hFacC in TKE volume control
C-    No need for maskC(k-1) with recip_hFacC(k-1)
         a3d(i,j,k) = -deltaTggl90
     &        *recip_drF(k-1)*recip_hFacC(i,j,k-1,bi,bj)
     &        *.5 _d 0*(KappaE(i,j, k )+KappaE(i,j,km1))
     &        *recip_drC(k)*maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C--   Upper diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         c3d(i,j,1)  = 0. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         kp1=MAX(1,MIN(klowC(i,j,bi,bj),k+1))
C-    We keep recip_hFacC in the diffusive flux calculation,
C-    but no hFacC in TKE volume control
C-    No need for maskC(k) with recip_hFacC(k)
         c3d(i,j,k) = -deltaTggl90
     &        *recip_drF( k ) * recip_hFacC(i,j,k,bi,bj)
     &        *.5 _d 0*(KappaE(i,j,k)+KappaE(i,j,kp1))
     &        *recip_drC(k)*maskC(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_GGL90_IDEMIX
      IF ( useIDEMIX ) THEN
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          a3d(i,j,k) = a3d(i,j,k)*recip_hFacI(i,j,k)
          c3d(i,j,k) = c3d(i,j,k)*recip_hFacI(i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

      IF (.NOT.GGL90_dirichlet) THEN
C      Neumann bottom boundary condition for TKE: no flux from bottom
       DO j=jMin,jMax
        DO i=iMin,iMax
         kBottom   = MAX(kLowC(i,j,bi,bj),1)
         c3d(i,j,kBottom) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

C--   Center diagonal
      DO k=1,Nr
       km1 = MAX(k-1,1)
       DO j=jMin,jMax
        DO i=iMin,iMax
         b3d(i,j,k) = 1. _d 0 - c3d(i,j,k) - a3d(i,j,k)
     &        + implDissFac*deltaTggl90*GGL90ceps*SQRTTKE(i,j,k)
     &        * rMixingLength(i,j,k)
     &        * maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C     end set up matrix

C     Apply boundary condition
      kp1 = MIN(Nr,kSurf+1)
      DO j=jMin,jMax
       DO i=iMin,iMax
C     estimate friction velocity uStar from surface forcing
        uStarSquare = SQRT(
     &    ( .5 _d 0*( surfaceForcingU(i,  j,  bi,bj)
     &              + surfaceForcingU(i+1,j,  bi,bj) ) )**2
     &  + ( .5 _d 0*( surfaceForcingV(i,  j,  bi,bj)
     &              + surfaceForcingV(i,  j+1,bi,bj) ) )**2
     &                     )
C     Dirichlet surface boundary condition for TKE
        GGL90TKE(i,j,kSurf,bi,bj) = maskC(i,j,kSurf,bi,bj)
     &           *MAX(GGL90TKEsurfMin,GGL90m2*uStarSquare)
        GGL90TKE(i,j,kp1,bi,bj) = GGL90TKE(i,j,kp1,bi,bj)
     &               - a3d(i,j,kp1)*GGL90TKE(i,j,kSurf,bi,bj)
        a3d(i,j,kp1) = 0. _d 0
       ENDDO
      ENDDO

      IF (GGL90_dirichlet) THEN
C      Dirichlet bottom boundary condition for TKE = GGL90TKEbottom
       DO j=jMin,jMax
        DO i=iMin,iMax
         kBottom   = MAX(kLowC(i,j,bi,bj),1)
         GGL90TKE(i,j,kBottom,bi,bj) = GGL90TKE(i,j,kBottom,bi,bj)
     &                              - GGL90TKEbottom*c3d(i,j,kBottom)
         c3d(i,j,kBottom) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

C     solve tri-diagonal system
      errCode = -1
      CALL SOLVE_TRIDIAGONAL( iMin,iMax, jMin,jMax,
     I                        a3d, b3d, c3d,
     U                        GGL90TKE(1-OLx,1-OLy,1,bi,bj),
     O                        errCode,
     I                        bi, bj, myThid )

      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
C     impose minimum TKE to avoid numerical undershoots below zero
         GGL90TKE(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
     &                  *MAX( GGL90TKE(i,j,k,bi,bj), GGL90TKEmin )
        ENDDO
       ENDDO
      ENDDO

C     end of time step
C     ===============================

      DO k=2,Nr
       DO j=1,sNy
        DO i=1,sNx
#ifdef ALLOW_GGL90_SMOOTH
         tmpVisc = (
     &     p4 *    GGL90visctmp(i  ,j  ,k)*mskCor(i  ,j  ,bi,bj)
     &    +p8 *( ( GGL90visctmp(i-1,j  ,k)*mskCor(i-1,j  ,bi,bj)
     &           + GGL90visctmp(i+1,j  ,k)*mskCor(i+1,j  ,bi,bj) )
     &         + ( GGL90visctmp(i  ,j-1,k)*mskCor(i  ,j-1,bi,bj)
     &           + GGL90visctmp(i  ,j+1,k)*mskCor(i  ,j+1,bi,bj) ) )
     &    +p16*( ( GGL90visctmp(i+1,j+1,k)*mskCor(i+1,j+1,bi,bj)
     &           + GGL90visctmp(i-1,j-1,k)*mskCor(i-1,j-1,bi,bj) )
     &         + ( GGL90visctmp(i+1,j-1,k)*mskCor(i+1,j-1,bi,bj)
     &           + GGL90visctmp(i-1,j+1,k)*mskCor(i-1,j+1,bi,bj) ) )
     &             )/(
     &     p4
     &    +p8 *( (  maskC(i-1,j  ,k,bi,bj)*mskCor(i-1,j  ,bi,bj)
     &           +  maskC(i+1,j  ,k,bi,bj)*mskCor(i+1,j  ,bi,bj) )
     &         + (  maskC(i  ,j-1,k,bi,bj)*mskCor(i  ,j-1,bi,bj)
     &           +  maskC(i  ,j+1,k,bi,bj)*mskCor(i  ,j+1,bi,bj) ) )
     &    +p16*( (  maskC(i+1,j+1,k,bi,bj)* mskCor(i+1,j+1,bi,bj)
     &           +  maskC(i-1,j-1,k,bi,bj)*mskCor(i-1,j-1,bi,bj) )
     &         + (  maskC(i+1,j-1,k,bi,bj)*mskCor(i+1,j-1,bi,bj)
     &           +  maskC(i-1,j+1,k,bi,bj)*mskCor(i-1,j+1,bi,bj) ) )
     &               )*maskC(i,j,k,bi,bj)*mskCor(i,j,bi,bj)
#else
         tmpVisc = GGL90visctmp(i,j,k)
#endif
         tmpVisc = MIN(tmpVisc/TKEPrandtlNumber(i,j,k),GGL90diffMax)
         GGL90diffKr(i,j,k,bi,bj)= MAX( tmpVisc , diffKrNrS(k) )
        ENDDO
       ENDDO
      ENDDO

      DO k=2,Nr
       DO j=1,sNy
        DO i=1,sNx+1
#ifdef ALLOW_GGL90_SMOOTH
         tmpVisc = (
     &     p4 *(   GGL90visctmp(i-1,j  ,k)*mskCor(i-1,j  ,bi,bj)
     &           + GGL90visctmp(i  ,j  ,k)*mskCor(i  ,j  ,bi,bj) )
     &    +p8 *( ( GGL90visctmp(i-1,j-1,k)*mskCor(i-1,j-1,bi,bj)
     &           + GGL90visctmp(i  ,j-1,k)*mskCor(i  ,j-1,bi,bj) )
     &         + ( GGL90visctmp(i-1,j+1,k)*mskCor(i-1,j+1,bi,bj)
     &           + GGL90visctmp(i  ,j+1,k)*mskCor(i  ,j+1,bi,bj) ) )
     &             )/(
     &     p4 * 2. _d 0
     &    +p8 *( (  maskC(i-1,j-1,k,bi,bj)*mskCor(i-1,j-1,bi,bj)
     &           +  maskC(i  ,j-1,k,bi,bj)*mskCor(i  ,j-1,bi,bj) )
     &         + (  maskC(i-1,j+1,k,bi,bj)*mskCor(i-1,j+1,bi,bj)
     &           +  maskC(i  ,j+1,k,bi,bj)*mskCor(i  ,j+1,bi,bj) ) )
     &               )*maskC(i-1,j,k,bi,bj)*mskCor(i-1,j,bi,bj)
     &                *maskC(i  ,j,k,bi,bj)*mskCor(i  ,j,bi,bj)
#else
         tmpVisc = _maskW(i,j,k,bi,bj) * halfRL
     &          *( GGL90visctmp(i-1,j,k)
     &           + GGL90visctmp(i,j,k) )
#endif
         tmpVisc = MIN( tmpVisc , GGL90viscMax )
         GGL90viscArU(i,j,k,bi,bj) = MAX( tmpVisc, viscArNr(k) )
        ENDDO
       ENDDO
      ENDDO

      DO k=2,Nr
       DO j=1,sNy+1
        DO i=1,sNx
#ifdef ALLOW_GGL90_SMOOTH
         tmpVisc = (
     &     p4 *(   GGL90visctmp(i  ,j-1,k)*mskCor(i  ,j-1,bi,bj)
     &           + GGL90visctmp(i  ,j  ,k)*mskCor(i  ,j  ,bi,bj) )
     &    +p8 *( ( GGL90visctmp(i-1,j-1,k)*mskCor(i-1,j-1,bi,bj)
     &           + GGL90visctmp(i-1,j  ,k)*mskCor(i-1,j  ,bi,bj) )
     &         + ( GGL90visctmp(i+1,j-1,k)*mskCor(i+1,j-1,bi,bj)
     &           + GGL90visctmp(i+1,j  ,k)*mskCor(i+1,j  ,bi,bj) ) )
     &             )/(
     &     p4 * 2. _d 0
     &    +p8 *( (  maskC(i-1,j-1,k,bi,bj)*mskCor(i-1,j-1,bi,bj)
     &           +  maskC(i-1,j  ,k,bi,bj)*mskCor(i-1,j  ,bi,bj) )
     &         + (  maskC(i+1,j-1,k,bi,bj)*mskCor(i+1,j-1,bi,bj)
     &           +  maskC(i+1,j  ,k,bi,bj)*mskCor(i+1,j  ,bi,bj) ) )
     &               )*maskC(i,j-1,k,bi,bj)*mskCor(i,j-1,bi,bj)
     &                *maskC(i,j  ,k,bi,bj)*mskCor(i,j  ,bi,bj)
#else
         tmpVisc = _maskS(i,j,k,bi,bj) * halfRL
     &          *( GGL90visctmp(i,j-1,k)
     &           + GGL90visctmp(i,j,k) )
#endif
         tmpVisc = MIN( tmpVisc , GGL90viscMax )
         GGL90viscArV(i,j,k,bi,bj) = MAX( tmpVisc, viscArNr(k) )
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( GGL90TKE   ,'GGL90TKE',
     &                         0,Nr, 1, bi, bj, myThid )
        CALL DIAGNOSTICS_FILL( GGL90viscArU,'GGL90ArU',
     &                         0,Nr, 1, bi, bj, myThid )
        CALL DIAGNOSTICS_FILL( GGL90viscArV,'GGL90ArV',
     &                         0,Nr, 1, bi, bj, myThid )
        CALL DIAGNOSTICS_FILL( GGL90diffKr,'GGL90Kr ',
     &                         0,Nr, 1, bi, bj, myThid )
        CALL DIAGNOSTICS_FILL( TKEPrandtlNumber ,'GGL90Prl',
     &                         0,Nr, 2, bi, bj, myThid )
        CALL DIAGNOSTICS_FILL( GGL90mixingLength,'GGL90Lmx',
     &                         0,Nr, 2, bi, bj, myThid )

        kp1 = MIN(Nr,kSurf+1)
        DO j=jMin,jMax
         DO i=iMin,iMax
C     diagnose surface flux of TKE
          surf_flx_tke(i,j) =(GGL90TKE(i,j,kSurf,bi,bj)-
     &                        GGL90TKE(i,j,kp1,bi,bj))
     &        *recip_drF(kSurf)*recip_hFacC(i,j,kSurf,bi,bj)
     &        *KappaE(i,j,kp1)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL( surf_flx_tke,'GGL90flx',
     &                         0, 1, 2, bi, bj, myThid )

        k=kSurf
        DO j=jMin,jMax
         DO i=iMin,iMax
C     diagnose work done by the wind
          surf_flx_tke(i,j) =
     &      halfRL*( surfaceForcingU(i,  j,bi,bj)*uVel(i  ,j,k,bi,bj)
     &              +surfaceForcingU(i+1,j,bi,bj)*uVel(i+1,j,k,bi,bj))
     &    + halfRL*( surfaceForcingV(i,j,  bi,bj)*vVel(i,j  ,k,bi,bj)
     &              +surfaceForcingV(i,j+1,bi,bj)*vVel(i,j+1,k,bi,bj))
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL( surf_flx_tke,'GGL90tau',
     &                         0, 1, 2, bi, bj, myThid )

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GGL90 */

      RETURN
      END
