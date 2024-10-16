#include "GGL90_OPTIONS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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
C     |  Gaspar et al. (1990), JGR 95 (C9), pp 16,179            |
C     | Some parts of the implementation follow Blanke and       |
C     |  Delecuse (1993), JPO, and OPA code, in particular the   |
C     |  computation of the                                      |
C     |  mixing length = max(min(lk,depth),lkmin)                |
C     | Note: Only call this S/R if Nr > 1 (no use if Nr=1)      |
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
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

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

#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C !LOCAL VARIABLES: ====================================================
C     iMin,iMax,jMin,jMax :: index boundaries of computation domain
C     i, j, k          :: array computation indices
C     kSrf             :: vertical index of surface level
C     kTop             :: index of top interface (just below surf. level)
C     kBot             :: index of bottom interface (just above bottom lev.)
C     hFac/hFacI       :: fractional thickness of W-cell
C     explDissFac      :: explicit Dissipation Factor (in [0-1])
C     implDissFac      :: implicit Dissipation Factor (in [0-1])
C
C     In general, all 3D variables are defined at W-points (i.e.,
C     between k and k-1), all 2D variables are also defined at W-points
C     or at the very surface level (like uStarSquare)
C
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
C     TKEPrandtlNumber :: here, an empirical function of the Richardson number
      INTEGER iMin ,iMax ,jMin ,jMax
      INTEGER i, j, k
      INTEGER kp1, km1
      INTEGER kSrf, kTop, kBot
      INTEGER errCode
      _RL deltaTloc
      _RL explDissFac, implDissFac
      _RL uStarSquare  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL verticalShear(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaM       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaH
c     _RL Nsquare
      _RL Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
c     _RL SQRTTKE
      _RL SQRTTKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL RiNumber
#ifdef ALLOW_GGL90_IDEMIX
      _RL IDEMIX_RiNumber
#endif
      _RL TKEdissipation
      _RL tempU, tempUp, tempV, tempVp, prTemp, tmpVisc
      _RL TKEPrandtlNumber (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL GGL90mixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rMixingLength    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaE           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL GGL90visctmp     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_GGL90_IDEMIX
      _RL hFacI            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     IDEMIX_gTKE :: dissipation of internal wave energy is a source
C                    of TKE and mixing (output of S/R GGL90_IDEMIX)
      _RL IDEMIX_gTKE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif /* ALLOW_GGL90_IDEMIX */
#ifdef ALLOW_GGL90_LANGMUIR
C   uStar, vStar :: frictional velocity component
      _RL uStar, vStar, depthFac
      _RL recip_Lasq, recip_LD
      _RL LCmixingLength(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL stokesterm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dstokesUdR(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dstokesVdR(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_GGL90_LANGMUIR */
      _RL recip_hFacI      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL hFac
      _RS mskLoc
#ifdef ALLOW_GGL90_SMOOTH
      _RS maskI            (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
C-    tri-diagonal matrix
      _RL a3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     This mixed layer model is not invariant under coordinate
C     transformation to pressure coordinates, so we need these
C     factors to scale the vertical (pressure) coordinates
      _RL coordFac, recip_coordFac
#ifdef ALLOW_GGL90_HORIZDIFF
C     xA, yA   :: area of lateral faces
C     dfx, dfy :: diffusive flux across lateral faces
C     gTKE     :: right hand side of diffusion equation
      _RL xA  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL yA  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfx (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gTKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_GGL90_HORIZDIFF */
#ifdef ALLOW_GGL90_SMOOTH
      _RL p4, p8, p16
#endif
#ifdef ALLOW_SHELFICE
      INTEGER ki
      _RL KE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cDragU (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cDragV (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stressU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stressV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kappaRX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
#endif
#ifdef ALLOW_DIAGNOSTICS
# ifndef ALLOW_AUTODIFF
      LOGICAL doDiagTKEmin
      _RL recip_deltaT
# endif
      _RL surf_flx_tke(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (depends on tiles)
C     kkey :: tape key (depends on levels and tiles)
      INTEGER tkey, kkey
#endif
CEOP

      PARAMETER( iMin = 2-OLx, iMax = sNx+OLx-1 )
      PARAMETER( jMin = 2-OLy, jMax = sNy+OLy-1 )
#ifdef ALLOW_GGL90_SMOOTH
      p4  = 0.25   _d 0
      p8  = 0.125  _d 0
      p16 = 0.0625 _d 0
#endif

      IF ( usingPCoords ) THEN
       kSrf = Nr
       kTop = Nr
      ELSE
       kSrf =  1
       kTop =  2
      ENDIF
      deltaTloc = dTtracerLev(kSrf)

      coordFac = 1. _d 0
      IF ( usingPCoords) coordFac = gravity * rhoConst
      recip_coordFac = 1./coordFac

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

C     explicit/implicit timestepping weights for dissipation
      explDissFac = 0. _d 0
      implDissFac = 1. _d 0 - explDissFac

#ifdef ALLOW_DIAGNOSTICS
# ifndef ALLOW_AUTODIFF
      doDiagTKEmin = .FALSE.
# endif
      IF ( useDiagnostics ) THEN
# ifndef ALLOW_AUTODIFF
       doDiagTKEmin = DIAGNOSTICS_IS_ON('GGL90Emn',myThid)
C- note: needs to explicitly increment the counter since DIAGNOSTICS_FILL
C        does it only if k=1 (never the case here)
       IF ( doDiagTKEmin )
     &      CALL DIAGNOSTICS_COUNT('GGL90Emn',bi,bj,myThid)
# endif
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         surf_flx_tke(i,j) = 0.
        ENDDO
       ENDDO
      ENDIF
#endif

C     For nonlinear free surface and especially with r*-coordinates, the
C     hFacs change every timestep, so we need to update them here in the
C     case of using IDEMIX.
       DO k=1,Nr
        km1 = MAX(k-1,1)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          hFac =
     &         MIN( halfRS, _hFacC(i,j,km1,bi,bj) )
     &       + MIN( halfRS, _hFacC(i,j,k  ,bi,bj) )
          recip_hFacI(i,j,k)=0. _d 0
          IF ( hFac .NE. 0. _d 0 )
     &         recip_hFacI(i,j,k)=1. _d 0/hFac
#ifdef ALLOW_GGL90_IDEMIX
          hFacI(i,j,k) = hFac
#endif /* ALLOW_GGL90_IDEMIX */
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_GGL90_IDEMIX
C     step forward IDEMIX_E(energy) and compute tendency for TKE,
C     IDEMIX_gTKE = tau_d * IDEMIX_E**2, following Olbers and Eden (2013)
      IF ( useIDEMIX) CALL GGL90_IDEMIX(
     I     bi, bj, hFacI, recip_hFacI, sigmaR,
     O     IDEMIX_gTKE,
     I     myTime, myIter, myThid )
#endif /* ALLOW_GGL90_IDEMIX */

C     Initialize local fields
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         rMixingLength(i,j,k)     = 0. _d 0
         GGL90visctmp(i,j,k)      = 0. _d 0
         KappaE(i,j,k)            = 0. _d 0
         TKEPrandtlNumber(i,j,k)  = 1. _d 0
         GGL90mixingLength(i,j,k) = GGL90mixingLengthMin
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90TKE(:,:,:,bi,bj)=comlev1_bibj, key=tkey, kind=isbyte
#endif
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        KappaM(i,j)        = 0. _d 0
        uStarSquare(i,j)   = 0. _d 0
        verticalShear(i,j) = 0. _d 0
c       rMixingLength(i,j,1)  = 0. _d 0
#ifdef ALLOW_AUTODIFF
        IF ( usingZCoords .AND. maskC(i,j,1,bi,bj).EQ.oneRS
     &       .AND. GGL90TKE(i,j,1,bi,bj) .GT. zeroRL ) THEN
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

#ifdef ALLOW_GGL90_LANGMUIR
      IF (useLANGMUIR) THEN
       recip_Lasq = 1. _d 0 / LC_num
       recip_Lasq = recip_Lasq * recip_Lasq
       recip_LD   = 4. _d 0 * PI / LC_lambda
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         stokesterm(i,j) = 0. _d 0
         dstokesUdR(i,j) = 0. _d 0
         dstokesVdR(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF
#endif

      DO k = 2, Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         mskLoc = maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
#ifdef ALLOW_AUTODIFF
         IF ( mskLoc.EQ.oneRS
     &        .AND. GGL90TKE(i,j,k,bi,bj) .GT. zeroRL ) THEN
#endif
          SQRTTKE(i,j,k)=SQRT( GGL90TKE(i,j,k,bi,bj) )
#ifdef ALLOW_AUTODIFF
         ELSE
          SQRTTKE(i,j,k)=0. _d 0
         ENDIF
#endif

C     buoyancy frequency
         Nsquare(i,j,k) = gravity*gravitySign*recip_rhoConst
     &                  * sigmaR(i,j,k) * coordFac
C     vertical shear term (dU/dz)^2+(dV/dz)^2 is computed later
C     to save some memory
C     Initialise mixing length (eq. 2.35)
         GGL90mixingLength(i,j,k) = SQRTTWO *
     &        SQRTTKE(i,j,k)/SQRT( MAX(Nsquare(i,j,k),GGL90eps) )
     &        * mskLoc
        ENDDO
       ENDDO
      ENDDO

      CALL GGL90_MIXINGLENGTH(
     U     GGL90mixingLength,
#ifdef ALLOW_GGL90_LANGMUIR
     O     LCmixingLength,
#endif
     O     rMixingLength,
     I     iMin ,iMax ,jMin ,jMax,
     I     bi, bj, myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90mixingLength = comlev1_bibj, key=tkey, kind=isbyte
CADJ STORE rMixingLength     = comlev1_bibj, key=tkey, kind=isbyte
# ifdef ALLOW_GGL90_LANGMUIR
CADJ STORE LCmixingLength    = comlev1_bibj, key=tkey, kind=isbyte
# endif
#endif

C     start "proper" k-loop
      DO k=2,Nr
       km1 = k-1
#ifdef ALLOW_AUTODIFF_TAMC
       kkey = k + (tkey-1)*Nr
#endif
#ifdef ALLOW_GGL90_HORIZDIFF
       IF ( GGL90diffTKEh .GT. 0. _d 0 ) THEN
C     horizontal diffusion of TKE (requires an exchange in
C      do_fields_blocking_exchanges)
C     common factors
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j) = _dyG(i,j,bi,bj)*drC(k)*
     &                 (MIN(.5 _d 0,_hFacW(i,j,km1,bi,bj) ) +
     &                  MIN(.5 _d 0,_hFacW(i,j,k  ,bi,bj) ) )
          yA(i,j) = _dxG(i,j,bi,bj)*drC(k)*
     &                 (MIN(.5 _d 0,_hFacS(i,j,km1,bi,bj) ) +
     &                  MIN(.5 _d 0,_hFacS(i,j,k  ,bi,bj) ) )
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
#ifdef ALLOW_GGL90_LANGMUIR
       IF (useLANGMUIR) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          KappaM(i,j) = GGL90ck*LCmixingLength(i,j,k)*SQRTTKE(i,j,k)
         ENDDO
        ENDDO
       ELSE
#endif
        DO j=jMin,jMax
         DO i=iMin,iMax
          KappaM(i,j) = GGL90ck*GGL90mixingLength(i,j,k)*SQRTTKE(i,j,k)
#ifdef ALLOW_GGL90_LANGMUIR
         ENDDO
        ENDDO
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
#endif
         GGL90visctmp(i,j,k) = MAX( KappaM(i,j),diffKrNrS(k)
     &                            * recip_coordFac*recip_coordFac )
     &        * maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
C        note: storing GGL90visctmp like this, and using it later to compute
C              GGL9rdiffKr etc. is robust in case of smoothing (e.g. see OPA)
         KappaM(i,j) = MAX( KappaM(i,j),viscArNr(k)
     &                    * recip_coordFac*recip_coordFac )
     &                    * maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
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
     &                 ( tempU*tempU + tempUp*tempUp )
     &               + ( tempV*tempV + tempVp*tempVp )
     &                         )*halfRL*recip_drC(k)*recip_drC(k)
     &                          *coordFac*coordFac
         ENDDO
        ENDDO
       ELSE
C     from the averaged flow at grid-cell center (2 compon x 2 pos.)
        DO j=jMin,jMax
         DO i=iMin,iMax
          tempU = ( ( uVel(i,j,km1,bi,bj) + uVel(i+1,j,km1,bi,bj) )
     &             -( uVel(i,j,k  ,bi,bj) + uVel(i+1,j,k  ,bi,bj) )
     &            )*halfRL*recip_drC(k)
     &             *coordFac
          tempV = ( ( vVel(i,j,km1,bi,bj) + vVel(i,j+1,km1,bi,bj) )
     &             -( vVel(i,j,k  ,bi,bj) + vVel(i,j+1,k  ,bi,bj) )
     &            )*halfRL*recip_drC(k)
     &             *coordFac
          verticalShear(i,j) = tempU*tempU + tempV*tempV
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaM        = comlev1_bibj_k, key = kkey, kind=isbyte
CADJ STORE verticalShear = comlev1_bibj_k, key = kkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_GGL90_LANGMUIR
       IF (useLANGMUIR) THEN
C       compute (dStokesU/dz) and (dStokesV/dz)
        depthFac = recip_Lasq*EXP( recip_LD*rF(k) )
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uStar = SIGN( SQRT(ABS(surfaceForcingU(i,j,bi,bj))),
     &                  surfaceForcingU(i,j,bi,bj) )
          dstokesUdR(i,j) = recip_LD * uStar * depthFac
          vStar = SIGN( SQRT(ABS(surfaceForcingV(i,j,bi,bj))),
     &                  surfaceForcingV(i,j,bi,bj) )
          dstokesVdR(i,j) = recip_LD * vStar * depthFac
         ENDDO
        ENDDO

        IF ( calcMeanVertShear ) THEN
C     by averaging (@ grid-cell center) the 4 vertical shear compon @
C     U,V pos.
         DO j=jMin,jMax
          DO i=iMin,iMax
           tempU  = ( uVel( i ,j,km1,bi,bj) - uVel( i ,j,k,bi,bj) )
           tempUp = ( uVel(i+1,j,km1,bi,bj) - uVel(i+1,j,k,bi,bj) )
           tempV  = ( vVel(i, j ,km1,bi,bj) - vVel(i, j ,k,bi,bj) )
           tempVp = ( vVel(i,j+1,km1,bi,bj) - vVel(i,j+1,k,bi,bj) )
           stokesterm(i,j) = (
     &              ( tempU *dstokesUdR(i,j)
     &               +tempUp*dstokesUdR(i+1,j) )
     &             +( tempV *dstokesVdR(i,j)
     &               +tempVp*dstokesVdR(i,j+1) )
     &                       )*halfRL*recip_drC(k)*coordFac*coordFac
          ENDDO
         ENDDO
        ELSE
C     from the averaged flow at grid-cell center (2 compon x 2 pos.)
         DO j=jMin,jMax
          DO i=iMin,iMax
           tempU = ( ( uVel(i,j,km1,bi,bj) + uVel(i+1,j,km1,bi,bj) )
     &              -( uVel(i,j,k  ,bi,bj) + uVel(i+1,j,k  ,bi,bj) )
     &             )*halfRL*recip_drC(k)
     &              *coordFac
           tempV = ( ( vVel(i,j,km1,bi,bj) + vVel(i,j+1,km1,bi,bj) )
     &              -( vVel(i,j,k  ,bi,bj) + vVel(i,j+1,k  ,bi,bj) )
     &             )*halfRL*recip_drC(k)
     &              *coordFac
           stokesterm(i,j) = halfRL*coordFac*(
     &                       tempU*(dstokesUdR(i,j)+dstokesUdR(i+1,j))
     &                     + tempV*(dstokesVdR(i,j)+dstokesVdR(i,j+1))
     &                       )
          ENDDO
         ENDDO
        ENDIF
       ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE stokesterm = comlev1_bibj, key=tkey, kind=isbyte
# endif
#endif /* ALLOW_GGL90_LANGMUIR */

C     compute Prandtl number (always greater than 1)
#ifdef ALLOW_GGL90_IDEMIX
       IF ( useIDEMIX ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          RiNumber = MAX(Nsquare(i,j,k),0. _d 0)
     &         /(verticalShear(i,j)+GGL90eps)
          IDEMIX_RiNumber = MAX( KappaM(i,j)*Nsquare(i,j,k), 0. _d 0)/
     &         ( GGL90eps + IDEMIX_gTKE(i,j,k) )
          prTemp          = 6.6 _d 0 * MIN( RiNumber, IDEMIX_RiNumber )
          TKEPrandtlNumber(i,j,k) = MIN(10. _d 0,prTemp)
          TKEPrandtlNumber(i,j,k) = MAX( oneRL,TKEPrandtlNumber(i,j,k) )
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE TKEPrandtlNumber(:,:,k)=comlev1_bibj_k,key=kkey,kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

       DO j=jMin,jMax
        DO i=iMin,iMax
C        diffusivity
         KappaH = KappaM(i,j)/TKEPrandtlNumber(i,j,k)
         KappaE(i,j,k) = GGL90alpha * KappaM(i,j)
     &        * maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)

C     dissipation term
         TKEdissipation = explDissFac*GGL90ceps
     &        *SQRTTKE(i,j,k)*rMixingLength(i,j,k)
     &        *GGL90TKE(i,j,k,bi,bj)
C     partial update with sum of explicit contributions
         GGL90TKE(i,j,k,bi,bj) = GGL90TKE(i,j,k,bi,bj)
     &        + deltaTloc*(
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
     &         + deltaTloc*IDEMIX_gTKE(i,j,k)
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_GGL90_IDEMIX */

#ifdef ALLOW_GGL90_LANGMUIR
       IF ( useLANGMUIR ) THEN
C     add Langmuir contribution to the turbulent kinetic energy
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90TKE(i,j,k,bi,bj) = GGL90TKE(i,j,k,bi,bj)
     &         + deltaTloc*(KappaM(i,j)*stokesterm(i,j))
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_GGL90_LANGMUIR */

#ifdef ALLOW_GGL90_HORIZDIFF
       IF ( GGL90diffTKEh .GT. 0. _d 0 ) THEN
C--    Add horiz. diffusion tendency
        DO j=jMin,jMax
         DO i=iMin,iMax
          GGL90TKE(i,j,k,bi,bj) = GGL90TKE(i,j,k,bi,bj)
     &                          + gTKE(i,j)*deltaTloc
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_GGL90_HORIZDIFF */

C--   end of k loop
      ENDDO
      IF ( usingPCoords ) THEN
C     impose TKE(1) = 0.
       DO j=jMin,jMax
        DO i=iMin,iMax
         GGL90TKE(i,j,1,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

C     ==============================================
C     Implicit time step to update TKE for k=1,Nr;
C     TKE(Nr+1)=0 by default;
C     for pressure coordinates, this translates into
C     TKE(1)  = 0, TKE(Nr+1) is the surface value
C     ==============================================
C     set up matrix
C--   Lower diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         a3d(i,j,1) = 0. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
#ifdef GGL90_MISSING_HFAC_BUG
       IF ( .NOT.useIDEMIX ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          recip_hFacI(i,j,k) = oneRS
         ENDDO
        ENDDO
       ENDIF
#endif
       km1=MAX(2,k-1)
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( usingPCoords) km1=MIN(Nr,MAX(kSurfC(i,j,bi,bj)+1,k-1))
C-    We keep recip_hFacC in the diffusive flux calculation,
C-    but no hFacC in TKE volume control
C-    No need for maskC(k-1) with recip_hFacC(k-1)
         a3d(i,j,k) = -deltaTloc
     &        *recip_drF(k-1)*recip_hFacC(i,j,k-1,bi,bj)
     &        *.5 _d 0*(KappaE(i,j, k )+KappaE(i,j,km1))
     &        *recip_drC(k)*maskC(i,j,k,bi,bj)*recip_hFacI(i,j,k)
     &        *coordFac*coordFac
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
       kp1=MIN(k+1,Nr)
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( usingZCoords ) kp1=MAX(1,MIN(klowC(i,j,bi,bj),k+1))
C-    We keep recip_hFacC in the diffusive flux calculation,
C-    but no hFacC in TKE volume control
C-    No need for maskC(k) with recip_hFacC(k)
         c3d(i,j,k) = -deltaTloc
     &        *recip_drF( k ) * recip_hFacC(i,j,k,bi,bj)
     &        *.5 _d 0*(KappaE(i,j,k)+KappaE(i,j,kp1))
     &        *recip_drC(k)*maskC(i,j,k-1,bi,bj)*recip_hFacI(i,j,k)
     &        *coordFac*coordFac
        ENDDO
       ENDDO
      ENDDO

      IF (.NOT.GGL90_dirichlet) THEN
C      Neumann bottom boundary condition for TKE: no flux from bottom
       IF ( usingPCoords ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          kBot = MIN(kSurfC(i,j,bi,bj)+1,Nr)
          a3d(i,j,kBot) = 0. _d 0
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          kBot = MAX(kLowC(i,j,bi,bj),1)
          c3d(i,j,kBot) = 0. _d 0
         ENDDO
        ENDDO
       ENDIF
      ENDIF

C--   Center diagonal
      DO k=1,Nr
       km1 = MAX(k-1,1)
       DO j=jMin,jMax
        DO i=iMin,iMax
         b3d(i,j,k) = 1. _d 0 - c3d(i,j,k) - a3d(i,j,k)
     &        + implDissFac*deltaTloc*GGL90ceps*SQRTTKE(i,j,k)
     &        * rMixingLength(i,j,k)
     &        * maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      IF ( usingPCoords ) THEN
C     impose TKE(1) = 0.
       DO j=jMin,jMax
        DO i=iMin,iMax
         b3d(i,j,1) = 1. _d 0
        ENDDO
       ENDDO
      ENDIF
C     end set up matrix

C     Apply boundary condition
      IF ( calcMeanVertShear ) THEN
C     by averaging (@ grid-cell center) the 4 components @ U,V pos.
       DO j=jMin,jMax
        DO i=iMin,iMax
         tempU  = surfaceForcingU( i ,j,bi,bj)
         tempUp = surfaceForcingU(i+1,j,bi,bj)
         tempV  = surfaceForcingV(i, j ,bi,bj)
         tempVp = surfaceForcingV(i,j+1,bi,bj)
         uStarSquare(i,j) =
     &        ( tempU*tempU + tempUp*tempUp
     &        + tempV*tempV + tempVp*tempVp
     &        )*halfRL
C Note: adding parenthesis in 4 terms sum (-> 2 group of 2) as below:
c        uStarSquare(i,j) =
c    &        ( ( tempU*tempU + tempUp*tempUp )
c    &        + ( tempV*tempV + tempVp*tempVp )
c    &        )*halfRL
C       seems to break restart !
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
C     estimate friction velocity uStar from surface forcing
         uStarSquare(i,j) =
     &     ( .5 _d 0*( surfaceForcingU(i,  j,  bi,bj)
     &               + surfaceForcingU(i+1,j,  bi,bj) ) )**2
     &   + ( .5 _d 0*( surfaceForcingV(i,  j,  bi,bj)
     &               + surfaceForcingV(i,  j+1,bi,bj) ) )**2
        ENDDO
       ENDDO
      ENDIF
#ifdef ALLOW_SHELFICE
C     uStarSquare should not have any effect undernath iceshelves, but
C     masking uStarSquare underneath ice shelf is not necessary because
C     surfaceForcingU/V=0 in this case (see shelfice_forcing_surf.F)
C     Instead, uStarSquare is computed from the sub-glacial drag.
      IF ( useSHELFICE .AND.
     &     ( no_slip_shelfice .OR. SHELFICEDragLinear.NE.zeroRL
     &                        .OR. SHELFICEselectDragQuadr.GE.0 )
     &   ) THEN
C     First, we need to compute an early estimate of the drag
C     coefficients based ond the ocean velocity of the previous time
C     step only; because kappaRU and kappaRV are not yet available,
C     kappyRX is just set to horizontally constant viscosity
C     coefficients.
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         stressU(i,j) = 0. _d 0
         stressV(i,j) = 0. _d 0
        ENDDO
       ENDDO
       DO k=1,Nr+1
        ki = MIN(k,Nr)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kappaRX(i,j,k) = viscArNr(ki)
         ENDDO
        ENDDO
       ENDDO
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          KE  (i,j) = 0. _d 0
          uFld(i,j) = uVel(i,j,k,bi,bj)
          vFld(i,j) = vVel(i,j,k,bi,bj)
         ENDDO
        ENDDO
        CALL SHELFICE_U_DRAG_COEFF( bi, bj, k, .FALSE.,
     I       uFld, vFld, kappaRX, KE,
     O       cDragU,
     I       myIter, myThid )
        CALL SHELFICE_V_DRAG_COEFF( bi, bj, k, .FALSE.,
     I       uFld, vFld, kappaRX, KE,
     O       cDragV,
     I       myIter, myThid )
C-     compute explicit stress
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          stressU(i,j) = stressU(i,j) - cDragU(i,j)*uFld(i,j)*rUnit2mass
          stressV(i,j) = stressV(i,j) - cDragV(i,j)*vFld(i,j)*rUnit2mass
         ENDDO
        ENDDO
       ENDDO
C
       DO j=jMin,jMax
        DO i=jMin,jMax
         IF ( kTopC(i,j,bi,bj) .GT. 0 ) THEN
          uStarSquare(i,j) =
     &         ( stressU(i,j)*stressU(i,j)+stressU(i+1,j)*stressU(i+1,j)
     &         + stressV(i,j)*stressV(i,j)+stressV(i,j+1)*stressV(i,j+1)
     &         )*halfRL
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uStarSquare      = comlev1_bibj, key = tkey, kind = isbyte
#endif
      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef ALLOW_AUTODIFF
         IF ( uStarSquare(i,j) .GT. zeroRL )
     &        uStarSquare(i,j) = SQRT(uStarSquare(i,j))*recip_coordFac
#else
         uStarSquare(i,j) = SQRT(uStarSquare(i,j))*recip_coordFac
#endif
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid recomputing the above multiple times in AD routine
CADJ STORE TKEPrandtlNumber = comlev1_bibj, key = tkey, kind = isbyte
CADJ STORE GGL90visctmp     = comlev1_bibj, key = tkey, kind = isbyte
CADJ STORE kappaE           = comlev1_bibj, key = tkey, kind = isbyte
CADJ STORE a3d, b3d, c3d    = comlev1_bibj, key = tkey, kind = isbyte
#endif

C     Dirichlet surface boundary condition for TKE
      IF ( usingPCoords ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
CML#ifdef ALLOW_SHELFICE
CML         IF ( useShelfIce ) THEN
CML          kSrf = MAX(kTopC(i,j,bi,bj),1)
CML          kTop = kSrf
CML         ENDIF
CML#endif
         GGL90TKE(i,j,kSrf,bi,bj) = GGL90TKE(i,j,kSrf,bi,bj)
     &        - c3d(i,j,kSrf) * maskC(i,j,kSrf,bi,bj)
     &        *MAX(GGL90TKEsurfMin,GGL90m2*uStarSquare(i,j))
         c3d(i,j,kSrf) = 0. _d 0
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
#ifdef ALLOW_SHELFICE
         IF ( useShelfIce ) THEN
          kSrf = MAX(1,kTopC(i,j,bi,bj))
          kTop = MIN(kSrf+1,Nr)
         ENDIF
#endif
         GGL90TKE(i,j,kSrf,bi,bj) = maskC(i,j,kSrf,bi,bj)
     &        *MAX(GGL90TKEsurfMin,GGL90m2*uStarSquare(i,j))
         GGL90TKE(i,j,kTop,bi,bj) = GGL90TKE(i,j,kTop,bi,bj)
     &        - a3d(i,j,kTop)*GGL90TKE(i,j,kSrf,bi,bj)
         a3d(i,j,kTop) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

      IF (GGL90_dirichlet) THEN
C      Dirichlet bottom boundary condition for TKE = GGL90TKEbottom
       IF ( usingPCoords ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          kBot = MIN(kSurfC(i,j,bi,bj)+1,Nr)
          GGL90TKE(i,j,kBot,bi,bj) = GGL90TKE(i,j,kBot,bi,bj)
     &                             - GGL90TKEbottom*a3d(i,j,kBot)
          a3d(i,j,kBot) = 0. _d 0
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          kBot = MAX(kLowC(i,j,bi,bj),1)
          GGL90TKE(i,j,kBot,bi,bj) = GGL90TKE(i,j,kBot,bi,bj)
     &                             - GGL90TKEbottom*c3d(i,j,kBot)
          c3d(i,j,kBot) = 0. _d 0
         ENDDO
        ENDDO
       ENDIF
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90TKE(:,:,:,bi,bj)=comlev1_bibj, key=tkey, kind=isbyte
#endif
C     solve tri-diagonal system
      errCode = -1
      CALL SOLVE_TRIDIAGONAL( iMin,iMax, jMin,jMax,
     I                        a3d, b3d, c3d,
     U                        GGL90TKE(1-OLx,1-OLy,1,bi,bj),
     O                        errCode,
     I                        bi, bj, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90TKE(:,:,:,bi,bj)=comlev1_bibj, key=tkey, kind=isbyte
#endif
      DO k=2,Nr
#if ( defined ALLOW_DIAGNOSTICS && !defined ALLOW_AUTODIFF )
C     This diagnostics code causes extra recomputations so we skip it.
       IF ( doDiagTKEmin ) THEN
        DO j=1,sNy
         DO i=1,sNx
          surf_flx_tke(i,j) = GGL90TKE(i,j,k,bi,bj)
     &         * maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
         ENDDO
        ENDDO
       ENDIF
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
C     impose minimum TKE to avoid numerical undershoots below zero;
C     level k=1 is either prescribed surface boundary condition (z-coords) or
C     bottom boundary conditions, which by definition is zero
         GGL90TKE(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &                  *MAX( GGL90TKE(i,j,k,bi,bj), GGL90TKEmin )
        ENDDO
       ENDDO
#if ( defined ALLOW_DIAGNOSTICS && !defined ALLOW_AUTODIFF )
       IF ( doDiagTKEmin ) THEN
        recip_deltaT = 1. _d 0 / deltaTloc
        DO j=1,sNy
         DO i=1,sNx
          surf_flx_tke(i,j) = (GGL90TKE(i,j,k,bi,bj)-surf_flx_tke(i,j))
     &         *recip_deltaT
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL( surf_flx_tke ,'GGL90Emn',
     &                         k, 1, 2, bi, bj, myThid )
       ENDIF
#endif
      ENDDO

C     end of time step
C     ===============================

      DO k=2,Nr
#ifdef ALLOW_GGL90_SMOOTH
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         maskI(i,j) = maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &        *mskCor(i,j,bi,bj)
         GGL90visctmp(i,j,k) = GGL90visctmp(i,j,k)*mskCor(i,j,bi,bj)
        ENDDO
       ENDDO
#endif
       DO j=1,sNy
        DO i=1,sNx
#ifdef ALLOW_GGL90_SMOOTH
         tmpVisc = (
     &     p4 *    GGL90visctmp(i  ,j  ,k)
     &    +p8 *( ( GGL90visctmp(i-1,j  ,k) + GGL90visctmp(i+1,j  ,k) )
     &         + ( GGL90visctmp(i  ,j-1,k) + GGL90visctmp(i  ,j+1,k) ) )
     &    +p16*( ( GGL90visctmp(i+1,j+1,k) + GGL90visctmp(i-1,j-1,k) )
     &         + ( GGL90visctmp(i+1,j-1,k) + GGL90visctmp(i-1,j+1,k) ) )
     &             )/(
     &     p4
     &    +p8 *(( maskI(i-1,j  ) + maskI(i+1,j  ) )
     &         +( maskI(i  ,j-1) + maskI(i  ,j+1) ) )
     &    +p16*(( maskI(i+1,j+1) + maskI(i-1,j-1) )
     &         +( maskI(i+1,j-1) + maskI(i-1,j+1) ) )
     &               )*maskI(i,j)
#else
         tmpVisc = GGL90visctmp(i,j,k)
#endif
         tmpVisc = MIN(tmpVisc/TKEPrandtlNumber(i,j,k),GGL90diffMax)
     &        * coordFac*coordFac
         GGL90diffKr(i,j,k,bi,bj)= MAX( tmpVisc , diffKrNrS(k) )
        ENDDO
       ENDDO

       DO j=1,sNy
        DO i=1,sNx+1
#ifdef ALLOW_GGL90_SMOOTH
         tmpVisc = (
     &     p4 *(   GGL90visctmp(i-1,j  ,k) + GGL90visctmp(i,j  ,k) )
     &    +p8 *( ( GGL90visctmp(i-1,j-1,k) + GGL90visctmp(i,j-1,k) )
     &         + ( GGL90visctmp(i-1,j+1,k) + GGL90visctmp(i,j+1,k) ) )
     &             )/(
     &     p4 * 2. _d 0
     &    +p8 *(( maskI(i-1,j-1) + maskI(i,j-1) )
     &         +( maskI(i-1,j+1) + maskI(i,j+1) ) )
     &               )*maskI(i-1,j) * maskI(i,j)
#else
         tmpVisc = _maskW(i,j,k-1,bi,bj) * _maskW(i,j,k,bi,bj) * halfRL
     &          *( GGL90visctmp(i-1,j,k)
     &           + GGL90visctmp(i,  j,k) )
#endif
         tmpVisc = MIN( tmpVisc , GGL90viscMax )
     &        * coordFac*coordFac
         GGL90viscArU(i,j,k,bi,bj) = MAX( tmpVisc, viscArNr(k) )
        ENDDO
       ENDDO

       DO j=1,sNy+1
        DO i=1,sNx
#ifdef ALLOW_GGL90_SMOOTH
         tmpVisc = (
     &     p4 *(   GGL90visctmp(i  ,j-1,k) + GGL90visctmp(i  ,j,k) )
     &    +p8 *( ( GGL90visctmp(i-1,j-1,k) + GGL90visctmp(i-1,j,k) )
     &         + ( GGL90visctmp(i+1,j-1,k) + GGL90visctmp(i+1,j,k) ) )
     &             )/(
     &     p4 * 2. _d 0
     &    +p8 *(( maskI(i-1,j-1) + maskI(i-1,j) )
     &         +( maskI(i+1,j-1) + maskI(i+1,j) ) )
     &               )*maskI(i,j-1) * maskI(i,j)
#else
         tmpVisc = _maskS(i,j,k-1,bi,bj) * _maskS(i,j,k,bi,bj) * halfRL
     &          *( GGL90visctmp(i,j-1,k)
     &           + GGL90visctmp(i,j,  k) )
#endif
         tmpVisc = MIN( tmpVisc , GGL90viscMax )
     &        * coordFac*coordFac
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
#ifdef ALLOW_GGL90_LANGMUIR
        IF (useLANGMUIR) THEN
         CALL DIAGNOSTICS_FILL( LCmixingLength,'GGL90Lmx',
     &                          0,Nr, 2, bi, bj, myThid )
        ELSE
#else
        IF (.TRUE.) THEN
#endif /* ALLOW_GGL90_LANGMUIR */
         CALL DIAGNOSTICS_FILL( GGL90mixingLength,'GGL90Lmx',
     &                          0,Nr, 2, bi, bj, myThid )
        ENDIF

C     avoid extra 3D diagnostics field and abuse unused field
        IF ( DIAGNOSTICS_IS_ON('GGL90KN2',myThid) ) THEN
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            TKEPrandtlNumber(i,j,k) =
     &           GGL90diffKr(i,j,k,bi,bj) * Nsquare(i,j,k)
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( TKEPrandtlNumber ,'GGL90KN2',
     &                          0, Nr, 2, bi, bj, myThid )
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('GGL90flx',myThid) ) THEN
C     diagnose surface flux of TKE
         IF ( usingPCoords ) THEN
          DO j=jMin,jMax
           DO i=iMin,iMax
CML#ifdef ALLOW_SHELFICE
CML           IF ( useShelfIce ) THEN
CML            kSrf = MAX(kTopC(i,j,bi,bj),1)
CML            kTop = kSrf
CML           ENDIF
CML#endif
            surf_flx_tke(i,j) =
     &           (MAX(GGL90TKEsurfMin,GGL90m2*uStarSquare(i,j))
     &           - GGL90TKE(i,j,kSrf,bi,bj) )
     &           *recip_drF(kSrf)*recip_hFacC(i,j,kSrf,bi,bj)
     &           *KappaE(i,j,kSrf)
     &           *coordFac
           ENDDO
          ENDDO
         ELSE
          DO j=jMin,jMax
           DO i=iMin,iMax
#ifdef ALLOW_SHELFICE
            IF ( useShelfIce ) THEN
             kSrf = MAX(1,kTopC(i,j,bi,bj))
             kTop = MIN(kSrf+1,Nr)
            ENDIF
#endif
            surf_flx_tke(i,j) =(GGL90TKE(i,j,kSrf,bi,bj)-
     &                          GGL90TKE(i,j,kTop,bi,bj))
     &         *recip_drF(kSrf)*recip_hFacC(i,j,kSrf,bi,bj)
     &         *KappaE(i,j,kTop)
           ENDDO
          ENDDO
         ENDIF
         CALL DIAGNOSTICS_FILL( surf_flx_tke,'GGL90flx',
     &                          0, 1, 2, bi, bj, myThid )
        ENDIF

        IF ( DIAGNOSTICS_IS_ON('GGL90tau',myThid) ) THEN
         k=kSrf
         DO j=jMin,jMax
          DO i=iMin,iMax
#ifdef ALLOW_SHELFICE
           IF ( useShelfIce ) k = MAX(1,kTopC(i,j,bi,bj))
#endif
C     diagnose work done by the wind
           surf_flx_tke(i,j) =
     &      halfRL*( surfaceForcingU(i,  j,bi,bj)*uVel(i  ,j,k,bi,bj)
     &              +surfaceForcingU(i+1,j,bi,bj)*uVel(i+1,j,k,bi,bj))
     &    + halfRL*( surfaceForcingV(i,j,  bi,bj)*vVel(i,j  ,k,bi,bj)
     &              +surfaceForcingV(i,j+1,bi,bj)*vVel(i,j+1,k,bi,bj))
           surf_flx_tke(i,j) = surf_flx_tke(i,j) *recip_coordFac
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( surf_flx_tke,'GGL90tau',
     &                          0, 1, 2, bi, bj, myThid )
        ENDIF
C     endif useDiagnostics
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GGL90 */

      RETURN
      END
