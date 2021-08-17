#include "GGL90_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#undef GM_EG_PROGNOSTIC

C--  File ggl90_idemix.F:
C--   Contents
C--   o GGL90_IDEMIX
C--   o IDEMIX_gofx2
C--   o IDEMIX_hofx1

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GGL90_IDEMIX
C     !INTERFACE: ======================================================
      SUBROUTINE GGL90_IDEMIX(
     I     bi, bj, hFacI, recip_hFacI, sigmaR,
     O     gTKE,
     I     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R GGL90_IDEMIX
C     |
C     | IDEMIX1 model as described in
C     | - Olbers, D. and Eden, C. (2013), JPO, doi:10.1175/JPO-D-12-0207.1
C     | in a nutshell:
C     | computes contribution of internal wave field to vertical mixing
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GGL90.h"
#include "FFIELDS.h"
#include "GRID.h"

#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi, bj :: Current tile indices
C     hFacI  :: thickness factors for w-cells (interface)
C               with reciprocal of hFacI = recip_hFacI
C     sigmaR :: Vertical gradient of iso-neutral density
C     gTKE   :: TKE tendency due to internal waves (output of S/R GGL90_IDEMIX)
C     myTime :: Current time in simulation
C     myIter :: Current time-step number
C     myThid :: My Thread Id number
      INTEGER bi, bj
      _RL       hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL recip_hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL        gTKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     sigmaR (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90_IDEMIX
C     !FUNCTIONS:
      _RL  IDEMIX_gofx2, IDEMIX_hofx1
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER iMin ,iMax ,jMin ,jMax
      INTEGER i, j, k, kl, kp1, km1
      INTEGER kSrf, kTop, kBot
      INTEGER errCode
      _RL  deltaTloc
C     cstar :: vertical integral over N, eq (13) in Olbers+Eden (2013)
      _RL  fxa,fxb,fxc,cstar,twoOverPi
      _RL  coordFac, recip_coordFac
      _RL  dfx        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  dfy        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     bN0  :: vertically integrated N
      _RL  bN0        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  delta      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  Nsquare    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  a3d        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  b3d        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  c3d        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     v0 :: mean lateral group velocity
      _RL  v0         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     c0 ::  mean vertical group velocity, defined at interfaces (wVel-like)
      _RL  c0         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     tau_d :: dissipation parameter (see Olbers and Eden 2013, eq.12)
      _RL  tau_d      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  forc       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  gm_forc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_DIAGNOSTICS
      _RL  osborn_diff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
CEOP

      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

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

      twoOverPi = 2. _d 0/PI

C     Initialize local fields
      DO k = 1, Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         gTKE(i,j,k)    = 0. _d 0
         Nsquare(i,j,k) = 0. _d 0
         delta(i,j,k)   = 0. _d 0
         a3d(i,j,k)     = 0. _d 0
         b3d(i,j,k)     = 1. _d 0
         c3d(i,j,k)     = 0. _d 0
         c0(i,j,k)      = 0. _d 0
         v0(i,j,k)      = 0. _d 0
         tau_d(i,j,k)   = 0. _d 0
         forc(i,j,k)    = 0. _d 0
#ifdef ALLOW_DIAGNOSTICS
         osborn_diff(i,j,k) = 0. _d 0
#endif
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         dfx(i,j) = 0. _d 0
         dfy(i,j) = 0. _d 0
         bN0(i,j) = 0. _d 0
         gm_forc(i,j) = 0. _d 0
       ENDDO
      ENDDO
c-----------------------------------------------------------------------
c     allow for IW everywhere by limiting buoyancy freq.
c-----------------------------------------------------------------------
      DO k=2,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         Nsquare(i,j,k) = gravity*gravitySign*recip_rhoConst
     &                  * sigmaR(i,j,k) * coordFac
         fxb = MAX( 1. _d -6, ABS( fCori(i,j,bi,bj) ))
         Nsquare(i,j,k)= MAX( 100.*fxb*fxb, Nsquare(i,j,k) )
     &                 *maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
c-----------------------------------------------------------------------
c     vertically integrated N
c-----------------------------------------------------------------------
      DO k=2,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
           bN0(i,j)=bN0(i,j)
     &       +SQRT(Nsquare(i,j,k))*drC(k)*recip_coordFac*hFacI(i,j,k)
        ENDDO
       ENDDO
      ENDDO
c-----------------------------------------------------------------------
c     vertical and horizontal group velocities
c     and constant for dissipation
c-----------------------------------------------------------------------
      DO k=2,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          fxb = MAX( 1. _d -6, ABS( fCori(i,j,bi,bj) ))
          fxa = SQRT(Nsquare(i,j,k))/fxb
          cstar = bN0(i,j)/(PI*IDEMIX_jstar)
          c0(i,j,k)=MAX(0. _d 0,
     &             cstar*IDEMIX_gamma*IDEMIX_gofx2(fxa,twoOverPI))
          v0(i,j,k)=MAX(0. _d 0,
     &             cstar*IDEMIX_gamma*IDEMIX_hofx1(fxa,twoOverPI))
          fxc = MAX( 1. _d 0 , fxa )
          fxc = LOG( fxc + SQRT( fxc*fxc -1.))
          tau_d(i,j,k) = IDEMIX_mu0*fxb*fxc*
     &         (IDEMIX_jstar*PI/(GGL90eps+bN0(i,j)) )**2
        ENDDO
       ENDDO
      ENDDO
      IF ( IDEMIX_tau_h .GT. 0. _d 0 ) THEN
C     horizontal diffusion of IW energy can become unstable for long
C     time steps, so limit horizontal group velocity to satisfy simple
C     CFL-like criterion:
C     tau_h V0**2 *dt/dx**2 < 0.25 <=> V0 < sqrt( 0.25 * dx**2/(dt*tau_h) )
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fxa = SQRT( 1. _d 0/( deltaTloc * IDEMIX_tau_h ) )
          fxb = 0.5*MIN( _dxF(i,j,bi,bj), _dyF(i,j,bi,bj) )*fxa
          v0(i,j,k) = MIN( v0(i,j,k), fxb )
         ENDDO
        ENDDO
       ENDDO
      ENDIF
c-----------------------------------------------------------------------
c     forcing by mesoscale GM
c-----------------------------------------------------------------------

c     vertically integrated forcing
#ifdef ALLOW_GMREDI
      IF (useGmredi) THEN
#ifdef GM_EG_PROGNOSTIC
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gm_forc(i,j) = gm_forc(i,j)
     &         +GM_EG_diss(i,j,k,bi,bj)
     &         *drF(k)*recip_coordFac*hFacC(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO
#else /* GM_EG_PROGNOSTIC */
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gm_forc(i,j) = gm_forc(i,j)
     &              +MAX( 0. _d 0,Kwz(i,j,k,bi,bj)*Nsquare(i,j,k) )
     &               *drC(k)*recip_coordFac*hFacI(i,j,k)
         ENDDO
        ENDDO
       ENDDO
#endif /* GM_EG_PROGNOSTIC */
      ENDIF

      IF (IDEMIX_include_GM .AND. useGmredi) THEN
c      inject locally
#ifdef GM_EG_PROGNOSTIC
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          forc(i,j,k) = forc(i,j,k)
     &              +.5 _d 0*(GM_EG_diss(i,j,k,bi,bj)+
     &                        GM_EG_diss(i,j,k-1,bi,bj))
         ENDDO
        ENDDO
       ENDDO
#else /* GM_EG_PROGNOSTIC */
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          forc(i,j,k) = forc(i,j,k)
     &              +MAX( 0. _d 0,Kwz(i,j,k,bi,bj)*Nsquare(i,j,k) )
         ENDDO
        ENDDO
       ENDDO
#endif /* GM_EG_PROGNOSTIC */
      ENDIF

      IF (IDEMIX_include_GM_bottom .AND. useGmredi) THEN
c      inject at bottom box only
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF ( usingPCoords ) THEN
          kBot=MIN(kSurfC(i,j,bi,bj)+1,Nr)
         ELSE
          kBot   = MAX(kLowC(i,j,bi,bj),1)
         ENDIF
         forc(i,j,kBot)=forc(i,j,kBot)
     &     + gm_forc(i,j)*recip_drC(kBot)*coordFac
     &                  *recip_hFacI(i,j,kBot)
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_GMREDI */

c-----------------------------------------------------------------------
c     horizontal diffusion of IW energy
c-----------------------------------------------------------------------
       DO k=2,Nr
        kl = k
        IF ( usingPCoords ) kl = k - 1
        DO j=1-OLy,sNy+OLy
         dfx(1-OLx,j)=0. _d 0
         DO i=1-OLx+1,sNx+OLx
          fxa = IDEMIX_tau_h*0.5 _d 0*(
     &        v0(i-1,j,k)*maskC(i-1,j,kl,bi,bj)
     &       +v0(i  ,j,k)*maskC(i  ,j,kl,bi,bj))
          dfx(i,j) = -fxa*_dyG(i,j,bi,bj)*drC(k)
     &                *(MIN(.5 _d 0,_hFacW(i,j,k-1,bi,bj) ) +
     &                  MIN(.5 _d 0,_hFacW(i,j,k  ,bi,bj) ) )
     &      *_recip_dxC(i,j,bi,bj)
     &      *(v0(i  ,j,k)*IDEMIX_E(i  ,j,k,bi,bj)
     &       -v0(i-1,j,k)*IDEMIX_E(i-1,j,k,bi,bj))
     &         *maskW(i,j,kl,bi,bj) ! paranoia setting
         ENDDO
        ENDDO
        DO i=1-OLx,sNx+OLx
         dfy(i,1-OLy)=0. _d 0
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fxa = IDEMIX_tau_h*0.5 _d 0*(
     &        v0(i,j  ,k)*maskC(i,j  ,kl,bi,bj)
     &       +v0(i,j-1,k)*maskC(i,j-1,kl,bi,bj) )
          dfy(i,j) = -fxa*_dxG(i,j,bi,bj)*drC(k)
     &                *(MIN(.5 _d 0,_hFacS(i,j,k-1,bi,bj) ) +
     &                  MIN(.5 _d 0,_hFacS(i,j,k  ,bi,bj) ) )
     &      *_recip_dyC(i,j,bi,bj)
     &      *(v0(i,j  ,k)*IDEMIX_E(i,j  ,k,bi,bj)
     &       -v0(i,j-1,k)*IDEMIX_E(i,j-1,k,bi,bj))
     &         *maskS(i,j,kl,bi,bj) ! paranoia setting
         ENDDO
        ENDDO
c-----------------------------------------------------------------------
C     Compute divergence of fluxes, add time tendency
c-----------------------------------------------------------------------
        DO j=jMin,jMax
         DO i=iMin,iMax
          IDEMIX_E(i,j,k,bi,bj) = IDEMIX_E(i,j,k,bi,bj)
     &       + deltaTloc*(-recip_drC(k)*recip_rA(i,j,bi,bj)
     &                   *recip_hFacI(i,j,k)
     &         *((dfx(i+1,j)-dfx(i,j))+(dfy(i,j+1)-dfy(i,j)) )  )
     &         *maskC(i,j,kl,bi,bj) ! paranoia setting
         ENDDO
        ENDDO
       ENDDO ! k loop
c-----------------------------------------------------------------------
c      add interior forcing e.g. by mesoscale GM
c-----------------------------------------------------------------------
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          IDEMIX_E(i,j,k,bi,bj) = IDEMIX_E(i,j,k,bi,bj)
     &                      + forc(i,j,k)*deltaTloc
        ENDDO
       ENDDO
      ENDDO
c-----------------------------------------------------------------------
c      solve vertical diffusion implicitly
c-----------------------------------------------------------------------

C     delta_k = dt tau_v /drF_k (c_k+c_k+1)/2
C     delta(1) and delta(Nr) are zero by initialisation
      DO k=2,Nr-1
       DO j=jMin,jMax
        DO i=iMin,iMax
         delta(i,j,k)  = deltaTloc*IDEMIX_tau_v
     &        *recip_drF(k)*coordFac*recip_hFacC(i,j,k,bi,bj)
     &        *.5 _d 0*(c0(i,j,k)+c0(i,j,k+1))
        ENDDO
       ENDDO
      ENDDO
      IF ( usingPCoords ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         kBot = MIN(kSurfC(i,j,bi,bj),Nr)
         delta(i,j,kBot) = 0. _d 0
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         kBot = MAX(kLowC(i,j,bi,bj),1)
         delta(i,j,kBot) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

C--   Lower diagonal  for E_(k-1) : -delta_k-1 c_k-1/drC_k
C     but leaving the contribution of c0_k-1 for later
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
C-       No need for maskC(k-1) with recip_hFacC(k-1) in delta(k-1)
         a3d(i,j,k) = -delta(i,j,k-1)
     &        *recip_drC(k)*coordFac*recip_hFacI(i,j,k)
     &        *maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C--   Upper diagonal for E_(k+1):  delta_k c_k+1/drC_k
C     but leaving the contribution of c0_k+1 for later
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
C-       No need for maskC(k) with recip_hFacC(k) in delta(k)
         c3d(i,j,k) = -delta(i,j,k)
     &        *recip_drC(k)*coordFac*recip_hFacI(i,j,k)
     &        *maskC(i,j,k-1,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     treat bottom and surface boundaries for coeffients
C     of upper and lower diagonal by masking
      IF ( usingPCoords ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
C     a3d at bottom is zero
         kBot=MIN(kSurfC(i,j,bi,bj)+1,Nr)
         a3d(i,j,kBot) = 0. _d 0
C     for p-coords, c3d is zero at the surface, too
         c3d(i,j,Nr)   = 0. _d 0
        ENDDO
       ENDDO
      ELSEIF ( usingZCoords ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
C     c3d at bottom is zero
         kBot = MAX(kLowC(i,j,bi,bj),1)
         c3d(i,j,kBot) = 0. _d 0
C     for z-coords, a3d is zero at the surface (level 2), too
         a3d(i,j,kTop) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF
C     For k=1 there is nothing to solve for
      DO j=jMin,jMax
       DO i=iMin,iMax
C     so that both off-diagonal coefficients are zero
        a3d(i,j,1) = 0. _d 0
        c3d(i,j,1) = 0. _d 0
C     and the main diagonal is one (for stability)
        b3d(i,j,1) = 1. _d 0
       ENDDO
      ENDDO

C--   Center diagonal
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         b3d(i,j,k) = 1. _d 0+deltaTloc*tau_d(i,j,k)
     &         *IDEMIX_E(i,j,k,bi,bj)
     &         *maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
     &        - ( a3d(i,j,k) + c3d(i,j,k) ) * c0(i,j,k)
        ENDDO
       ENDDO
      ENDDO

C--   Complete computation of lower and upper diagonal after they have
C     been used for the center diagonal: add the contribution of c0_k+/-1
      DO k=2,Nr
       kp1=MIN(k+1,Nr)
       km1=MAX(k-1,2)
       DO j=jMin,jMax
        DO i=iMin,iMax
         a3d(i,j,k) = a3d(i,j,k)*c0(i,j,km1)
         c3d(i,j,k) = c3d(i,j,k)*c0(i,j,kp1)
        ENDDO
       ENDDO
      ENDDO

C--   Apply flux boundary condition
      kl = kTop
      IF ( usingPCoords ) kl = kTop - 1
      DO j=jMin,jMax
       DO i=iMin,iMax
        IDEMIX_E(i,j,kTop,bi,bj) = IDEMIX_E(i,j,kTop,bi,bj)
     &       +deltaTloc*IDEMIX_F_s(i,j,bi,bj)
     &       *recip_drC(kTop)*coordFac*recip_hFacI(i,j,kTop)
     &       *maskC(i,j,kl,bi,bj)
       ENDDO
      ENDDO
      IF ( usingZCoords) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         kBot = MAX(kLowC(i,j,bi,bj),1)
         IDEMIX_E(i,j,kBot,bi,bj) = IDEMIX_E(i,j,kBot,bi,bj)
     &        -deltaTloc*IDEMIX_F_b(i,j,bi,bj)
     &        *recip_drC(kBot)*coordFac*recip_hFacI(i,j,kBot)
     &        *maskC(i,j,kBot,bi,bj)
        ENDDO
       ENDDO
      ELSEIF ( usingPCoords ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         kBot = MIN(kSurfC(i,j,bi,bj)+1,Nr)
         IDEMIX_E(i,j,kBot,bi,bj) = IDEMIX_E(i,j,kBot,bi,bj)
     &        -deltaTloc*IDEMIX_F_b(i,j,bi,bj)
     &        *recip_drC(kBot)*coordFac*recip_hFacI(i,j,kBot)
     &        *maskC(i,j,kBot-1,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C     solve tri-diagonal system
      errCode = -1
      CALL SOLVE_TRIDIAGONAL( iMin,iMax, jMin,jMax,
     I                        a3d, b3d, c3d,
     U                        IDEMIX_E(1-OLx,1-OLy,1,bi,bj),
     O                        errCode,
     I                        bi, bj, myThid )

C     generate TKE tendency due to internal waves (output)
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         gTKE(i,j,k) =
     &        tau_d(i,j,k)*IDEMIX_E(i,j,k,bi,bj)*IDEMIX_E(i,j,k,bi,bj)
C-    to reproduce older results (before adding output arguement gTKE)
c    &        tau_d(i,j,k)*IDEMIX_E(i,j,k,bi,bj)**2
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
c-----------------------------------------------------------------------
c     compute diffusivity due to internal wave breaking
c     assuming local Osborn-Cox balance model
c     kept for diagnostics only
c-----------------------------------------------------------------------
       IF ( DIAGNOSTICS_IS_ON('IDEMIX_K',myThid) ) THEN
        DO k=2,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
           osborn_diff(i,j,k) = IDEMIX_mixing_efficiency * gTKE(i,j,k)
     &          /MAX(1. _d -12,Nsquare(i,j,k))*maskC(i,j,k,bi,bj)
           osborn_diff(i,j,k) = MIN(IDEMIX_diff_max,osborn_diff(i,j,k))
          ENDDO
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL( osborn_diff ,'IDEMIX_K',
     &                          0, Nr, 2, bi, bj, myThid )
       ENDIF
       CALL DIAGNOSTICS_FILL( IDEMIX_E ,'IDEMIX_E',0,Nr,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL( tau_d,    'IDEMIX_t',0,Nr,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL( v0,       'IDEMIX_v',0,Nr,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL( c0 ,      'IDEMIX_c',0,Nr,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL( forc,     'IDEMIX_F',0,Nr,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(IDEMIX_F_b,'IDEM_F_b',0, 1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(IDEMIX_F_s,'IDEM_F_s',0, 1,1,bi,bj,myThid)
# ifdef ALLOW_GMREDI
       IF (useGmredi) THEN
        CALL DIAGNOSTICS_FILL( gm_forc, 'IDEM_F_g',0, 1,2,bi,bj,myThid)
       ENDIF
# endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GGL90_IDEMIX */
      RETURN
      END

#ifdef ALLOW_GGL90_IDEMIX
C     helper functions
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      _RL FUNCTION IDEMIX_gofx2(xx,toPI)
      IMPLICIT NONE
      _RL xx
      _RL toPI                  ! 2.d0/PI
      _RL x,c
      x=MAX(3.d0,xx)
      c= 1.d0-toPI*ASIN(1.d0/x)
      IDEMIX_gofx2 = toPI/c*0.9d0*x**(-2.d0/3.d0)*(1.-EXP(-x/4.3d0))
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      _RL FUNCTION IDEMIX_hofx1(x,toPI)
      IMPLICIT NONE
      _RL x
      _RL toPI                  ! 2.d0/PI
      IDEMIX_hofx1 = toPI/(1.d0-toPI*ASIN(1.d0/MAX(1.01d0,x)))
     &     *(x-1.d0)/(x+1.d0)
      END
#endif /* ALLOW_GGL90_IDEMIX */
