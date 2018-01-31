#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_IDEMIX
C     !INTERFACE: ======================================================
      SUBROUTINE GGL90_IDEMIX(
     I     bi, bj, hFacI, recip_hFacI, sigmaR, myTime, myIter, myThid )

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
#include "GMREDI_OPTIONS.h"
#include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi, bj :: Current tile indices
C     hFacI  :: thickness factors for w-cells (interface)
C               with reciprocal of hFacI = recip_hFacI
C     sigmaR :: Vertical gradient of iso-neutral density
C     myTime :: Current time in simulation
C     myIter :: Current time-step number
C     myThid :: My Thread Id number
      INTEGER bi, bj
      _RL       hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL recip_hFacI(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90_IDEMIX
C     !LOCAL VARIABLES :
C     === Local variables ===
      INTEGER iMin ,iMax ,jMin ,jMax
      INTEGER i, j, k, kp1, km1, kBottom
      INTEGER errCode
      _RL  Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  deltaTggl90
      _RL  fxa,fxb,fxc,cstar
      _RL  dfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  dfy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  IDEMIX_gofx2,IDEMIX_hofx1
      _RL  delta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  bN0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  a3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  b3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  c3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  osborn_diff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  c0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  forc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  gm_forc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1
C     set separate time step (should be deltaTtracer)
      deltaTggl90 = dTtracerLev(1)

C     Initialize local fields
      DO k = 1, Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         Nsquare(i,j,k) = 0. _d 0
         delta(i,j,k)   = 0. _d 0
         a3d(i,j,k)     = 0. _d 0
         b3d(i,j,k)     = 1. _d 0
         c3d(i,j,k)     = 0. _d 0
         osborn_diff(i,j,k) = 0. _d 0
         c0(i,j,k)      = 0. _d 0
         forc(i,j,k)    = 0. _d 0
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
     &                  * sigmaR(i,j,k)
         fxb = max( 1. _d -6, abs( fCori(i,j,bi,bj) ))
         Nsquare(i,j,k)= max( 100.*fxb*fxb, Nsquare(i,j,k) )
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
     &       +SQRT(Nsquare(i,j,k))*drC(k)*hFacI(i,j,k)
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
          fxb = max( 1. _d -6, abs( fCori(i,j,bi,bj) ))
          fxa = SQRT(Nsquare(i,j,k))/fxb
          cstar = bN0(i,j)/(pi*IDEMIX_jstar)
          c0(i,j,k)=max(0. _d 0,
     &             cstar*IDEMIX_gamma*IDEMIX_gofx2(fxa))
          IDEMIX_V0(i,j,k,bi,bj)=max(0. _d 0,
     &             cstar*IDEMIX_gamma*IDEMIX_hofx1(fxa))
          fxc = max( 1. _d 0 , fxa )
          fxc = log( fxc + sqrt( fxc*fxc -1.))
          IDEMIX_tau_d(i,j,k,bi,bj) = IDEMIX_mu0*fxb*fxc*
     &         (IDEMIX_jstar*pi/(GGL90eps+bN0(i,j)) )**2
        ENDDO
       ENDDO
      ENDDO
      IF ( IDEMIX_tau_h .GT. 0. _d 0 ) THEN
C     horizontal diffusion of IW energy can become unstable for long
C     time steps, so limit horizontal group velocity to satisfy simple
C     CFL-like criterion:
C     tau_h V0**2 *dt/dx**2 < 0.25 <=> V0 < sqrt( 0.25 * dx**2/(dt*tau_h) )
       fxa = sqrt( 1. _d 0/( deltaTggl90 * IDEMIX_tau_h ) )
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fxb = 0.5*min( _dxF(i,j,bi,bj), _dyF(i,j,bi,bj) )*fxa
          IDEMIX_V0(i,j,k,bi,bj) = min( IDEMIX_V0(i,j,k,bi,bj), fxb )
         ENDDO
        ENDDO
       ENDDO
      ENDIF
c-----------------------------------------------------------------------
c     forcing by mesoscale GM
c-----------------------------------------------------------------------

c     vertically integrated forcing
#ifdef ALLOW_GMREDI
      if (useGmredi) then
#ifdef GM_EG_PROGNOSTIC
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gm_forc(i,j) = gm_forc(i,j)
     &           +GM_EG_diss(i,j,k,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO
#else
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gm_forc(i,j) = gm_forc(i,j)
     &              +max( 0. _d 0,Kwz(i,j,k,bi,bj)*Nsquare(i,j,k) )
     &               *drC(k)*hFacI(i,j,k)
         ENDDO
        ENDDO
       ENDDO
#endif
      endif

      if (IDEMIX_include_GM .and. useGmredi) then
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
#else
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          forc(i,j,k) = forc(i,j,k)
     &              +max( 0. _d 0,Kwz(i,j,k,bi,bj)*Nsquare(i,j,k) )
         ENDDO
        ENDDO
       ENDDO
#endif
      endif

      if (IDEMIX_include_GM_bottom .and. useGmredi) then
c      inject at bottom box only
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         kBottom   = MAX(kLowC(i,j,bi,bj),1)
         forc(i,j,kbottom)=forc(i,j,kbottom)
     &     + gm_forc(i,j)*recip_drC(kbottom)
     &                  *recip_hFacI(i,j,kbottom)
        ENDDO
       ENDDO
      endif
#endif

c-----------------------------------------------------------------------
c     horizontal diffusion of IW energy
c-----------------------------------------------------------------------
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         dfx(1-OLx,j)=0. _d 0
         DO i=1-OLx+1,sNx+OLx
          fxa = IDEMIX_tau_h*0.5 _d 0*(
     &        IDEMIX_V0(i-1,j,k,bi,bj)*maskC(i-1,j,k,bi,bj)
     &       +IDEMIX_V0(i  ,j,k,bi,bj)*maskC(i  ,j,k,bi,bj))
          dfx(i,j) = -fxa*_dyG(i,j,bi,bj)*drC(k)
     &                *(min(.5 _d 0,_hFacW(i,j,k-1,bi,bj) ) +
     &                  min(.5 _d 0,_hFacW(i,j,k  ,bi,bj) ) )
     &      *_recip_dxC(i,j,bi,bj)
     &      *(IDEMIX_V0(i  ,j,k,bi,bj)*IDEMIX_E(i  ,j,k,bi,bj)
     &       -IDEMIX_V0(i-1,j,k,bi,bj)*IDEMIX_E(i-1,j,k,bi,bj))
     &         *maskW(i,j,k,bi,bj) ! paranoia setting
         ENDDO
        ENDDO
        DO i=1-OLx,sNx+OLx
         dfy(i,1-OLy)=0. _d 0
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fxa = IDEMIX_tau_h*0.5 _d 0*(
     &        IDEMIX_V0(i,j  ,k,bi,bj)*maskC(i,j  ,k,bi,bj)
     &       +IDEMIX_V0(i,j-1,k,bi,bj)*maskC(i,j-1,k,bi,bj) )
          dfy(i,j) = -fxa*_dxG(i,j,bi,bj)*drC(k)
     &                *(min(.5 _d 0,_hFacS(i,j,k-1,bi,bj) ) +
     &                  min(.5 _d 0,_hFacS(i,j,k  ,bi,bj) ) )
     &      *_recip_dyC(i,j,bi,bj)
     &      *(IDEMIX_V0(i,j  ,k,bi,bj)*IDEMIX_E(i,j  ,k,bi,bj)
     &       -IDEMIX_V0(i,j-1,k,bi,bj)*IDEMIX_E(i,j-1,k,bi,bj))
     &         *maskS(i,j,k,bi,bj) ! paranoia setting
         ENDDO
        ENDDO
c-----------------------------------------------------------------------
C     Compute divergence of fluxes, add time tendency
c-----------------------------------------------------------------------
        DO j=jMin,jMax
         DO i=iMin,iMax
          IDEMIX_E(i,j,k,bi,bj) = IDEMIX_E(i,j,k,bi,bj)
     &       + deltaTggl90*(-recip_drC(k)*recip_rA(i,j,bi,bj)
     &                   *recip_hFacI(i,j,k)
     &         *((dfx(i+1,j)-dfx(i,j))+(dfy(i,j+1)-dfy(i,j)) )  )
     &         *maskC(i,j,k,bi,bj) ! paranoia setting
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
     &                      + forc(i,j,k)*deltaTggl90
        ENDDO
       ENDDO
      ENDDO
c-----------------------------------------------------------------------
c      solve vertical diffusion implicitly
c-----------------------------------------------------------------------

C     delta_k = dt tau_v /drF_k (c_k+c_k+1)/2
      DO k=2,Nr-1
       DO j=jMin,jMax
        DO i=iMin,iMax
         delta(i,j,k)  = deltaTggl90*IDEMIX_tau_v
     &                  *recip_drF(k)*recip_hFacC(i,j,k,bi,bj)
     &                  *.5 _d 0*(c0(i,j,k)+c0(i,j,k+1))
        ENDDO
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
         delta(i,j,1)  = 0. _d 0
         delta(i,j,Nr) = 0. _d 0
         kBottom   = MAX(kLowC(i,j,bi,bj),1)
         delta(i,j,kBottom) = 0. _d 0
       ENDDO
      ENDDO

C--   Lower diagonal  for E_(k-1) : -delta_k-1 c_k-1/drC_k
      DO j=jMin,jMax
       DO i=iMin,iMax
         a3d(i,j,1) = 0. _d 0
         a3d(i,j,2) = 0. _d 0
       ENDDO
      ENDDO
      DO k=3,Nr
       km1=MAX(2,k-1)
       DO j=jMin,jMax
        DO i=iMin,iMax
C-       No need for maskC(k-1) with recip_hFacC(k-1) in delta(k-1)
         a3d(i,j,k) = -delta(i,j,k-1)*c0(i,j,km1)
     &        *recip_drC(k)*recip_hFacI(i,j,k)
     &        *maskC(i,j,k,bi,bj)!*maskC(i,j,km1,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C--   Upper diagonal for E_(k+1):  delta_k c_k+1/drC_k
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
C-       No need for maskC(k) with recip_hFacC(k) in delta(k)
         kp1=MAX(1,MIN(klowC(i,j,bi,bj),k+1))
         c3d(i,j,k) = -delta(i,j,k)*c0(i,j,kp1)
     &        *recip_drC(k)*recip_hFacI(i,j,k)
     &        *maskC(i,j,k-1,bi,bj)
!     &        *maskC(i,j,k,bi,bj)*maskC(i,j,kp1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      DO j=jMin,jMax ! c3d at bottom is zero
       DO i=iMin,iMax
         c3d(i,j,1) = 0. _d 0
         kBottom   = MAX(kLowC(i,j,bi,bj),1)
         c3d(i,j,kBottom) = 0. _d 0
       ENDDO
      ENDDO

C--   Center diagonal
      DO j=jMin,jMax
       DO i=iMin,iMax
         b3d(i,j,1) = 1. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
       km1 = MAX(k-1,2)
       DO j=jMin,jMax
        DO i=iMin,iMax
          b3d(i,j,k) = 1. _d 0 + deltaTggl90*IDEMIX_tau_d(i,j,k,bi,bj)
     &         *IDEMIX_E(i,j,k,bi,bj)
     &         *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
C-       No need for maskC(k) with recip_hFacC(k) in delta(k)
          b3d(i,j,k) = b3d(i,j,k) + delta(i,j,k)*c0(i,j,k)
     &        *recip_drC(k)*recip_hFacI(i,j,k)
     &        *maskC(i,j,km1,bi,bj)
C-       No need for maskC(k-1) with recip_hFacC(k-1) in delta(k-1)
          b3d(i,j,k) = b3d(i,j,k) + delta(i,j,km1)*c0(i,j,k)
     &        *recip_drC(k)*recip_hFacI(i,j,k)
     &         *maskC(i,j,k,bi,bj)
         ENDDO
       ENDDO
      ENDDO

c     at surface and bottom
      DO j=jMin,jMax
       DO i=iMin,iMax
        k   = MAX(kLowC(i,j,bi,bj),1)
        km1 = MAX(k-1,2)
        b3d(i,j,k) =  1. _d 0  + deltaTggl90*IDEMIX_tau_d(i,j,k,bi,bj)
     &          *IDEMIX_E(i,j,k,bi,bj)
     &          *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
C-       No need for maskC(k-1) with recip_hFacC(k-1) in delta(k-1)
     &        + delta(i,j,km1 )*c0(i,j,k)
     &          *recip_drC(k)*recip_hFacI(i,j,k)
     &          *maskC(i,j,k,bi,bj)
        k=2
        b3d(i,j,k) = 1. _d 0 + deltaTggl90*IDEMIX_tau_d(i,j,k,bi,bj)
     &          *IDEMIX_E(i,j,k,bi,bj)
     &          *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
C-       No need for maskC(k) with recip_hFacC(k) in delta(k)
     &        + delta(i,j,k)*c0(i,j,k)
     &          *recip_drC(k)*recip_hFacI(i,j,k)
     &          *maskC(i,j,km1,bi,bj)

       ENDDO
      ENDDO

C     Apply flux boundary condition
      DO j=jMin,jMax
       DO i=iMin,iMax
        k=2
        IDEMIX_E(i,j,k,bi,bj)  =  IDEMIX_E(i,j,k,bi,bj)
     &      +deltaTggl90*IDEMIX_F_s(i,j,bi,bj)
     &        *recip_drC(k)*recip_hFacI(i,j,k)
     &        *maskC(i,j,k,bi,bj)
        k = MAX(kLowC(i,j,bi,bj),1)
        IDEMIX_E(i,j,k,bi,bj) = IDEMIX_E(i,j,k,bi,bj)
     &    -deltaTggl90*IDEMIX_F_b(i,j,bi,bj)
     &     *recip_drC(k)*recip_hFacI(i,j,k)
     &     *maskC(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     solve tri-diagonal system
      errCode = -1
      CALL SOLVE_TRIDIAGONAL( iMin,iMax, jMin,jMax,
     I                        a3d, b3d, c3d,
     U                        IDEMIX_E(1-OLx,1-OLy,1,bi,bj),
     O                        errCode,
     I                        bi, bj, myThid )

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
c-----------------------------------------------------------------------
c     compute diffusivity due to internal wave breaking
c     assuming local Osborn-Cox balance model
c     kept for diagnostics only
c-----------------------------------------------------------------------
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          osborn_diff(i,j,k) = IDEMIX_mixing_efficiency
     &     *IDEMIX_tau_d(i,j,k,bi,bj)
     &         *IDEMIX_E(i,j,k,bi,bj)*IDEMIX_E(i,j,k,bi,bj)
     &     /max(1. _d -12,Nsquare(i,j,k))*maskC(i,j,k,bi,bj)
          osborn_diff(i,j,k) = min(IDEMIX_diff_max,osborn_diff(i,j,k))
         ENDDO
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL( IDEMIX_E ,'IDEMIX_E',
     &                          0,Nr, 1, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL( IDEMIX_V0 ,'IDEMIX_v',
     &                          0,Nr, 1, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL( IDEMIX_tau_d ,'IDEMIX_t',
     &                          0,Nr, 1, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL( c0 ,'IDEMIX_c',
     &                          0,Nr, 2, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL( osborn_diff ,'IDEMIX_K',
     &                          0,Nr, 2, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL( forc ,'IDEMIX_F',
     &                          0,Nr, 2, bi, bj, myThid )
       CALL DIAGNOSTICS_FILL(IDEMIX_F_b,'IDEM_F_b',0,1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(IDEMIX_F_s,'IDEM_F_s',0,1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(gm_forc,'IDEM_F_g',
     &                          0,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GGL90_IDEMIX */
      RETURN
      END

#ifdef ALLOW_GGL90_IDEMIX
C     helper functions
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      _RL FUNCTION IDEMIX_gofx2(xx)
      IMPLICIT NONE
      _RL x,c,xx
      _RL pi
      PARAMETER( pi = 3.14159265358979323846264338327950588d0 )
      x=MAX(3.d0,xx)
      c= 1.d0-(2.d0/pi)*ASIN(1.d0/x)
      IDEMIX_gofx2 = 2.d0/pi/c*0.9d0*x**(-2.d0/3.d0)*(1.-EXP(-x/4.3d0))
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      _RL FUNCTION IDEMIX_hofx1(x)
      IMPLICIT NONE
      _RL x
      _RL pi
      PARAMETER( pi = 3.14159265358979323846264338327950588d0 )
      IDEMIX_hofx1 = (2.d0/pi)/(1.d0-(2.d0/pi)*
     &              ASIN(1.d0/MAX(1.01d0,x)))*(x-1.d0)/(x+1.d0)
      END
#endif /* ALLOW_GGL90_IDEMIX */
