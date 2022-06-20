#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
c#ifdef ALLOW_GENERIC_ADVDIFF
c# include "GAD_OPTIONS.h"
c#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GMREDI_TRIADS_DIFFUSE

C !INTERFACE: ==========================================================
      SUBROUTINE GMREDI_TRIADS_DIFFUSE(
     I     trIdentity, deltaTLev,
     I     rhoLoc, tracer,
     U     kappaRk,
     U     gTracer,
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C Calculates the tendency of a tracer due to Isopycnal Diffusion
C following Griffies etal, 1998, JPO, Triads implementation
C
C This routine updates the tracer tendency and vertical diffusivity
C  by adding, respectively, Isopycnal Diffusion tendency and vertical
C  diffusity contribution

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
c#ifdef ALLOW_GENERIC_ADVDIFF
c# include "GAD.h"
c#endif

C !INPUT PARAMETERS: ===================================================
C  trIdentity        :: tracer identifier
C  deltaTLev         :: tracer time-step
C  rhoLoc            :: local sea-water density
C  tracer            :: tracer field
C  bi, bj            :: tile indices
C  myTime            :: current time
C  myIter            :: iteration number
C  myThid            :: thread number
      INTEGER trIdentity
      _RL deltaTLev(Nr)
      _RL rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  gTracer           :: vertical diffusivity array
C  gTracer           :: tendency array
      _RL kappaRk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef GM_TRIADS_SCHEME
C !FUNCTIONS: ==========================================================
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
# ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
# endif
#endif

C !LOCAL VARIABLES: ====================================================
C  i, j, k       :: loop indices
C  xA, yA        :: areas of X and Y face of tracer cells
C  dfx           :: 3-D array of horizontal diffusive flux in X-dir
C  dfy           :: 3-D array of horizontal diffusive flux in Y-dir
C  dfr           :: 3-D array of vertical diffusive flux
C  sigmaX        :: density gradient, X component
C  sigmaY        :: density gradient, Y component
C  KwzLoc        :: vertical diffusivity contribution from GM-Redi
C  slope         :: isopycnal slope (no sign)
C  Kslope        :: Kappa x slope (no sign)
C  K_slp         :: tappered Kappa x slope (with sign)
C  K_lim         :: tappered Kappa
C dTdx,dTdy,dTdr :: tracer gradient, 3 components
      INTEGER i, j, k, kl, kp1
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dfy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dfr     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KwzLoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL GM_bolK3d
      _RL dR_delT_lim
      _RL maskp25, mskP1
      _RL K_loc, slope
      _RL Kslope, K_lim, K_slp
      _RL dTdx, dTdy, dTdr
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagAdvX, doDiagAdvY, doDiagAdvR
#endif /* ALLOW_DIAGNOSTICS */
CEOP

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostics flags and suffix for the current tracer
      doDiagAdvX = .FALSE.
      doDiagAdvY = .FALSE.
      doDiagAdvR = .FALSE.
      IF ( useDiagnostics ) THEN
       diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
       diagName = 'ADVx'//diagSufx
       doDiagAdvX = DIAGNOSTICS_IS_ON( diagName, myThid )
       diagName = 'ADVy'//diagSufx
       doDiagAdvY = DIAGNOSTICS_IS_ON( diagName, myThid )
       diagName = 'ADVr'//diagSufx
       doDiagAdvR = DIAGNOSTICS_IS_ON( diagName, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Set up work arrays with valid (i.e. not NaN) values
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         sigmaX(i,j) = 0. _d 0
         sigmaY(i,j) = 0. _d 0
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dfx(i,j,k) = 0. _d 0
         dfy(i,j,k) = 0. _d 0
         dfr(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Stability criteria:
C        |slope|*Kgm*dt/(dx*dz) < 1/4* c_max
C   expressed as dt_max (GM_delT_lim):
C     if this is satisfied: |slope|*Kgm < 1/4 (dx*dz) / dt_max
C     then will just have to pick dt_max such as: dt < c_max * dt_max

C-  Converting Griffies etal, JPO paper (MOM) grid variables to MITgcm var:
C      dxu --> dxC ; dxt --> dxF
C      dzw --> drC ; dzt --> drF
C   grid-weight factor from JPO paper: e.g. for triad #3,
C   contribution to dfx(i,j,k) is *drC(k) and at the end divide dfx by 4*drF(k)
C   contribution to dfr(i,j,k) is *dxC(i) and at the end divide dfr by 4*dxF(i)
C    and same for KappaR(i,j,k)
C-  Here last division is accounted in divergence of fluxes (units: [Tr]*m^3/s)
C   and we scale by a fraction of the grid-cell interrface area, e.g., triad #3
C    contrib to dfx: *drC(k)*dyG(i,j)*deepFacF(k)
C    contrib to dfr: *rAw(i,j)*deepFac2F(k)

C---  Start of k loop for horizontal fluxes
      DO k=2,Nr

C-    for stability limit:
       dR_delT_lim = op25*drC(k)*deepFacF(k) / GM_delT_lim

C-    set-up local 2-D array within k loop
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         xA(i,j) = _dyG(i,j,bi,bj)*deepFacF(k)*drC(k)
         yA(i,j) = _dxG(i,j,bi,bj)*deepFacF(k)*drC(k)
         KwzLoc(i,j) = 0. _d 0
        ENDDO
       ENDDO

C--   X-Z plane:

C-     density gradient in X-dir @ level k-1
       kl = k-1
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         sigmaX(i,j) =_maskW(i,j,kl,bi,bj)
     &               *_recip_dxC(i,j,bi,bj)*recip_deepFacC(kl)
     &               *( rhoLoc(i,j,kl) - rhoLoc(i-1,j,kl) )
        ENDDO
       ENDDO

C--   triad #1 : (i-1,k-1)| (i,k-1)
C                (i-1,k)  |
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskW(i,j,k-1,bi,bj)*maskC(i-1,j,k,bi,bj)
         slope = ABS( sigmaX(i,j)*GM_Nm2(i-1,j,k,bi,bj) )
         K_loc = GM_isoK3d(i-1,j,k-1,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dxC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaX(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdx = ( tracer(i,j,k-1) - tracer(i-1,j,k-1) )
     &        * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k-1)
         dTdr = ( tracer(i-1,j,k-1) - tracer(i-1,j,k) )
     &        * recip_drC(k)
         dfx(i,j,k-1) = dfx(i,j,k-1)
     &              - ( K_lim*dTdx
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*xA(i,j)
         dfr(i-1,j,k) = dfr(i-1,j,k)
     &              - ( K_slp*dTdx
C    &                + GM_skewflx*GM_bolK3d*dTdx
     &                )*rAw(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i-1,j) = KwzLoc(i-1,j)
     &              + Kslope*slope*rAw(i,j,bi,bj)
        ENDDO
       ENDDO

C--   triad #2 : (i-1,k-1)| (i,k-1)
C                         |  (i,k)
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskW(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
         slope = ABS( sigmaX(i,j)*GM_Nm2(i,j,k,bi,bj) )
         K_loc = GM_isoK3d(i,j,k-1,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dxC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaX(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdx = ( tracer(i,j,k-1) - tracer(i-1,j,k-1) )
     &        * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k-1)
         dTdr = ( tracer(i,j,k-1) - tracer(i,j,k) )
     &        * recip_drC(k)
         dfx(i,j,k-1) = dfx(i,j,k-1)
     &              - ( K_lim*dTdx
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*xA(i,j)
         dfr(i,j,k) = dfr(i,j,k)
     &              - ( K_slp*dTdx
C    &                + GM_skewflx*GM_bolK3d*dTdx
     &                )*rAw(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i,j) = KwzLoc(i,j)
     &              + Kslope*slope*rAw(i,j,bi,bj)
        ENDDO
       ENDDO

C-     density gradient in X-dir @ level k
       kl = k
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         sigmaX(i,j) =_maskW(i,j,kl,bi,bj)
     &               *_recip_dxC(i,j,bi,bj)*recip_deepFacC(kl)
     &               *( rhoLoc(i,j,kl) - rhoLoc(i-1,j,kl) )
        ENDDO
       ENDDO

C--   triad #3 :          | (i,k-1)
C                (i-1,k)  |  (i,k)
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskW(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
         slope = ABS( sigmaX(i,j)*GM_Nm2(i,j,k,bi,bj) )
         K_loc = GM_isoK3d(i,j,k,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dxC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaX(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdx = ( tracer(i,j,k) - tracer(i-1,j,k) )
     &        * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
         dTdr = ( tracer(i,j,k-1) - tracer(i,j,k) )
     &        * recip_drC(k)
         dfx(i,j,k) = dfx(i,j,k)
     &              - ( K_lim*dTdx
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*xA(i,j)
         dfr(i,j,k) = dfr(i,j,k)
     &              - ( K_slp*dTdx
C    &                + GM_skewflx*GM_bolK3d*dTdx
     &                )*rAw(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i,j) = KwzLoc(i,j)
     &              + Kslope*slope*rAw(i,j,bi,bj)
        ENDDO
       ENDDO

C--   triad #4 : (i-1,k-1)|
C                (i-1,k)  |  (i,k)
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskW(i,j,k,bi,bj)*maskC(i-1,j,k-1,bi,bj)
         slope = ABS( sigmaX(i,j)*GM_Nm2(i-1,j,k,bi,bj) )
         K_loc = GM_isoK3d(i-1,j,k,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dxC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaX(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdx = ( tracer(i,j,k) - tracer(i-1,j,k) )
     &        * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
         dTdr = ( tracer(i-1,j,k-1) - tracer(i-1,j,k) )
     &        * recip_drC(k)
         dfx(i,j,k) = dfx(i,j,k)
     &              - ( K_lim*dTdx
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*xA(i,j)
         dfr(i-1,j,k) = dfr(i-1,j,k)
     &              - ( K_slp*dTdx
C    &                + GM_skewflx*GM_bolK3d*dTdx
     &                )*rAw(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i-1,j) = KwzLoc(i-1,j)
     &              + Kslope*slope*rAw(i,j,bi,bj)
        ENDDO
       ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Y-Z plane:

C-     density gradient in Y-dir @ level k-1
       kl = k-1
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         sigmaY(i,j)=_maskS(i,j,kl,bi,bj)
     &              *_recip_dyC(i,j,bi,bj)*recip_deepFacC(kl)
     &              *( rhoLoc(i,j,kl) - rhoLoc(i,j-1,kl) )
        ENDDO
       ENDDO

C--   triad #1 : (j-1,k-1)| (j,k-1)
C                (j-1,k)  |
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskS(i,j,k-1,bi,bj)*maskC(i,j-1,k,bi,bj)
         slope = ABS( sigmaY(i,j)*GM_Nm2(i,j-1,k,bi,bj) )
         K_loc = GM_isoK3d(i,j-1,k-1,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dyC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaY(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdy = ( tracer(i,j,k-1) - tracer(i,j-1,k-1) )
     &        * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k-1)
         dTdr = ( tracer(i,j-1,k-1) - tracer(i,j-1,k) )
     &        * recip_drC(k)
         dfy(i,j,k-1) = dfy(i,j,k-1)
     &              - ( K_lim*dTdy
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*yA(i,j)
         dfr(i,j-1,k) = dfr(i,j-1,k)
     &              - ( K_slp*dTdy
C    &                + GM_skewflx*GM_bolK3d*dTdy
     &                )*rAs(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i,j-1) = KwzLoc(i,j-1)
     &              + Kslope*slope*rAs(i,j,bi,bj)
        ENDDO
       ENDDO

C--   triad #2 : (j-1,k-1)| (j,k-1)
C                         |  (j,k)
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskS(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
         slope = ABS( sigmaY(i,j)*GM_Nm2(i,j,k,bi,bj) )
         K_loc = GM_isoK3d(i,j,k-1,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dyC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaY(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdy = ( tracer(i,j,k-1) - tracer(i,j-1,k-1) )
     &        * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k-1)
         dTdr = ( tracer(i,j,k-1) - tracer(i,j,k) )
     &        * recip_drC(k)
         dfy(i,j,k-1) = dfy(i,j,k-1)
     &              - ( K_lim*dTdy
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*yA(i,j)
         dfr(i,j,k) = dfr(i,j,k)
     &              - ( K_slp*dTdy
C    &                + GM_skewflx*GM_bolK3d*dTdy
     &                )*rAs(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i,j) = KwzLoc(i,j)
     &              + Kslope*slope*rAs(i,j,bi,bj)
        ENDDO
       ENDDO

C-     density gradient in Y-dir @ level k
       kl = k
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         sigmaY(i,j)=_maskS(i,j,kl,bi,bj)
     &              *_recip_dyC(i,j,bi,bj)*recip_deepFacC(kl)
     &              *( rhoLoc(i,j,kl) - rhoLoc(i,j-1,kl) )
        ENDDO
       ENDDO

C--   triad #3 :          | (j,k-1)
C                (j-1,k)  |  (j,k)
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskS(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
         slope = ABS( sigmaY(i,j)*GM_Nm2(i,j,k,bi,bj) )
         K_loc = GM_isoK3d(i,j,k,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dyC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaY(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdy = ( tracer(i,j,k) - tracer(i,j-1,k) )
     &        * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
         dTdr = ( tracer(i,j,k-1) - tracer(i,j,k) )
     &        * recip_drC(k)
         dfy(i,j,k) = dfy(i,j,k)
     &              - ( K_lim*dTdy
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*yA(i,j)
         dfr(i,j,k) = dfr(i,j,k)
     &              - ( K_slp*dTdy
C    &                + GM_skewflx*GM_bolK3d*dTdy
     &                )*rAs(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i,j) = KwzLoc(i,j)
     &              + Kslope*slope*rAs(i,j,bi,bj)
        ENDDO
       ENDDO

C--   triad #4 : (j-1,k-1)|
C                (j-1,k)  |  (j,k)
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx

C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
         maskp25 = op25*maskS(i,j,k,bi,bj)*maskC(i,j-1,k-1,bi,bj)
         slope = ABS( sigmaY(i,j)*GM_Nm2(i,j-1,k,bi,bj) )
         K_loc = GM_isoK3d(i,j-1,k,bi,bj)
C-     Stability limit:
         Kslope = MIN( K_loc*slope, dyC(i,j,bi,bj)*dR_delT_lim )
     &           *maskp25
         IF ( slope.GT.zeroRL ) THEN
          K_lim = Kslope / slope
          K_slp = SIGN( Kslope, sigmaY(i,j) )
         ELSE
          K_lim = K_loc*maskp25
          K_slp = 0. _d 0
         ENDIF
         dTdy = ( tracer(i,j,k) - tracer(i,j-1,k) )
     &        * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
         dTdr = ( tracer(i,j-1,k-1) - tracer(i,j-1,k) )
     &        * recip_drC(k)
         dfy(i,j,k) = dfy(i,j,k)
     &              - ( K_lim*dTdy
     &                + K_slp*dTdr
C    &                - GM_skewflx*GM_bolK3d*dTdr
     &                )*yA(i,j)
         dfr(i,j-1,k) = dfr(i,j-1,k)
     &              - ( K_slp*dTdy
C    &                + GM_skewflx*GM_bolK3d*dTdy
     &                )*rAs(i,j,bi,bj)*deepFac2F(k)
         KwzLoc(i,j-1) = KwzLoc(i,j-1)
     &              + Kslope*slope*rAs(i,j,bi,bj)
        ENDDO
       ENDDO

C--   end Y-Z plane.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Add contribution to vertical diffusivity:
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         KwzLoc(i,j) = KwzLoc(i,j)*recip_rA(i,j,bi,bj)
         kappaRk(i,j,k) = kappaRk(i,j,k)
c    &              + KwzLoc(i,j)
     &              + MIN( KwzLoc(i,j), 1000. _d 0 )
        ENDDO
       ENDDO

C---  end k loop
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Tendency is minus flux-divergence:
      DO k= 1,Nr
       kp1 = MIN( k+1, Nr )
       mskP1 = 1. _d 0
       IF ( k.EQ.Nr ) mskP1 = 0. _d 0
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
          gTracer(i,j,k) = gTracer(i,j,k)
     &       - _recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &        *recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &        *( ( dfx(i+1,j,k) - dfx(i,j,k) )
     &         + ( dfy(i,j+1,k) - dfy(i,j,k) )
     &         + ( dfr(i,j,kp1)*mskP1 - dfr(i,j,k) )*rkSign
     &         )*maskInC(i,j,bi,bj)
c    &        *recip_rhoFacC(k)
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
c      IF ( doDiagAdvX ) THEN
c       diagName = 'ADVx'//diagSufx
c       CALL DIAGNOSTICS_FILL( afx, diagName, k,1, 2,bi,bj, myThid )
c      ENDIF
c      IF ( doDiagAdvY ) THEN
c       diagName = 'ADVy'//diagSufx
c       CALL DIAGNOSTICS_FILL( afy, diagName, k,1, 2,bi,bj, myThid )
c      ENDIF
c       IF ( doDiagAdvR ) THEN
c        diagName = 'ADVr'//diagSufx
c        CALL DIAGNOSTICS_FILL( fVerT(1-OLx,1-OLy,kUp),
c    &                          diagName, k,1, 2,bi,bj, myThid )
c       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_TRIADS_SCHEME */

      RETURN
      END
