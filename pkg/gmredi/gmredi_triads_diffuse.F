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
C  sigmaX        :: density gradient, X component
C  sigmaY        :: density gradient, Y component
C  slope         :: isopycnal slope (no sign)
C  Kslope        :: Kappa x slope (no sign)
C  K_slp         :: tappered Kappa x slope (z-coord: same sign as sigmaX/Y ;
C                ::                         p-coord: sigmaX/Y opposite sign)
C  K_lim         :: tappered Kappa
C  dfx           :: 2-D array of horizontal diffusive flux in X-dir @ level k
C  dfy           :: 2-D array of horizontal diffusive flux in Y-dir @ level k
C  dfr           :: 3-D array of vertical diffusive flux (> 0 in r-direction)
C  KwzLoc        :: vertical diffusivity contribution from GM-Redi
C  flxDiv        :: temp array to holds horizontal flux divergence
C dTdx,dTdy,dTdr :: tracer gradient, 3 components
      INTEGER i, j, k, kl, kp1
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL K_slp (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL K_lim (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfx   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KwzLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL flxDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
c     _RL GM_bolK3d, K_skewFlx
      _RL GM_dR2_fact
      _RL dR_delT_lim, dR2delT_lim, half_dr
      _RL maskp5, mskP1, tmpFac
      _RL K_loc, slope, Kslope
      _RL dTdx, dTdy, dTdr
C     msgBuf     :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagDfX, doDiagDfY, doDiagDfR
      LOGICAL     doDiagDifX, doDiagDifY
      _RL KuxLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KvyLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KuzLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KvzLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KwxLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KwyLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_DIAGNOSTICS */
CEOP

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostics flags and suffix for the current tracer
      doDiagDfX = .FALSE.
      doDiagDfY = .FALSE.
      doDiagDfR = .FALSE.
      doDiagDifX = .FALSE.
      doDiagDifY = .FALSE.
      diagSufx = 'xxxx'
      IF ( useDiagnostics ) THEN
# ifdef ALLOW_GENERIC_ADVDIFF
       diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
# endif
       diagName = 'DFxE'//diagSufx
       doDiagDfX = DIAGNOSTICS_IS_ON( diagName, myThid )
       diagName = 'DFyE'//diagSufx
       doDiagDfY = DIAGNOSTICS_IS_ON( diagName, myThid )
       diagName = 'DFrE'//diagSufx
       doDiagDfR = DIAGNOSTICS_IS_ON( diagName, myThid )
       IF ( trIdentity.EQ.1 ) THEN
        doDiagDifX = DIAGNOSTICS_IS_ON( 'GM_Kux  ', myThid )
     &          .OR. DIAGNOSTICS_IS_ON( 'GM_Kuz  ', myThid )
     &          .OR. DIAGNOSTICS_IS_ON( 'GM_Kwx  ', myThid )
        doDiagDifY = DIAGNOSTICS_IS_ON( 'GM_Kvy  ', myThid )
     &          .OR. DIAGNOSTICS_IS_ON( 'GM_Kvz  ', myThid )
     &          .OR. DIAGNOSTICS_IS_ON( 'GM_Kwy  ', myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Set up work arrays with valid (i.e. not NaN) values
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         xA(i,j) = 0. _d 0
         yA(i,j) = 0. _d 0
         sigmaX(i,j) = 0. _d 0
         sigmaY(i,j) = 0. _d 0
         K_lim(i,j)  = 0. _d 0
         K_slp(i,j)  = 0. _d 0
#ifdef ALLOW_DIAGNOSTICS
         KwxLoc(i,j) = 0. _d 0
         KwyLoc(i,j) = 0. _d 0
#endif /* ALLOW_DIAGNOSTICS */
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dfr(i,j,k)    = 0. _d 0
         KwzLoc(i,j,k) = 0. _d 0
         flxDiv(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Stability criteria:
C        |slope|*Kgm*dt/(dx*dz) < 1/4* c_max
C   expressed as dt_max (GM_delT_lim):
C     if this is satisfied: |slope|*Kgm < 1/4 (dx*dz) / dt_max
C     then will just have to pick dt_max such as: dt < c_max * dt_max

C-    Safer to cap diffusivity but with a large enough cap to not matter:
      GM_dR2_fact = 1. _d +3

C-  Converting Griffies etal, JPO paper (MOM) grid variables to MITgcm var:
C      dxu --> dxC ; dxt --> dxF
C      dzw --> drC ; dzt --> drF
C   grid-weight factor from JPO paper: e.g. for triad #1,
C   contribution to dfx(i,j,k) is *drC(k) and at the end divide dfx by 4*drF(k)
C   contribution to dfr(i,j,k) is *dxC(i) and at the end divide dfr by 4*dxF(i)
C    and same for KappaR(i,j,k)
C-  Here last division is accounted in divergence of fluxes (units: [Tr]*m^3/s)
C   and we scale by a fraction of the grid-cell interface area, e.g., triad #1
C    contrib to dfx: *drC(k)*dyG(i,j)*deepFacF(k)
C    contrib to dfr: *rAw(i,j)*deepFac2F(k)
C   Note: scaling of dfr (& KwzLoc) by deepFac2F/2 (& 1/rA/2) is done at the end
C   Note for P-coords:
C    slope (=dSigmaX/dSigmaP) has same sign than in z-coords (upward direction),
C    but need to flip sign (-gravitySign in "K_slp") in extra-diagonal terrms:
C      dfx = -[ K*dTdx + (-K*Sp)*dTdp ]
C      dfr = -[ (-K*Sp)*dTdx + K*(Sp^2)*dTdp ]

C---  Start of k loop corresponding to horizontal fluxes
      DO k=1,Nr

C-    set-up local 2-D array within k loop
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dfx(i,j)    = 0. _d 0
         dfy(i,j)    = 0. _d 0
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( doDiagDifX .OR. doDiagDifY ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          KuxLoc(i,j) = 0. _d 0
          KuzLoc(i,j) = 0. _d 0
          KvyLoc(i,j) = 0. _d 0
          KvzLoc(i,j) = 0. _d 0
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   X-Z plane:

C-     density gradient in X-dir @ level k
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         sigmaX(i,j) =_maskW(i,j,k,bi,bj)
     &               *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &               *( rhoLoc(i,j,k) - rhoLoc(i-1,j,k) )
        ENDDO
       ENDDO

       IF ( k.GT.1 ) THEN
        kl = k
C-    for stability limit:
        dR_delT_lim = op25*drC(kl)*deepFacF(kl) / GM_delT_lim
        dR2delT_lim = op25*( drC(kl)*drC(kl)/GM_delT_lim )*GM_dR2_fact

        half_dr = rF(kl) - rC(k)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j) = _dyG(i,j,bi,bj)*deepFacF(kl)
     &            * half_dr*hFacW(i,j,k,bi,bj)
         ENDDO
        ENDDO

C--   triad #1 ; corner (i,k),              | Tr(i,k-1)
C                                Tr(i-1,k)  | Tr(i,k)
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskW(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
          slope = ABS( sigmaX(i,j)*GM_Nm2(i,j,kl,bi,bj) )
          K_loc = GM_isoK3d(i,j,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dxC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaX(i,j) )
c          K_skewFlx = GM_skewflx*SIGN( GM_bolK3d*slope, sigmaX(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
c          K_skewFlx =  0. _d 0
          ENDIF
          dTdx = ( tracer(i,j,k) - tracer(i-1,j,k) )
     &         * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i,j,k) - tracer(i,j,k-1) )
     &         * recip_drC(kl)*rkSign
          dfx(i,j) = dfx(i,j)
     &               - ( K_lim(i,j)*dTdx
     &                 + K_slp(i,j)*dTdr
c    &                 - K_skewFlx*dTdr
     &                 )*xA(i,j)
          dfr(i,j,kl) = dfr(i,j,kl)
     &               - ( K_slp(i,j)*dTdx
c    &                 + K_skewFlx*dTdx
     &                 )*rAw(i,j,bi,bj)
          KwzLoc(i,j,kl) = KwzLoc(i,j,kl)
     &               + Kslope*slope*rAw(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifX ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KuxLoc(i,j) = KuxLoc(i,j) + K_lim(i,j)*half_dr
           KuzLoc(i,j) = KuzLoc(i,j) + K_slp(i,j)*half_dr
           KwxLoc(i,j) = KwxLoc(i,j) + K_slp(i,j)*rAw(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   triad #2 : corner (i,k),   Tr(i-1,k-1)|
C                                Tr(i-1,k)  | Tr(i,k)
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskW(i,j,k,bi,bj)*maskC(i-1,j,k-1,bi,bj)
          slope = ABS( sigmaX(i,j)*GM_Nm2(i-1,j,kl,bi,bj) )
          K_loc = GM_isoK3d(i-1,j,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dxC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaX(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
          ENDIF
          dTdx = ( tracer(i,j,k) - tracer(i-1,j,k) )
     &         * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i-1,j,k) - tracer(i-1,j,k-1) )
     &         * recip_drC(kl)*rkSign
          dfx(i,j) = dfx(i,j)
     &               - ( K_lim(i,j)*dTdx
     &                 + K_slp(i,j)*dTdr
     &                 )*xA(i,j)
          dfr(i-1,j,kl) = dfr(i-1,j,kl)
     &               - ( K_slp(i,j)*dTdx
     &                 )*rAw(i,j,bi,bj)
          KwzLoc(i-1,j,kl) = KwzLoc(i-1,j,kl)
     &               + Kslope*slope*rAw(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifX ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KuxLoc(i,j) = KuxLoc(i,j) + K_lim(i,j)*half_dr
           KuzLoc(i,j) = KuzLoc(i,j) + K_slp(i,j)*half_dr
           KwxLoc(i-1,j) = KwxLoc(i-1,j) + K_slp(i,j)*rAw(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KwxLoc(i,j) = KwxLoc(i,j)*recip_rA(i,j,bi,bj)*halfRL
          ENDDO
         ENDDO
C-       fill diagnostics with KwxLoc
         CALL DIAGNOSTICS_FILL( KwxLoc, 'GM_Kwx  ',
     &                          k, 1, 2, bi, bj, myThid )
         IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT( 'GM_Kwx  ',
     &                                   bi, bj, myThid )
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KwxLoc(i,j) = 0.
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     end if k > 1
       ENDIF

       IF ( k.LT.Nr ) THEN
        kl = k+1
C-    for stability limit:
        dR_delT_lim = op25*drC(kl)*deepFacF(kl) / GM_delT_lim
        dR2delT_lim = op25*( drC(kl)*drC(kl)/GM_delT_lim )*GM_dR2_fact

        half_dr = rC(k) - rF(kl)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j) = _dyG(i,j,bi,bj)*deepFacF(kl)
     &            * half_dr*hFacW(i,j,k,bi,bj)
         ENDDO
        ENDDO

C--   triad #3 : corner (i,k+1), Tr(i-1,k)  | Tr(i,k)
C                                Tr(i-1,k+1)|
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskW(i,j,k,bi,bj)*maskC(i-1,j,k+1,bi,bj)
          slope = ABS( sigmaX(i,j)*GM_Nm2(i-1,j,kl,bi,bj) )
          K_loc = GM_isoK3d(i-1,j,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dxC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaX(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
          ENDIF
          dTdx = ( tracer(i,j,k) - tracer(i-1,j,k) )
     &         * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i-1,j,k+1) - tracer(i-1,j,k) )
     &         * recip_drC(kl)*rkSign
          dfx(i,j) = dfx(i,j)
     &               - ( K_lim(i,j)*dTdx
     &                 + K_slp(i,j)*dTdr
     &                 )*xA(i,j)
          dfr(i-1,j,kl) = dfr(i-1,j,kl)
     &               - ( K_slp(i,j)*dTdx
     &                 )*rAw(i,j,bi,bj)
          KwzLoc(i-1,j,kl) = KwzLoc(i-1,j,kl)
     &               + Kslope*slope*rAw(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifX ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KuxLoc(i,j) = KuxLoc(i,j) + K_lim(i,j)*half_dr
           KuzLoc(i,j) = KuzLoc(i,j) + K_slp(i,j)*half_dr
           KwxLoc(i-1,j) = KwxLoc(i-1,j) + K_slp(i,j)*rAw(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   triad #4 : corner (i,k+1), Tr(i-1,k)  | Tr(i,k)
C                                           | Tr(i,k+1)
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskW(i,j,k,bi,bj)*maskC(i,j,k+1,bi,bj)
          slope = ABS( sigmaX(i,j)*GM_Nm2(i,j,kl,bi,bj) )
          K_loc = GM_isoK3d(i,j,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dxC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaX(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
          ENDIF
          dTdx = ( tracer(i,j,k) - tracer(i-1,j,k) )
     &         * _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i,j,k+1) - tracer(i,j,k) )
     &         * recip_drC(kl)*rkSign
          dfx(i,j) = dfx(i,j)
     &               - ( K_lim(i,j)*dTdx
     &                 + K_slp(i,j)*dTdr
     &                 )*xA(i,j)
          dfr(i,j,kl) = dfr(i,j,kl)
     &               - ( K_slp(i,j)*dTdx
     &                 )*rAw(i,j,bi,bj)
          KwzLoc(i,j,kl) = KwzLoc(i,j,kl)
     &               + Kslope*slope*rAw(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifX ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KuxLoc(i,j) = KuxLoc(i,j) + K_lim(i,j)*half_dr
           KuzLoc(i,j) = KuzLoc(i,j) + K_slp(i,j)*half_dr
           KwxLoc(i,j) = KwxLoc(i,j) + K_slp(i,j)*rAw(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     end if k < Nr
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Y-Z plane:

C-     density gradient in Y-dir @ level k
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         sigmaY(i,j) =_maskS(i,j,k,bi,bj)
     &               *_recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &               *( rhoLoc(i,j,k) - rhoLoc(i,j-1,k) )
        ENDDO
       ENDDO

       IF ( k.GT.1 ) THEN
        kl = k
C-    for stability limit:
        dR_delT_lim = op25*drC(kl)*deepFacF(kl) / GM_delT_lim
        dR2delT_lim = op25*( drC(kl)*drC(kl)/GM_delT_lim )*GM_dR2_fact

        half_dr = rF(kl) - rC(k)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          yA(i,j) = _dxG(i,j,bi,bj)*deepFacF(kl)
     &            * half_dr*hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C--   triad #1 ; corner (j,k),              | Tr(j,k-1)
C                                Tr(j-1,k)  | Tr(j,k)
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskS(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
          slope = ABS( sigmaY(i,j)*GM_Nm2(i,j,kl,bi,bj) )
          K_loc = GM_isoK3d(i,j,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dyC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaY(i,j) )
c          K_skewFlx = GM_skewflx*SIGN( GM_bolK3d*slope, sigmaY(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
c          K_skewFlx =  0. _d 0
          ENDIF
          dTdy = ( tracer(i,j,k) - tracer(i,j-1,k) )
     &         * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i,j,k) - tracer(i,j,k-1) )
     &         * recip_drC(kl)*rkSign
          dfy(i,j) = dfy(i,j)
     &               - ( K_lim(i,j)*dTdy
     &                 + K_slp(i,j)*dTdr
c    &                 - K_skewFlx*dTdr
     &                 )*yA(i,j)
          dfr(i,j,kl) = dfr(i,j,kl)
     &               - ( K_slp(i,j)*dTdy
c    &                 + K_skewFlx*dTdy
     &                 )*rAs(i,j,bi,bj)
          KwzLoc(i,j,kl) = KwzLoc(i,j,kl)
     &               + Kslope*slope*rAs(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifY ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KvyLoc(i,j) = KvyLoc(i,j) + K_lim(i,j)*half_dr
           KvzLoc(i,j) = KvzLoc(i,j) + K_slp(i,j)*half_dr
           KwyLoc(i,j) = KwyLoc(i,j) + K_slp(i,j)*rAs(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   triad #2 : corner (j,k),   Tr(j-1,k-1)|
C                                Tr(j-1,k)  | Tr(j,k)
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskS(i,j,k,bi,bj)*maskC(i,j-1,k-1,bi,bj)
          slope = ABS( sigmaY(i,j)*GM_Nm2(i,j-1,kl,bi,bj) )
          K_loc = GM_isoK3d(i,j-1,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dyC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaY(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
          ENDIF
          dTdy = ( tracer(i,j,k) - tracer(i,j-1,k) )
     &         * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i,j-1,k) - tracer(i,j-1,k-1) )
     &         * recip_drC(kl)*rkSign
          dfy(i,j) = dfy(i,j)
     &               - ( K_lim(i,j)*dTdy
     &                 + K_slp(i,j)*dTdr
     &                 )*yA(i,j)
          dfr(i,j-1,kl) = dfr(i,j-1,kl)
     &               - ( K_slp(i,j)*dTdy
     &                 )*rAs(i,j,bi,bj)
          KwzLoc(i,j-1,kl) = KwzLoc(i,j-1,kl)
     &               + Kslope*slope*rAs(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifY ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KvyLoc(i,j) = KvyLoc(i,j) + K_lim(i,j)*half_dr
           KvzLoc(i,j) = KvzLoc(i,j) + K_slp(i,j)*half_dr
           KwyLoc(i,j-1) = KwyLoc(i,j-1) + K_slp(i,j)*rAs(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KwyLoc(i,j) = KwyLoc(i,j)*recip_rA(i,j,bi,bj)*halfRL
          ENDDO
         ENDDO
C-       fill diagnostics with KwyLoc
         CALL DIAGNOSTICS_FILL( KwyLoc, 'GM_Kwy  ',
     &                          k, 1, 2, bi, bj, myThid )
         IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT( 'GM_Kwy  ',
     &                                   bi, bj, myThid )
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KwyLoc(i,j) = 0.
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     end if k > 1
       ENDIF

       IF ( k.LT.Nr ) THEN
        kl = k+1
C-    for stability limit:
        dR_delT_lim = op25*drC(kl)*deepFacF(kl) / GM_delT_lim
        dR2delT_lim = op25*( drC(kl)*drC(kl)/GM_delT_lim )*GM_dR2_fact

        half_dr = rC(k) - rF(kl)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          yA(i,j) = _dxG(i,j,bi,bj)*deepFacF(kl)
     &            * half_dr*hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C--   triad #3 : corner (j,k+1), Tr(j-1,k)  | Tr(j,k)
C                                Tr(j-1,k+1)|
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskS(i,j,k,bi,bj)*maskC(i,j-1,k+1,bi,bj)
          slope = ABS( sigmaY(i,j)*GM_Nm2(i,j-1,kl,bi,bj) )
          K_loc = GM_isoK3d(i,j-1,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dyC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaY(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
          ENDIF
          dTdy = ( tracer(i,j,k) - tracer(i,j-1,k) )
     &         * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i,j-1,k+1) - tracer(i,j-1,k) )
     &         * recip_drC(kl)*rkSign
          dfy(i,j) = dfy(i,j)
     &               - ( K_lim(i,j)*dTdy
     &                 + K_slp(i,j)*dTdr
     &                 )*yA(i,j)
          dfr(i,j-1,kl) = dfr(i,j-1,kl)
     &               - ( K_slp(i,j)*dTdy
     &                 )*rAs(i,j,bi,bj)
          KwzLoc(i,j-1,kl) = KwzLoc(i,j-1,kl)
     &               + Kslope*slope*rAs(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifY ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KvyLoc(i,j) = KvyLoc(i,j) + K_lim(i,j)*half_dr
           KvzLoc(i,j) = KvzLoc(i,j) + K_slp(i,j)*half_dr
           KwyLoc(i,j-1) = KwyLoc(i,j-1) + K_slp(i,j)*rAs(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   triad #4 : corner (j,k+1), Tr(j-1,k)  | Tr(j,k)
C                                           | Tr(j,k+1)
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
C-    GM_Nm2 (N^-2) holds the inverse of "gravitySign*sigmaR"
          maskp5 = halfRS*maskS(i,j,k,bi,bj)*maskC(i,j,k+1,bi,bj)
          slope = ABS( sigmaY(i,j)*GM_Nm2(i,j,kl,bi,bj) )
          K_loc = GM_isoK3d(i,j,k,bi,bj)
          IF ( slope.GT.zeroRL ) THEN
C-     Stability limit:
           Kslope = maskp5*MIN( K_loc*slope, dR2delT_lim/slope,
     &                          dyC(i,j,bi,bj)*dR_delT_lim )
           K_lim(i,j) = Kslope / slope
           K_slp(i,j) = -gravitySign*SIGN( Kslope, sigmaY(i,j) )
          ELSE
           Kslope = 0. _d 0
           K_lim(i,j) = K_loc*maskp5
           K_slp(i,j) = 0. _d 0
          ENDIF
          dTdy = ( tracer(i,j,k) - tracer(i,j-1,k) )
     &         * _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
          dTdr = ( tracer(i,j,k+1) - tracer(i,j,k) )
     &         * recip_drC(kl)*rkSign
          dfy(i,j) = dfy(i,j)
     &               - ( K_lim(i,j)*dTdy
     &                 + K_slp(i,j)*dTdr
     &                 )*yA(i,j)
          dfr(i,j,kl) = dfr(i,j,kl)
     &               - ( K_slp(i,j)*dTdy
     &                 )*rAs(i,j,bi,bj)
          KwzLoc(i,j,kl) = KwzLoc(i,j,kl)
     &               + Kslope*slope*rAs(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( doDiagDifY ) THEN
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           KvyLoc(i,j) = KvyLoc(i,j) + K_lim(i,j)*half_dr
           KvzLoc(i,j) = KvzLoc(i,j) + K_slp(i,j)*half_dr
           KwyLoc(i,j) = KwyLoc(i,j) + K_slp(i,j)*rAs(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     end if k < Nr
       ENDIF

C--   end Y-Z plane.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Save horizontal flux divergence:
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
          flxDiv(i,j,k) = ( dfx(i+1,j) - dfx(i,j) )
     &                  + ( dfy(i,j+1) - dfy(i,j) )
        ENDDO
       ENDDO
C--   Add contribution to vertical diffusivity:
C     note: could move this in its own k loop
       tmpFac = deepFac2F(k)*halfRL*rhoFacF(k)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          dfr(i,j,k) = dfr(i,j,k)*tmpFac
          KwzLoc(i,j,k) = KwzLoc(i,j,k)*recip_rA(i,j,bi,bj)*halfRL
          kappaRk(i,j,k) = kappaRk(i,j,k) + KwzLoc(i,j,k)
        ENDDO
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
       IF ( doDiagDifX ) THEN
        tmpFac = recip_drF(k)
        CALL DIAGNOSTICS_SCALE_FILL( KuxLoc, tmpFac, 1, 'GM_Kux  ',
     &                               k, 1, 2, bi, bj, myThid )
        CALL DIAGNOSTICS_SCALE_FILL( KuzLoc, tmpFac, 1, 'GM_Kuz  ',
     &                               k, 1, 2, bi, bj, myThid )
       ENDIF
       IF ( doDiagDifY ) THEN
        tmpFac = recip_drF(k)
        CALL DIAGNOSTICS_SCALE_FILL( KvyLoc, tmpFac, 1, 'GM_Kvy  ',
     &                               k, 1, 2, bi, bj, myThid )
        CALL DIAGNOSTICS_SCALE_FILL( KvzLoc, tmpFac, 1, 'GM_Kvz  ',
     &                               k, 1, 2, bi, bj, myThid )
       ENDIF
       IF ( doDiagDfX ) THEN
        diagName = 'DFxE'//diagSufx
        CALL DIAGNOSTICS_FILL( dfx, diagName, k,1, -2,bi,bj, myThid )
       ENDIF
       IF ( doDiagDfY ) THEN
        diagName = 'DFyE'//diagSufx
        CALL DIAGNOSTICS_FILL( dfy, diagName, k,1, -2,bi,bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

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
     &        *( flxDiv(i,j,k)
     &         + ( dfr(i,j,kp1)*mskP1 - dfr(i,j,k) )*rkSign
     &           *recip_rhoFacC(k)
     &         )*maskInC(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. trIdentity.EQ.1 ) THEN
        CALL DIAGNOSTICS_FILL( KwzLoc, 'GM_Kwz  ',
     &                         0, Nr, 2, bi, bj, myThid )
      ENDIF
      IF ( doDiagDfR ) THEN
        diagName = 'DFrE'//diagSufx
        CALL DIAGNOSTICS_FILL( dfr, diagName,
     &                         0, Nr, -2, bi, bj, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_TRIADS_SCHEME */

      RETURN
      END
