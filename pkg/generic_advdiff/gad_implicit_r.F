C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_implicit_r.F,v 1.22 2014/08/14 16:46:36 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_IMPLICIT_R
C     !INTERFACE:
      SUBROUTINE GAD_IMPLICIT_R(
     I      implicitAdvection, advectionScheme, tracerIdentity,
     I      deltaTLev,
     I      kappaRX, recip_hFac, wFld, tracer,
     U      gTracer,
     I      bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION:
C     Solve implicitly vertical advection and diffusion for one tracer.

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "GAD.h"

C !INPUT/OUTPUT PARAMETERS:
C == Routine Arguments ==
C implicitAdvection :: if True, treat vertical advection implicitly
C advectionScheme   :: advection scheme to use
C tracerIdentity    :: Identifier for the tracer
C kappaRX           :: 3-D array for vertical diffusion coefficient
C recip_hFac        :: inverse of cell open-depth factor
C wFld              :: Advection velocity field, vertical component
C tracer            :: tracer field at current time step
C gTracer           :: future tracer field
C bi,bj             :: tile indices
C myTime            :: current time
C myIter            :: current iteration number
C myThid            :: thread number
      LOGICAL implicitAdvection
      INTEGER advectionScheme
      INTEGER tracerIdentity
      _RL     deltaTLev(Nr)
      _RL kappaRX   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTracer   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter, myThid

#ifdef ALLOW_DIAGNOSTICS
C !FUNCTIONS:
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
      LOGICAL     DIAGNOSTICS_IS_ON
      EXTERNAL    DIAGNOSTICS_IS_ON
#endif

C !LOCAL VARIABLES:
C == Local variables ==
C iMin,iMax,jMin,jMax :: computational domain
C i,j,k     :: loop indices
C a5d       :: 2nd  lower diagonal of the pentadiagonal matrix
C b5d       :: 1rst lower diagonal of the pentadiagonal matrix
C c5d       :: main diagonal       of the pentadiagonal matrix
C d5d       :: 1rst upper diagonal of the pentadiagonal matrix
C e5d       :: 2nd  upper diagonal of the pentadiagonal matrix
C rTrans    :: vertical volume transport at interface k
C localTijk :: local copy of tracer (for Non-Lin Adv.Scheme)
C diagonalNumber :: number of non-zero diagonals in the matrix
C errCode   :: > 0 if singular matrix
      INTEGER iMin,iMax,jMin,jMax
      PARAMETER( iMin = 1, iMax = sNx )
      PARAMETER( jMin = 1, jMax = sNy )
      INTEGER i,j,k
      INTEGER diagonalNumber, errCode
      _RL a5d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b5d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c5d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL d5d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL e5d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rTrans   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localTijk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     diagDif, diagAdv
      INTEGER km1, km2, kp1, kp2
      _RL df (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL div(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
CEOP

C--   no need to solve anything with only 1 level:
      IF (Nr.GT.1) THEN

C--   Initialise
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         a5d(i,j,k) = 0. _d 0
         b5d(i,j,k) = 0. _d 0
         c5d(i,j,k) = 1. _d 0
         d5d(i,j,k) = 0. _d 0
         e5d(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
      diagonalNumber = 1

C--   Non-Linear Advection scheme: keep a local copy of tracer field
      IF ( advectionScheme.EQ.ENUM_FLUX_LIMIT .OR.
     &     advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
        IF ( multiDimAdvection ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            localTijk(i,j,k) = gTracer(i,j,k)
           ENDDO
          ENDDO
         ENDDO
        ELSE
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            localTijk(i,j,k) = tracer(i,j,k)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
      ENDIF

      IF (implicitDiffusion) THEN
C--   set the tri-diagonal matrix to solve the implicit diffusion problem
       diagonalNumber = 3
C-     1rst lower diagonal :
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
           b5d(i,j,k) = -deltaTLev(k)*maskC(i,j,k-1,bi,bj)
     &                  *recip_hFac(i,j,k)*recip_drF(k)
     &                  *kappaRX(i,j, k )*recip_drC( k )
         ENDDO
        ENDDO
       ENDDO
C-     1rst upper diagonal :
       DO k=1,Nr-1
        DO j=jMin,jMax
         DO i=iMin,iMax
           d5d(i,j,k) = -deltaTLev(k)*maskC(i,j,k+1,bi,bj)
     &                 *recip_hFac(i,j,k)*recip_drF(k)
     &                 *KappaRX(i,j,k+1)*recip_drC(k+1)
         ENDDO
        ENDDO
       ENDDO
C-     Main diagonal :
       DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
           c5d(i,j,k) = 1. _d 0 - b5d(i,j,k) - d5d(i,j,k)
         ENDDO
        ENDDO
       ENDDO

C--   end if implicitDiffusion
      ENDIF

      IF (implicitAdvection) THEN

       DO k=Nr,1,-1

C--    Compute transport
        IF (k.EQ.1) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            rTrans(i,j) = 0. _d 0
          ENDDO
         ENDDO
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            rTrans(i,j) = wFld(i,j,k)*rA(i,j,bi,bj)
     &                               *maskC(i,j,k-1,bi,bj)
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_AIM
C- a hack to prevent Water-Vapor vert.transport into the stratospheric level Nr
        IF ( k.GE.2 .AND.
     &     (.NOT.useAIM .OR.tracerIdentity.NE.GAD_SALINITY .OR.k.LT.Nr)
     &              ) THEN
#else
        IF ( k.GE.2 ) THEN
#endif

         IF ( advectionScheme.EQ.ENUM_CENTERED_2ND ) THEN
          diagonalNumber = 3
          CALL GAD_C2_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        deltaTLev, rTrans, recip_hFac,
     U                        b5d, c5d, d5d,
     I                        myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_1RST
     &       .OR. advectionScheme.EQ.ENUM_DST2 ) THEN
          diagonalNumber = 3
          CALL GAD_DST2U1_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        advectionScheme, deltaTLev,
     I                        rTrans, recip_hFac,
     U                        b5d, c5d, d5d,
     I                        myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_FLUX_LIMIT ) THEN
          diagonalNumber = 3
          CALL GAD_FLUXLIMIT_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        deltaTLev, rTrans, recip_hFac, localTijk,
     U                        b5d, c5d, d5d,
     I                        myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_UPWIND_3RD
     &       .OR. advectionScheme.EQ.ENUM_CENTERED_4TH
     &       .OR. advectionScheme.EQ.ENUM_DST3 ) THEN
          diagonalNumber = 5
          CALL GAD_U3C4_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        advectionScheme, deltaTLev,
     I                        rTrans, recip_hFac,
     U                        a5d, b5d, c5d, d5d, e5d,
     I                        myThid )
         ELSEIF ( advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
          diagonalNumber = 5
          CALL GAD_DST3FL_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        deltaTLev, rTrans, recip_hFac, localTijk,
     U                        a5d, b5d, c5d, d5d, e5d,
     I                        myThid )
         ELSE
          STOP 'GAD_IMPLICIT_R: Adv.Scheme in Impl form not yet coded'
         ENDIF

        ENDIF

C--     end k loop
       ENDDO

C--   end if implicitAdvection
      ENDIF

      IF ( diagonalNumber .EQ. 3 ) THEN
C--   Solve tri-diagonal system :
        CALL SOLVE_TRIDIAGONAL( iMin,iMax, jMin,jMax,
     I                          b5d, c5d, d5d,
     U                          gTracer,
     O                          errCode,
     I                          bi, bj, myThid )
        IF (errCode.GE.1) THEN
          STOP 'GAD_IMPLICIT_R: error when solving 3-Diag problem'
        ENDIF
      ELSEIF ( diagonalNumber .EQ. 5 ) THEN
C--   Solve penta-diagonal system :
        CALL SOLVE_PENTADIAGONAL( iMin,iMax, jMin,jMax,
     I                            a5d, b5d, c5d, d5d, e5d,
     U                            gTracer,
     O                            errCode,
     I                            bi, bj, myThid )
        IF (errCode.GE.1) THEN
          STOP 'GAD_IMPLICIT_R: error when solving 5-Diag problem'
        ENDIF
      ELSEIF ( diagonalNumber .NE. 1 ) THEN
        STOP 'GAD_IMPLICIT_R: no solver available'
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostic suffix for the current tracer
      IF ( useDiagnostics ) THEN
        diagSufx = GAD_DIAG_SUFX( tracerIdentity, myThid )
        diagName = 'DFrI'//diagSufx
        diagDif = implicitDiffusion
        IF ( diagDif ) diagDif = DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'ADVr'//diagSufx
        diagAdv = implicitAdvection
        IF ( diagAdv ) diagAdv = DIAGNOSTICS_IS_ON(diagName,myThid)

        IF ( diagDif .OR. diagAdv ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            flx(i,j) = 0. _d 0
          ENDDO
         ENDDO
         DO k= Nr,1,-1
          IF ( implicitDiffusion .AND. k.GE.2 ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
               df(i,j) =
cc#ifdef ALLOW_AUTODIFF_OPENAD
cc     &             -rA(i,j,bi,bj)%v
cc#else
     &             -rA(i,j,bi,bj)
cc#endif
     &            * KappaRX(i,j,k)*recip_drC(k)*rkSign
     &            * (gTracer(i,j,k) - gTracer(i,j,k-1))
     &            * maskC(i,j,k,bi,bj)
     &            * maskC(i,j,k-1,bi,bj)
             ENDDO
            ENDDO
          ELSE
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               df(i,j) = 0. _d 0
             ENDDO
            ENDDO
          ENDIF
C-  Note: Needs to explicitly increment counter (call DIAGNOSTICS_COUNT)
C         since skipping k=1 DIAGNOSTICS_FILL call.
          IF ( diagDif .AND. k.GE.2 ) THEN
           diagName = 'DFrI'//diagSufx
           CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
           IF ( k.EQ.2 ) CALL DIAGNOSTICS_COUNT(diagName,bi,bj,myThid)
#ifdef ALLOW_LAYERS
           IF ( useLayers ) THEN
            IF ( (tracerIdentity.EQ.1).OR.(tracerIdentity.EQ.2) ) THEN
             CALL LAYERS_FILL_DFR( df, tracerIdentity,
     &                           k, 1, 2,bi,bj, myThid )
            ENDIF
           ENDIF
#endif /* ALLOW_LAYERS */
          ENDIF
          IF ( diagAdv ) THEN
           km1=MAX(1,k-1)
           km2=MAX(1,k-2)
           kp1=MIN(Nr,k+1)
           kp2=MIN(Nr,k+2)
C--   Flux_divergence*deltaT = Tr^n - Tr^n+1 = [A-I](Tr^n+1)
C                            = deltaT*rkSign*[ Flx_k+1 - Flx_k ]/dz
           DO j=jMin,jMax
            DO i=iMin,iMax
              div(i,j) = gTracer(i,j,k)*( c5d(i,j,k) - 1. _d 0 )
     &                 + gTracer(i,j,km1)*b5d(i,j,k)
     &                 + gTracer(i,j,kp1)*d5d(i,j,k)
            ENDDO
           ENDDO
           IF ( diagonalNumber .EQ. 5 ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
              div(i,j) = div(i,j)
     &                 + gTracer(i,j,km2)*a5d(i,j,k)
     &                 + gTracer(i,j,kp2)*e5d(i,j,k)
             ENDDO
            ENDDO
           ENDIF
#ifdef NONLIN_FRSURF
           IF ( nonlinFreeSurf.GT.0 ) THEN
C--    use future hFac to stay consistent with solver matrix
            IF ( select_rStar.GT.0 ) THEN
             DO j=jMin,jMax
              DO i=iMin,iMax
               div(i,j) = div(i,j)*h0FacC(i,j,k,bi,bj)*drF(k)
     &                            *rStarFacC(i,j,bi,bj)
              ENDDO
             ENDDO
            ELSEIF ( selectSigmaCoord.NE.0 ) THEN
             DO j=jMin,jMax
              DO i=iMin,iMax
               div(i,j) = div(i,j)*(
     &               _hFacC(i,j,k,bi,bj)*drF(k)
     &              + dBHybSigF(k)*dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
     &                             )
              ENDDO
             ENDDO
            ELSE
             DO j=jMin,jMax
              DO i=iMin,iMax
               IF ( k.EQ.kSurfC(i,j,bi,bj) ) THEN
                div(i,j) = div(i,j)*hFac_surfC(i,j,bi,bj)*drF(k)
               ELSE
                div(i,j) = div(i,j)*_hFacC(i,j,k,bi,bj)*drF(k)
               ENDIF
              ENDDO
             ENDDO
            ENDIF
           ELSE
#else /* NONLIN_FRSURF */
           IF ( .TRUE. ) THEN
#endif /* NONLIN_FRSURF */
C--    use current hFac (consistent with solver matrix)
             DO j=jMin,jMax
              DO i=iMin,iMax
               div(i,j) = div(i,j)*_hFacC(i,j,k,bi,bj)*drF(k)
              ENDDO
             ENDDO
           ENDIF
           DO j=jMin,jMax
            DO i=iMin,iMax
              flx(i,j) = flx(i,j)
cc#ifdef ALLOW_AUTODIFF_OPENAD
cc     &                - rkSign*div(i,j)*rA(i,j,bi,bj)%v/deltaTLev(k)
cc#else
     &                - rkSign*div(i,j)*rA(i,j,bi,bj)/deltaTLev(k)
cc#endif
              af(i,j) = flx(i,j) - df(i,j)
            ENDDO
           ENDDO
           diagName = 'ADVr'//diagSufx
           CALL DIAGNOSTICS_FILL(af,diagName, k,1, 2,bi,bj, myThid)
          ENDIF
         ENDDO
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   end if Nr > 1
      ENDIF

      RETURN
      END
