#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_IMPLICIT_R
C     !INTERFACE:
      SUBROUTINE GAD_IMPLICIT_R(
     I      implicitAdvection, diagAdvVar,
     I      advectionScheme, tracerIdentity,
     I      deltaTLev, kappaRX, recip_hFac, wFld, tracer,
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
C diagAdvVar        :: calculate variance budget diagnostics
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
      LOGICAL implicitAdvection, diagAdvVar
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
C localTr   :: local copy of tracer (for Non-Lin Adv.Scheme)
C diagonalNumber :: number of non-zero diagonals in the matrix
C errCode   :: > 0 if singular matrix
C msgBuf    :: Informational/error message buffer
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
      _RL rTrans (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL localTr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     diagDif, diagAdv
      LOGICAL fwdSqGrad, bwdSqGrad
C--   Select time discretis. for Squared Gradient: Forward, Centered or Backward
      PARAMETER( fwdSqGrad = .TRUE. , bwdSqGrad = .FALSE. )
c     PARAMETER( fwdSqGrad = .FALSE., bwdSqGrad = .FALSE. )
c     PARAMETER( fwdSqGrad = .FALSE., bwdSqGrad = .TRUE.  )
      INTEGER km1, km2, kp1, kp2
      _RL df (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL af (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL div(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagTrK, diagTrKm, delTr, barTr, maskUp
      _RL diagVTR (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagVPR (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagSqD (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# ifdef SOLVE_DIAGONAL_LOWMEMORY
      CHARACTER*(MAX_LEN_MBUF) msgBuf
# endif /* SOLVE_DIAGONAL_LOWMEMORY */
#endif /* ALLOW_DIAGNOSTICS */
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

      IF (implicitDiffusion) THEN
C--   set the tri-diagonal matrix to solve the implicit diffusion problem
       diagonalNumber = 3
C-     1rst lower diagonal :
       DO k=2,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
           b5d(i,j,k) = -deltaTLev(k)*maskC(i,j,k-1,bi,bj)
     &                  *recip_hFac(i,j,k)*recip_drF(k)
     &                  *recip_deepFac2C(k)*recip_rhoFacC(k)
     &                  *kappaRX(i,j, k )*recip_drC( k )
     &                  *deepFac2F( k )*rhoFacF( k )
         ENDDO
        ENDDO
       ENDDO
C-     1rst upper diagonal :
       DO k=1,Nr-1
        DO j=jMin,jMax
         DO i=iMin,iMax
           d5d(i,j,k) = -deltaTLev(k)*maskC(i,j,k+1,bi,bj)
     &                  *recip_hFac(i,j,k)*recip_drF(k)
     &                  *recip_deepFac2C(k)*recip_rhoFacC(k)
     &                  *kappaRX(i,j,k+1)*recip_drC(k+1)
     &                  *deepFac2F(k+1)*rhoFacF(k+1)
         ENDDO
        ENDDO
       ENDDO
C-     Main diagonal :
       DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
           c5d(i,j,k) = 1. _d 0 - ( b5d(i,j,k) + d5d(i,j,k) )
C      to recover older (prior to 2016-10-05) results:
c          c5d(i,j,k) = 1. _d 0 - b5d(i,j,k) - d5d(i,j,k)
         ENDDO
        ENDDO
       ENDDO

C--   end if implicitDiffusion
      ENDIF

      IF (implicitAdvection) THEN

C--   Non-Linear Advection scheme: keep a local copy of tracer field
       IF ( advectionScheme.EQ.ENUM_FLUX_LIMIT .OR.
     &      advectionScheme.EQ.ENUM_DST3_FLUX_LIMIT ) THEN
        IF ( multiDimAdvection ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            localTr(i,j,k) = gTracer(i,j,k)
           ENDDO
          ENDDO
         ENDDO
        ELSE
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            localTr(i,j,k) = tracer(i,j,k)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
       ENDIF

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
     &                               *deepFac2F(k)*rhoFacF(k)
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
     I                        deltaTLev, rTrans, recip_hFac, localTr,
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
     I                        deltaTLev, rTrans, recip_hFac, localTr,
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

C--   Make a copy of tracer field before update (for diagnostics only)
      IF ( diagAdvVar ) THEN
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            localTr(i,j,k) = gTracer(i,j,k)
           ENDDO
          ENDDO
         ENDDO
      ENDIF

      IF ( diagonalNumber .EQ. 3 ) THEN
C--   Solve tri-diagonal system :
        errCode = -1
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
        errCode = -1
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

        IF ( diagDif .OR. diagAdv .OR. diagAdvVar ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            flx(i,j) = 0. _d 0
          ENDDO
         ENDDO
C--      start diagnostics k loop
         DO k= Nr,1,-1

          IF ( implicitDiffusion .AND. k.GE.2 ) THEN
            DO j=jMin,jMax
             DO i=iMin,iMax
               df(i,j) =
cc#ifdef ALLOW_AUTODIFF_OPENAD
cc     &             -rA(i,j,bi,bj)%v
cc#else
     &             -rA(i,j,bi,bj)*deepFac2F(k)*rhoFacF(k)
cc#endif
     &            * kappaRX(i,j,k)*recip_drC(k)*rkSign
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
             CALL LAYERS_FILL( df, tracerIdentity, 'DFR',
     &                           k, 1, 2,bi,bj, myThid )
           ENDIF
#endif /* ALLOW_LAYERS */
          ENDIF

          IF ( diagAdv .OR. ( diagAdvVar.AND.implicitAdvection ) ) THEN
#ifdef SOLVE_DIAGONAL_LOWMEMORY
           diagName = 'ADVr'//diagSufx
           WRITE(msgBuf,'(4A)') 'GAD_IMPLICIT_R: ',
     &      'unable to compute Diagnostic "', diagName, '" with'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(4A)') 'GAD_IMPLICIT_R: ',
     &      '#define SOLVE_DIAGONAL_LOWMEMORY (in CPP_OPTIONS.h)'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           STOP 'ABNORMAL END: S/R GAD_IMPLICIT_R'
#endif /* SOLVE_DIAGONAL_LOWMEMORY */
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
     &                - rkSign*div(i,j)*rA(i,j,bi,bj)
     &                        *deepFac2C(k)*rhoFacC(k)/deltaTLev(k)
cc#endif
              af(i,j) = flx(i,j) - df(i,j)
            ENDDO
           ENDDO
          ELSEIF ( diagAdvVar ) THEN
           DO j=jMin,jMax
            DO i=iMin,iMax
              flx(i,j) = df(i,j)
            ENDDO
           ENDDO
          ENDIF

          IF ( diagAdv ) THEN
           diagName = 'ADVr'//diagSufx
           CALL DIAGNOSTICS_FILL(af,diagName, k,1, 2,bi,bj, myThid)
#ifdef ALLOW_LAYERS
           IF ( useLayers ) THEN
             CALL LAYERS_FILL(af,tracerIdentity,'AFR',
     &                            k,1,2,bi,bj,myThid)
           ENDIF
#endif /* ALLOW_LAYERS */
          ENDIF

          IF ( diagAdvVar .AND. k.GE.2 ) THEN
           DO j=jMin,jMax
            DO i=iMin,iMax
              maskUp = 1.
              IF ( recip_hFac(i,j,k-1).EQ.zeroRS .OR.
     &             recip_hFac(i,j,k).EQ.zeroRS ) maskUp = 0.
C-    Calculate centered-in-time average of tracer
              diagTrKm = ( localTr(i,j,k-1) + gTracer(i,j,k-1) )*halfRl
              diagTrK  = ( localTr(i,j,k)   + gTracer(i,j,k)   )*halfRl
C-    Calculate Variance transport and production
              delTr = ( diagTrK - diagTrKm )*rkSign*maskUp
              barTr = ( diagTrK + diagTrKm )*halfRL
              diagVTR(i,j) = twoRL*flx(i,j)*barTr
              diagVPR(i,j) = twoRL*flx(i,j)*delTr
C-    Calculate Square of tracer gradient
              IF ( fwdSqGrad ) THEN
               delTr = ( localTr(i,j,k)
     &                 - localTr(i,j,k-1) )*rkSign*maskUp
              ELSEIF ( bwdSqGrad ) THEN
               delTr = ( gTracer(i,j,k)
     &                 - gTracer(i,j,k-1) )*rkSign*maskUp
              ENDIF
              diagSqD(i,j) = delTr*delTr*rA(i,j,bi,bj)
            ENDDO
           ENDDO
           diagName = 'VTRi'//diagSufx
           CALL DIAGNOSTICS_FILL( diagVTR,diagName,k,1,2,bi,bj,myThid)
           diagName = 'VPRi'//diagSufx
           CALL DIAGNOSTICS_FILL( diagVPR,diagName,k,1,2,bi,bj,myThid)
           diagName = 'SqDi'//diagSufx
           CALL DIAGNOSTICS_FILL( diagSqD,diagName,k,1,2,bi,bj,myThid)
          ENDIF

C--      end diagnostics k loop
         ENDDO
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   end if Nr > 1
      ENDIF

      RETURN
      END
