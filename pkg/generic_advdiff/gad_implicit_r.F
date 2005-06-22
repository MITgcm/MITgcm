C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_implicit_r.F,v 1.10 2005/06/22 00:27:47 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_IMPLICIT_R
C     !INTERFACE:
      SUBROUTINE GAD_IMPLICIT_R( 
     I      implicitAdvection, advectionScheme, tracerIdentity,
     I      kappaRX, wVel, tracer, 
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
#include "GAD.h"

C !INPUT/OUTPUT PARAMETERS:
C == Routine Arguments ==
C implicitAdvection :: if True, treat vertical advection implicitly
C advectionScheme   :: advection scheme to use
C tracerIdentity    :: Identifier for the tracer
C kappaRX           :: 3-D array for vertical diffusion coefficient
C wVel              :: vertical component of the velcity field
C tracer            :: tracer field at current time step
C gTracer           :: future tracer field
C bi,bj             :: tile indices
C myTime            :: current time
C myIter            :: current iteration number
C myThid            :: thread number
      LOGICAL implicitAdvection
      INTEGER advectionScheme
      INTEGER tracerIdentity
      _RL kappaRX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL wVel   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL tracer (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL gTracer(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter, myThid

C !LOCAL VARIABLES:
C == Local variables ==
C iMin,iMax,jMin,jMax  :: computational domain
C i,j,k  :: loop indices
C a5d    :: 2nd  lower diagonal of the pentadiagonal matrix
C b5d    :: 1rst lower diagonal of the pentadiagonal matrix
C c5d    :: main diagonal       of the pentadiagonal matrix
C d5d    :: 1rst upper diagonal of the pentadiagonal matrix
C e5d    :: 2nd  upper diagonal of the pentadiagonal matrix
C rTrans    :: vertical volume transport at inteface k
C rTransKp1 :: vertical volume transport at inteface k+1
C localTijk :: local copy of tracer (for Non-Lin Adv.Scheme)
C diagonalNumber :: number of non-zero diagonals in the matrix
C errCode   :: > 0 if singular matrix
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i,j,k
      INTEGER diagonalNumber, errCode
      _RL a5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL d5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL e5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL rTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rTransKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL localTijk(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 GAD_DIAG_SUFX, diagSufx
      EXTERNAL    GAD_DIAG_SUFX
      LOGICAL     DIAGNOSTICS_IS_ON
      EXTERNAL    DIAGNOSTICS_IS_ON
      _RL df (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#endif
CEOP

C--   no need to solve anything with only 1 level:
      IF (Nr.GT.1) THEN

C--   Initialise
      iMin = 1
      jMin = 1
      iMax = sNx
      jMax = sNy
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
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
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            localTijk(i,j,k) = gTracer(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSE
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            localTijk(i,j,k) = tracer(i,j,k,bi,bj)
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
           b5d(i,j,k) = -dTtracerLev(k)*maskC(i,j,k-1,bi,bj)
     &                  *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &                  *kappaRX(i,j, k )*recip_drC( k )
         ENDDO
        ENDDO
       ENDDO
C-     1rst upper diagonal :
       DO k=1,Nr-1
        DO j=jMin,jMax
         DO i=iMin,iMax
           d5d(i,j,k) = -dTtracerLev(k)*maskC(i,j,k+1,bi,bj)
     &                 *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
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
        IF (k.EQ.Nr) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rTransKp1(i,j) = 0.
          ENDDO
         ENDDO
        ELSE
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rTransKp1(i,j) = rTrans(i,j)
          ENDDO
         ENDDO
        ENDIF

        IF (k.EQ.1) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rTrans(i,j) = 0.
          ENDDO
         ENDDO
        ELSE
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rTrans(i,j) = wVel(i,j,k,bi,bj)*rA(i,j,bi,bj)
     &                   *maskC(i,j,k-1,bi,bj)
          ENDDO
         ENDDO
#ifdef ALLOW_GMREDI
C--   Residual transp = Bolus transp + Eulerian transp
          IF (useGMRedi)
     &      CALL GMREDI_CALC_WFLOW(
     &                    rTrans, bi, bj, k, myThid)
#endif /* ALLOW_GMREDI */
        ENDIF
        DO j=jMin,jMax
          DO i=iMin,iMax
c          localTijk(i,j,k)     = gTracer(i,j,k,bi,bj) 
           gTracer(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
     &      + dTtracerLev(1)*recip_rA(i,j,bi,bj)
     &       *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &       *tracer(i,j,k,bi,bj)*(rTransKp1(i,j)-rTrans(i,j))*rkSign
          ENDDO
        ENDDO

#ifdef ALLOW_AIM
C- a hack to prevent Water-Vapor vert.transport into the stratospheric level Nr
        IF ( K.GE.2 .AND.
     &     (.NOT.useAIM .OR.tracerIdentity.NE.GAD_SALINITY .OR.K.LT.Nr)
     &              ) THEN
#else
        IF ( K.GE.2 ) THEN
#endif

         IF ( advectionScheme.EQ.ENUM_CENTERED_2ND ) THEN
          diagonalNumber = 3
          CALL GAD_C2_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        dTtracerLev, rTrans,
     U                        b5d, c5d, d5d,
     I                        myThid)
         ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
          diagonalNumber = 3
          CALL GAD_FLUXLIMIT_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        dTtracerLev, rTrans, localTijk,
     U                        b5d, c5d, d5d,
     I                        myThid)
         ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD .OR.
     &           advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
          diagonalNumber = 5
          CALL GAD_U3C4_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        advectionScheme, dTtracerLev, rTrans,
     U                        a5d, b5d, c5d, d5d, e5d,
     I                        myThid)
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
      IF ( useDiagnostics .AND. implicitDiffusion ) THEN
        diagSufx = GAD_DIAG_SUFX( tracerIdentity, myThid )
        diagName = 'DFrI'//diagSufx
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO k= 1,Nr
          IF ( k.EQ.1 ) THEN
C-  Note: Needs to call DIAGNOSTICS_FILL at level k=1 even if array == 0
C         otherwise counter is not incremented !!
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               df(i,j) = 0. _d 0
             ENDDO
            ENDDO
          ELSE
            DO j=1,sNy
             DO i=1,sNx
               df(i,j) = 
     &              rA(i,j,bi,bj)
     &            * KappaRX(i,j,k)*recip_drC(k)
     &            * (gTracer(i,j,k,bi,bj) - gTracer(i,j,k-1,bi,bj))
             ENDDO
            ENDDO
          ENDIF
          CALL DIAGNOSTICS_FILL(df,diagName, k,1, 2,bi,bj, myThid)
         ENDDO
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   end if Nr > 1
      ENDIF

      RETURN
      END
