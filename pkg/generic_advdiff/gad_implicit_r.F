C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_implicit_r.F,v 1.2 2004/01/07 21:35:00 jmc Exp $
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
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R GAD_IMPLICIT_R
C     | o Solve implicitly vertical advection & diffusion
C     |   for one tracer
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
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

C     !LOCAL VARIABLES:
C     == Local variables ==
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
      _RL limitDf  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rFlx
CEOP

      IF (Nr.LE.1) RETURN

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
          IF (maskC(i,j,k-1,bi,bj).EQ.1.) 
     &     b5d(i,j,k) = -deltaTtracer
     &                  *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &                  *kappaRX(i,j, k )*recip_drC( k )
         ENDDO
        ENDDO
       ENDDO
C-     1rst upper diagonal :
       DO k=1,Nr-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF (maskC(i,j,k+1,bi,bj).EQ.1.) 
     &     d5d(i,j,k) = -deltaTtracer
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
     &      + deltaTtracer*recip_rA(i,j,bi,bj)
     &       *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
     &       *tracer(i,j,k,bi,bj)*(rTrans(i,j)-rTransKp1(i,j))*rkFac
          ENDDO
        ENDDO

        IF (K.GE.2) THEN

         IF ( advectionScheme.EQ.ENUM_CENTERED_2ND ) THEN
          diagonalNumber = 3
          CALL GAD_C2_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        deltaTtracer, rTrans,
     U                        b5d, c5d, d5d,
     I                        myThid)
         ELSEIF (advectionScheme.EQ.ENUM_FLUX_LIMIT) THEN
          diagonalNumber = 3
          CALL GAD_FLUXLIMIT_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        deltaTtracer, rTrans, localTijk,
     U                        b5d, c5d, d5d,
     I                        myThid)
         ELSEIF (advectionScheme.EQ.ENUM_UPWIND_3RD .OR.
     &           advectionScheme.EQ.ENUM_CENTERED_4TH) THEN
          diagonalNumber = 5
          CALL GAD_U3C4_IMPL_R( bi,bj,k, iMin,iMax,jMin,jMax,
     I                        advectionScheme, deltaTtracer, rTrans,
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

      RETURN
      END
