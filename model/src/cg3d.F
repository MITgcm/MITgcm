C $Header: /u/gcmpack/MITgcm/model/src/cg3d.F,v 1.19 2007/09/04 14:54:58 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CG3D
C     !INTERFACE:
      SUBROUTINE CG3D(
     I                cg3d_b,
     U                cg3d_x,
     O                firstResidual,
     O                lastResidual,
     U                numIters,
     I                myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CG3D
C     | o Three-dimensional grid problem conjugate-gradient
C     |   inverter (with preconditioner).
C     *==========================================================*
C     | Con. grad is an iterative procedure for solving Ax = b.
C     | It requires the A be symmetric.
C     | This implementation assumes A is a seven-diagonal
C     | matrix of the form that arises in the discrete
C     | representation of the del^2 operator in a
C     | three-dimensional space.
C     | Notes:
C     | ======
C     | This implementation can support shared-memory
C     | multi-threaded execution. In order to do this COMMON
C     | blocks are used for many of the arrays - even ones that
C     | are only used for intermedaite results. This design is
C     | OK if you want to all the threads to collaborate on
C     | solving the same problem. On the other hand if you want
C     | the threads to solve several different problems
C     | concurrently this implementation will not work.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG3D.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    - Thread on which I am working.
C     cg3d_b    - The source term or "right hand side"
C     cg3d_x    - The solution
C     firstResidual - the initial residual before any iterations
C     lastResidual  - the actual residual reached
C     numIters  - Entry: the maximum number of iterations allowed
C                 Exit:  the actual number of iterations used
      _RL  cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  firstResidual
      _RL  lastResidual
      INTEGER numIters
      INTEGER myThid


#ifdef ALLOW_NONHYDROSTATIC

C     !LOCAL VARIABLES:
C     === Local variables ====
C     actualIts      - Number of iterations taken
C     actualResidual - residual
C     bi          - Block index in X and Y.
C     bj
C     eta_qrN     - Used in computing search directions
C     eta_qrNM1     suffix N and NM1 denote current and
C     cgBeta        previous iterations respectively.
C     alpha
C     sumRHS      - Sum of right-hand-side. Sometimes this is a
C                   useful debuggin/trouble shooting diagnostic.
C                   For neumann problems sumRHS needs to be ~0.
C                   or they converge at a non-zero residual.
C     err         - Measure of residual of Ax - b, usually the norm.
C     I, J, K, N  - Loop counters ( N counts CG iterations )
      INTEGER actualIts
      _RL    actualResidual
      INTEGER bi, bj
      INTEGER I, J, K, it3d
      INTEGER Km1, Kp1
      _RL    maskM1, maskP1
      _RL    err,    errTile(nSx,nSy)
      _RL    eta_qrN,eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1
      _RL    cgBeta
      _RL    alpha , alphaTile(nSx,nSy)
      _RL    sumRHS, sumRHStile(nSx,nSy)
      _RL    rhsMax
      _RL    rhsNorm
CEOP


C--   Initialise inverter
      eta_qrNM1 = 1. D0

C--   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           cg3d_b(I,J,K,bi,bj) = cg3d_b(I,J,K,bi,bj)*cg3dNorm
     &                          * maskC(I,J,K,bi,bj)
           rhsMax = MAX(ABS(cg3d_b(I,J,K,bi,bj)),rhsMax)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_MAX_R8( rhsMax, myThid )
      rhsNorm = 1. _d 0
      IF ( rhsMax .NE. 0. ) rhsNorm = 1. _d 0 / rhsMax
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           cg3d_b(I,J,K,bi,bj) = cg3d_b(I,J,K,bi,bj)*rhsNorm
           cg3d_x(I,J,K,bi,bj) = cg3d_x(I,J,K,bi,bj)*rhsNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlaps
c     _EXCH_XYZ_R8( cg3d_b, myThid )
      _EXCH_XYZ_R8( cg3d_x, myThid )

C--   Initial residual calculation (with free-Surface term)
      err    = 0. _d 0
      sumRHS = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        errTile(bi,bj)    = 0. _d 0
        sumRHStile(bi,bj) = 0. _d 0
        DO K=1,Nr
         Km1 = MAX(K-1, 1 )
         Kp1 = MIN(K+1, Nr)
         maskM1 = 1. _d 0
         maskP1 = 1. _d 0
         IF ( K .EQ. 1 ) maskM1 = 0. _d 0
         IF ( K .EQ. Nr) maskP1 = 0. _d 0

         DO J=1,sNy
          DO I=1,sNx
           cg3d_r(I,J,K,bi,bj) = cg3d_b(I,J,K,bi,bj) -( 0.
     &     +aW3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I-1,J  ,K  ,bi,bj)
     &     +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_x(I+1,J  ,K  ,bi,bj)
     &     +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J-1,K  ,bi,bj)
     &     +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_x(I  ,J+1,K  ,bi,bj)
     &     +aV3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,Km1,bi,bj)*maskM1
     &     +aV3d(I  ,J  ,Kp1,bi,bj)*cg3d_x(I  ,J  ,Kp1,bi,bj)*maskP1
     &     +aC3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     )
           errTile(bi,bj) = errTile(bi,bj)
     &     +cg3d_r(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           sumRHStile(bi,bj) = sumRHStile(bi,bj)
     &     +cg3d_b(I,J,K,bi,bj)
          ENDDO
         ENDDO
         DO J=1-1,sNy+1
          DO I=1-1,sNx+1
           cg3d_s(I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
c       err    = err    + errTile(bi,bj)
c       sumRHS = sumRHS + sumRHStile(bi,bj)
       ENDDO
      ENDDO
       CALL EXCH_S3D_RL( cg3d_r, Nr, myThid )
c      CALL EXCH_S3D_RL( cg3d_s, Nr, myThid )
c     _GLOBAL_SUM_R8( sumRHS, myThid )
c     _GLOBAL_SUM_R8( err   , myThid )
       CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
       CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )

      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        write(standardmessageunit,'(A,1P2E22.14)')
     &     ' cg3d: Sum(rhs),rhsMax = ',sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF

      actualIts      = 0
      actualResidual = SQRT(err)
C     _BARRIER
c     _BEGIN_MASTER( myThid )
c      WRITE(*,'(A,I6,1PE30.14)') ' CG3D iters, err = ',
c    &  actualIts, actualResidual
c     _END_MASTER( myThid )
      firstResidual=actualResidual

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it3d=1, numIters

CcnhDebugStarts
c      IF ( mod(it3d-1,10).EQ.0)
c    &   WRITE(*,*) ' CG3D: Iteration ',it3d-1,
c    &      ' residual = ',actualResidual
CcnhDebugEnds
       IF ( actualResidual .LT. cg3dTargetResidual ) GOTO 11
C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
C      Note. On the next to loops over all tiles the inner loop ranges
C            in sNx and sNy are expanded by 1 to avoid a communication
C            step. However this entails a bit of gynamastics because we only
C            want eta_qrN for the interior points.
       eta_qrN = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         eta_qrNtile(bi,bj) = 0. _d 0
         DO K=1,1
          DO J=1-1,sNy+1
           DO I=1-1,sNx+1
            cg3d_q(I,J,K,bi,bj) =
     &       zMC(I  ,J  ,K,bi,bj)*cg3d_r(I  ,J  ,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO K=2,Nr
          DO J=1-1,sNy+1
           DO I=1-1,sNx+1
            cg3d_q(I,J,K,bi,bj) =
     &       zMC(I,J,K,bi,bj)*(cg3d_r(I,J,K  ,bi,bj)
     &      -zML(I,J,K,bi,bj)*cg3d_q(I,J,K-1,bi,bj))
           ENDDO
          ENDDO
         ENDDO
         DO K=Nr,Nr
caja      IF (Nr .GT. 1) THEN
caja       DO J=1-1,sNy+1
caja        DO I=1-1,sNx+1
caja         cg3d_q(I,J,K,bi,bj) =
caja &        zMC(i,j,k,bi,bj)*(cg3d_r(i,j,k  ,bi,bj)
caja &       -zML(i,j,k,bi,bj)*cg3d_q(i,j,k-1,bi,bj))
caja        ENDDO
caja       ENDDO
caja      ENDIF
          DO J=1,sNy
           DO I=1,sNx
            eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &      +cg3d_q(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO K=Nr-1,1,-1
          DO J=1-1,sNy+1
           DO I=1-1,sNx+1
            cg3d_q(I,J,K,bi,bj) =
     &      cg3d_q(I,J,K,bi,bj)
     &      -zMU(I,J,K,bi,bj)*cg3d_q(I,J,K+1,bi,bj)
           ENDDO
          ENDDO
          DO J=1,sNy
           DO I=1,sNx
            eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &      +cg3d_q(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
c        eta_qrN = eta_qrN + eta_qrNtile(bi,bj)
        ENDDO
       ENDDO
caja
caja  eta_qrN=0.
caja   DO bj=myByLo(myThid),myByHi(myThid)
caja    DO bi=myBxLo(myThid),myBxHi(myThid)
caja     DO K=1,Nr
caja      DO J=1,sNy
caja       DO I=1,sNx
caja        eta_qrN = eta_qrN
caja &      +cg3d_q(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
caja       ENDDO
caja      ENDDO
caja     ENDDO
caja    ENDDO
caja   ENDDO
caja

c      _GLOBAL_SUM_R8(eta_qrN, myThid)
       CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' eta_qrN = ',eta_qrN
CcnhDebugEnds
       cgBeta   = eta_qrN/eta_qrNM1
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' beta = ',cgBeta
CcnhDebugEnds
       eta_qrNM1 = eta_qrN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1-1,sNy+1
           DO I=1-1,sNx+1
            cg3d_s(I,J,K,bi,bj) = cg3d_q(I,J,K,bi,bj)
     &                          + cgBeta*cg3d_s(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
       alpha = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         alphaTile(bi,bj) = 0. _d 0
         IF ( Nr .GT. 1 ) THEN
          DO K=1,1
           DO J=1,sNy
            DO I=1,sNx
             cg3d_q(I,J,K,bi,bj) =
     &       aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I-1,J  ,K  ,bi,bj)
     &      +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I+1,J  ,K  ,bi,bj)
     &      +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J-1,K  ,bi,bj)
     &      +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J+1,K  ,bi,bj)
     &      +aV3d(I  ,J  ,K+1,bi,bj)*cg3d_s(I  ,J  ,K+1,bi,bj)
     &      +aC3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
             alphaTile(bi,bj) = alphaTile(bi,bj)
     &                 +cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSE
          DO K=1,1
           DO J=1,sNy
            DO I=1,sNx
             cg3d_q(I,J,K,bi,bj) =
     &       aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I-1,J  ,K  ,bi,bj)
     &      +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I+1,J  ,K  ,bi,bj)
     &      +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J-1,K  ,bi,bj)
     &      +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J+1,K  ,bi,bj)
     &      +aC3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
             alphaTile(bi,bj) = alphaTile(bi,bj)
     &                 +cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         DO K=2,Nr-1
          DO J=1,sNy
           DO I=1,sNx
            cg3d_q(I,J,K,bi,bj) =
     &      aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I-1,J  ,K  ,bi,bj)
     &     +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I+1,J  ,K  ,bi,bj)
     &     +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J-1,K  ,bi,bj)
     &     +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J+1,K  ,bi,bj)
     &     +aV3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K-1,bi,bj)
     &     +aV3d(I  ,J  ,K+1,bi,bj)*cg3d_s(I  ,J  ,K+1,bi,bj)
     &     +aC3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
            alphaTile(bi,bj) = alphaTile(bi,bj)
     &                +cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         IF ( Nr .GT. 1 ) THEN
          DO K=Nr,Nr
           DO J=1,sNy
            DO I=1,sNx
             cg3d_q(I,J,K,bi,bj) =
     &       aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I-1,J  ,K  ,bi,bj)
     &      +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I+1,J  ,K  ,bi,bj)
     &      +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J-1,K  ,bi,bj)
     &      +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J+1,K  ,bi,bj)
     &      +aV3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K-1,bi,bj)
     &      +aC3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
             alphaTile(bi,bj) = alphaTile(bi,bj)
     &                 +cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
c        alpha = alpha + alphaTile(bi,bj)
        ENDDO
       ENDDO
c      _GLOBAL_SUM_R8(alpha,myThid)
       CALL GLOBAL_SUM_TILE_RL( alphaTile,  alpha,  myThid )
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' SUM(s*q)= ',alpha
CcnhDebugEnds
       alpha = eta_qrN/alpha
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' alpha= ',alpha
CcnhDebugEnds

C==    Update solution and residual vectors
C      Now compute "interior" points.
       err = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
        errTile(bi,bj)    = 0. _d 0
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            cg3d_x(I,J,K,bi,bj)=cg3d_x(I,J,K,bi,bj)
     &            +alpha*cg3d_s(I,J,K,bi,bj)
            cg3d_r(I,J,K,bi,bj)=cg3d_r(I,J,K,bi,bj)
     &            -alpha*cg3d_q(I,J,K,bi,bj)
            errTile(bi,bj) = errTile(bi,bj)
     &             +cg3d_r(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
c        err = err + errTile(bi,bj)
        ENDDO
       ENDDO

c      _GLOBAL_SUM_R8( err   , myThid )
       CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
       err = SQRT(err)
       actualIts      = it3d
       actualResidual = err
       IF ( actualResidual .LT. cg3dTargetResidual ) GOTO 11
       CALL EXCH_S3D_RL( cg3d_r, Nr, myThid )

   10 CONTINUE
   11 CONTINUE

C--   Un-normalise the answer
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           cg3d_x(I,J,K,bi,bj) = cg3d_x(I,J,K,bi,bj)/rhsNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

Cadj  _EXCH_XYZ_R8(cg3d_x, myThid )
c     _BEGIN_MASTER( myThid )
c      WRITE(*,'(A,I6,1PE30.14)') ' CG3D iters, err = ',
c    &  actualIts, actualResidual
c     _END_MASTER( myThid )
      lastResidual=actualResidual
      numIters=actualIts

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
