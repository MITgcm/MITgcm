C $Header: /u/gcmpack/MITgcm/model/src/cg3d.F,v 1.4 2000/03/27 22:25:43 adcroft Exp $

#include "CPP_OPTIONS.h"

#define VERBOSE

      SUBROUTINE CG3D(  
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE CG3D                                          |
C     | o Three-dimensional grid problem conjugate-gradient      |
C     |   inverter (with preconditioner).                        |
C     |==========================================================|
C     | Con. grad is an iterative procedure for solving Ax = b.  |
C     | It requires the A be symmetric.                          |
C     | This implementation assumes A is a five-diagonal         |
C     | matrix of the form that arises in the discrete           |
C     | representation of the del^2 operator in a                |
C     | two-dimensional space.                                   |
C     | Notes:                                                   |
C     | ======                                                   |
C     | This implementation can support shared-memory            | 
C     | multi-threaded execution. In order to do this COMMON     | 
C     | blocks are used for many of the arrays - even ones that  | 
C     | are only used for intermedaite results. This design is   | 
C     | OK if you want to all the threads to collaborate on      | 
C     | solving the same problem. On the other hand if you want  | 
C     | the threads to solve several different problems          | 
C     | concurrently this implementation will not work.          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG3D.h"

C     === Routine arguments ===
C     myThid - Thread on which I am working.
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC

C     === Local variables ====
C     actualIts      - Number of iterations taken
C     actualResidual - residual
C     bi          - Block index in X and Y.
C     bj
C     etaN        - Used in computing search directions
C     etaNM1        suffix N and NM1 denote current and
C     cgBeta        previous iterations respectively.
C     alpha  
C     sumRHS      - Sum of right-hand-side. Sometimes this is a
C                   useful debuggin/trouble shooting diagnostic.
C                   For neumann problems sumRHS needs to be ~0.
C                   or they converge at a non-zero residual.
C     err         - Measure of residual of Ax - b, usually the norm.
C     I, J, N     - Loop counters ( N counts CG iterations )
      INTEGER actualIts
      _RL    actualResidual
      INTEGER bi, bj              
      INTEGER I, J, K, it3d
      INTEGER KM1, KP1
      _RL    err
      _RL    etaN
      _RL    etaNM1
      _RL    cgBeta
      _RL    alpha
      _RL    sumRHS
      _RL    rhsMax
      _RL    rhsNorm

      INTEGER OLw
      INTEGER OLe
      INTEGER OLn
      INTEGER OLs
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER myNz
      _RL     topLevFac

C--   Initialise inverter
      etaNM1              = 1. D0

C--   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           cg3d_b(I,J,K,bi,bj) = cg3d_b(I,J,K,bi,bj)*cg3dNorm
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
      _EXCH_XYZ_R8( cg3d_b, myThid )
      _EXCH_XYZ_R8( cg3d_x, myThid )

#ifdef NONO
CcnhDebugStarts
C--   Initial residual calculation
      err    = 0. _d 0
      sumRHS = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
          alpha = 0. _d 0
          DO K=1,Nr
           KM1 = K-1
           IF ( KM1 .EQ. 0 )    KM1 = 1
           KP1 = K+1
           IF ( KP1 .EQ. Nr+1 ) KP1 = 1 
            cg3d_r(I,J,K,bi,bj) = cg3d_b(I,J,K,bi,bj) -( 0.
     &     +aW3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I-1,J  ,K  ,bi,bj)
     &     +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_x(I+1,J  ,K  ,bi,bj)
     &     +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J-1,K  ,bi,bj)
     &     +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_x(I  ,J+1,K  ,bi,bj)
     &     +aV3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,KM1,bi,bj)
     &     +aV3d(I  ,J  ,KP1,bi,bj)*cg3d_x(I  ,J  ,KP1,bi,bj)
     &     -aW3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aW3d(I+1,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aS3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aS3d(I  ,J+1,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aV3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aV3d(I  ,J  ,KP1,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     )
           alpha = alpha
     &     +cg3d_r(I,J,K,bi,bj)
           sumRHS = sumRHS
     &     +cg3d_b(I,J,K,bi,bj)
          ENDDO
          err = err + alpha*alpha
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      WRITE(6,*) 'DEBUG  mythid, err = ', mythid, SQRT(err)
      _GLOBAL_SUM_R8( err   , myThid )
      _GLOBAL_SUM_R8( sumRHS   , myThid )
      _BEGIN_MASTER( myThid )
      write(0,*) 'DEBUG cg3d: Sum(rhs) = ',sumRHS
      _END_MASTER( )
      actualIts      = 0
      actualResidual = SQRT(err)
C     _BARRIER
      _BEGIN_MASTER( myThid )
       WRITE(0,'(A,I6,1PE30.14)') 'DEBUG  CG3D iters, err = ',
     &  actualIts, actualResidual
      _END_MASTER( )
CcnhDebugEnds
#endif

C--   Initial residual calculation
      err    = 0. _d 0
      sumRHS = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         KM1 = K-1
         IF ( K .EQ. 1 ) KM1 = 1
         KP1 = K+1
         IF ( K .EQ. Nr ) KP1 = 1 
         topLevFac = 0.
         IF ( K .EQ. 1) topLevFac = 1.
         DO J=1,sNy
          DO I=1,sNx
           cg3d_s(I,J,K,bi,bj) = 0.
           cg3d_r(I,J,K,bi,bj) = cg3d_b(I,J,K,bi,bj) -( 0.
     &     +aW3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I-1,J  ,K  ,bi,bj)
     &     +aW3d(I+1,J  ,K  ,bi,bj)*cg3d_x(I+1,J  ,K  ,bi,bj)
     &     +aS3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J-1,K  ,bi,bj)
     &     +aS3d(I  ,J+1,K  ,bi,bj)*cg3d_x(I  ,J+1,K  ,bi,bj)
     &     +aV3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,KM1,bi,bj)
     &     +aV3d(I  ,J  ,KP1,bi,bj)*cg3d_x(I  ,J  ,KP1,bi,bj)
     &     -aW3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aW3d(I+1,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aS3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aS3d(I  ,J+1,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aV3d(I  ,J  ,K  ,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -aV3d(I  ,J  ,KP1,bi,bj)*cg3d_x(I  ,J  ,K  ,bi,bj)
     &     -freeSurfFac*_rA(i,j,bi,bj)* horiVertRatio*
     &      cg3d_x(I  ,J  ,K,bi,bj)/deltaTMom/deltaTMom*cg3dNorm
     &      *topLevFac
     &     )
           err = err
     &     +cg3d_r(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           sumRHS = sumRHS
     &     +cg3d_b(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XYZ_R8( cg3d_r, myThid )
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = Nr
       CALL EXCH_RL( cg3d_r,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )
C     _EXCH_XYZ_R8( cg3d_s, myThid )
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = Nr
       CALL EXCH_RL( cg3d_s,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )
      _GLOBAL_SUM_R8( sumRHS, myThid )
      _GLOBAL_SUM_R8( err   , myThid )
      
      _BEGIN_MASTER( myThid )
      write(0,'(A,1PE30.14)') ' cg3d: Sum(rhs) = ',sumRHS
      _END_MASTER( )

      actualIts      = 0
      actualResidual = SQRT(err)
C     _BARRIER
      _BEGIN_MASTER( myThid )
       WRITE(0,'(A,I6,1PE30.14)') ' CG3D iters, err = ',
     &  actualIts, actualResidual
      _END_MASTER( )

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it3d=1, cg3dMaxIters

CcnhDebugStarts
#ifdef VERBOSE
       IF ( mod(it3d-1,10).EQ.0)
     &   WRITE(0,*) ' CG3D: Iteration ',it3d-1,
     &      ' residual = ',actualResidual
#endif
CcnhDebugEnds
       IF ( actualResidual .LT. cg3dTargetResidual ) GOTO 11
C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
C      Note. On the next to loops over all tiles the inner loop ranges
C            in sNx and sNy are expanded by 1 to avoid a communication 
C            step. However this entails a bit of gynamastics because we only 
C            want etaN for the interior points.
       etaN = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
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
            etaN = etaN
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
            etaN = etaN
     &      +cg3d_q(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
caja
caja  etaN=0.
caja   DO bj=myByLo(myThid),myByHi(myThid)
caja    DO bi=myBxLo(myThid),myBxHi(myThid)
caja     DO K=1,Nr
caja      DO J=1,sNy
caja       DO I=1,sNx
caja        etaN = etaN
caja &      +cg3d_q(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
caja       ENDDO
caja      ENDDO
caja     ENDDO
caja    ENDDO
caja   ENDDO
caja

       _GLOBAL_SUM_R8(etaN, myThid)
CcnhDebugStarts
C      WRITE(0,*) ' CG3D: Iteration ',it3d-1,' etaN = ',etaN
CcnhDebugEnds
       cgBeta   = etaN/etaNM1
CcnhDebugStarts
C      WRITE(0,*) ' CG3D: Iteration ',it3d-1,' beta = ',cgBeta
CcnhDebugEnds
       etaNM1 = etaN

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
     &      -aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aV3d(I  ,J  ,K+1,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -freeSurfFac*_rA(i,j,bi,bj)* horiVertRatio*
     &       cg3d_s(I  ,J  ,K,bi,bj)/deltaTMom/deltaTMom*cg3dNorm
             alpha = alpha+cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
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
     &      -aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -freeSurfFac*_rA(i,j,bi,bj)* horiVertRatio*
     &       cg3d_s(I  ,J  ,K,bi,bj)/deltaTMom/deltaTMom*cg3dNorm
             alpha = alpha+cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
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
     &     -aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &     -aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &     -aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &     -aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &     -aV3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &     -aV3d(I  ,J  ,K+1,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
            alpha = alpha+cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
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
     &      -aW3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aW3d(I+1,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aS3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aS3d(I  ,J+1,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
     &      -aV3d(I  ,J  ,K  ,bi,bj)*cg3d_s(I  ,J  ,K  ,bi,bj)
             alpha = alpha+cg3d_s(I,J,K,bi,bj)*cg3d_q(I,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
       _GLOBAL_SUM_R8(alpha,myThid)
CcnhDebugStarts
C      WRITE(0,*) ' CG3D: Iteration ',it3d-1,' SUM(s*q)= ',alpha
CcnhDebugEnds
       alpha = etaN/alpha
CcnhDebugStarts
C      WRITE(0,*) ' CG3D: Iteration ',it3d-1,' alpha= ',alpha
CcnhDebugEnds
     
C==    Update solution and residual vectors
C      Now compute "interior" points.
       err = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
            cg3d_x(I,J,K,bi,bj)=cg3d_x(I,J,K,bi,bj)
     &            +alpha*cg3d_s(I,J,K,bi,bj)
            cg3d_r(I,J,K,bi,bj)=cg3d_r(I,J,K,bi,bj)
     &            -alpha*cg3d_q(I,J,K,bi,bj)
            err = err+cg3d_r(I,J,K,bi,bj)*cg3d_r(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8( err   , myThid )
       err = SQRT(err)
       actualIts      = it3d
       actualResidual = err
       IF ( actualResidual .LT. cg3dTargetResidual ) GOTO 11
C      _EXCH_XYZ_R8(cg3d_r, myThid )
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = Nr
       CALL EXCH_RL( cg3d_r,
     I             OLw, OLe, OLs, OLn, myNz,
     I             exchWidthX, exchWidthY,
     I             FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )

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
      _BEGIN_MASTER( myThid )
       WRITE(0,'(A,I6,1PE30.14)') ' CG3D iters, err = ',
     &  actualIts, actualResidual
      _END_MASTER( )

CcnhDebugStarts
C     CALL PLOT_FIELD_XYRL( cg2d_x, 'CALC_MOM_RHS CG2D_X' , 1, myThid )
C     err    = 0. _d 0
C     DO bj=myByLo(myThid),myByHi(myThid)
C      DO bi=myBxLo(myThid),myBxHi(myThid)
C       DO J=1,sNy
C        DO I=1,sNx
C         cg2d_r(I,J,bi,bj) = cg2d_b(I,J,bi,bj) -
C    &    (aW2d(I  ,J  ,bi,bj)*cg2d_x(I-1,J  ,bi,bj)
C    &    +aW2d(I+1,J  ,bi,bj)*cg2d_x(I+1,J  ,bi,bj)
C    &    +aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J-1,bi,bj)
C    &    +aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J+1,bi,bj)
C    &    -aW2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
C    &    -aW2d(I+1,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
C    &    -aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
C    &    -aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J  ,bi,bj))
C         err            = err            + 
C    &     cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
C        ENDDO
C       ENDDO
C      ENDDO
C     ENDDO
C     _GLOBAL_SUM_R8( err   , myThid )
C     write(0,*) 'cg2d: Ax - b = ',SQRT(err)
CcnhDebugEnds

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
