C $Header: /u/gcmpack/MITgcm/model/src/cg2d.F,v 1.25 2000/03/24 19:06:14 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE CG2D(  
     I                cg2d_b,
     U                cg2d_x,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE CG2D                                          |
C     | o Two-dimensional grid problem conjugate-gradient        |
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
#include "CG2D_INTERNAL.h"

C     === Routine arguments ===
C     myThid - Thread on which I am working.
      INTEGER myThid
      _RL  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)


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
      INTEGER I, J, it2d
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


CcnhDebugStarts
C     CHARACTER*(MAX_LEN_FNAM) suff
CcnhDebugEnds


C--   Initialise inverter
      etaNM1              = 1. _d 0

CcnhDebugStarts
C     _EXCH_XY_R8( cg2d_b, myThid )
C     CALL PLOT_FIELD_XYRL( cg2d_b, 'CG2D.0 CG2D_B' , 1, myThid )
C     suff = 'unnormalised'
C     CALL WRITE_FLD_XY_RL (  'cg2d_b.',suff,    cg2d_b, 1, myThid)
C     STOP
CcnhDebugEnds

C--   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          cg2d_b(I,J,bi,bj) = cg2d_b(I,J,bi,bj)*cg2dNorm
          rhsMax = MAX(ABS(cg2d_b(I,J,bi,bj)),rhsMax)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef LETS_MAKE_JAM
C     _GLOBAL_MAX_R8( rhsMax, myThid )
      rhsMax=1.
#else
      _GLOBAL_MAX_R8( rhsMax, myThid )
#endif
      rhsNorm = 1. _d 0
      IF ( rhsMax .NE. 0. ) rhsNorm = 1. _d 0 / rhsMax
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          cg2d_b(I,J,bi,bj) = cg2d_b(I,J,bi,bj)*rhsNorm
          cg2d_x(I,J,bi,bj) = cg2d_x(I,J,bi,bj)*rhsNorm
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlaps
      _EXCH_XY_R8( cg2d_b, myThid )
      _EXCH_XY_R8( cg2d_x, myThid )
CcnhDebugStarts
C     CALL PLOT_FIELD_XYRL( cg2d_b, 'CG2D.1 CG2D_B' , 1, myThid )
C     suff = 'normalised'
C     CALL WRITE_FLD_XY_RL (  'cg2d_b.',suff,    cg2d_b, 1, myThid)
CcnhDebugEnds

C--   Initial residual calculation
      err    = 0. _d 0
      sumRHS = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          cg2d_s(I,J,bi,bj) = 0.
          cg2d_r(I,J,bi,bj) = cg2d_b(I,J,bi,bj) -
     &    (aW2d(I  ,J  ,bi,bj)*cg2d_x(I-1,J  ,bi,bj)
     &    +aW2d(I+1,J  ,bi,bj)*cg2d_x(I+1,J  ,bi,bj)
     &    +aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J-1,bi,bj)
     &    +aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J+1,bi,bj)
     &    -aW2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
     &    -aW2d(I+1,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
     &    -aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
     &    -aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
     &    -freeSurfFac*_rA(i,j,bi,bj)* horiVertRatio*
     &     cg2d_x(I  ,J  ,bi,bj)/deltaTMom/deltaTMom*cg2dNorm
     &    )
          err            = err            + 
     &     cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          sumRHS            = sumRHS            +
     &     cg2d_b(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     _EXCH_XY_R8( cg2d_r, myThid )
#ifdef LETS_MAKE_JAM
      CALL EXCH_XY_O1_R8_JAM( cg2d_r )
#else
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = 1
       CALL EXCH_RL( cg2d_r,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )
#endif
C     _EXCH_XY_R8( cg2d_s, myThid )
#ifdef LETS_MAKE_JAM
      CALL EXCH_XY_O1_R8_JAM( cg2d_s )
#else
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = 1
       CALL EXCH_RL( cg2d_s,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )
#endif
      _GLOBAL_SUM_R8( sumRHS, myThid )
C     WRITE(6,*) ' mythid, err = ', mythid, SQRT(err)
      _GLOBAL_SUM_R8( err   , myThid )
      
      _BEGIN_MASTER( myThid )
      write(0,'(A,1PE30.14)') ' cg2d: Sum(rhs) = ',sumRHS
      _END_MASTER( )

      actualIts      = 0
      actualResidual = SQRT(err)
C     _BARRIER
      _BEGIN_MASTER( myThid )
       WRITE(0,'(A,I6,1PE30.14)') ' CG2D iters, err = ', 
     & actualIts, actualResidual
      _END_MASTER( )

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it2d=1, cg2dMaxIters

CcnhDebugStarts
C      WRITE(0,*) ' CG2D: Iteration ',it2d-1,' residual = ',
C    &  actualResidual
CcnhDebugEnds
       IF ( err .LT. cg2dTargetResidual ) GOTO 11
C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
       etaN = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_q(I,J,bi,bj) = 
     &      pC(I  ,J  ,bi,bj)*cg2d_r(I  ,J  ,bi,bj)
     &     +pW(I  ,J  ,bi,bj)*cg2d_r(I-1,J  ,bi,bj)
     &     +pW(I+1,J  ,bi,bj)*cg2d_r(I+1,J  ,bi,bj)
     &     +pS(I  ,J  ,bi,bj)*cg2d_r(I  ,J-1,bi,bj)
     &     +pS(I  ,J+1,bi,bj)*cg2d_r(I  ,J+1,bi,bj)
CcnhDebugStarts
C          cg2d_q(I,J,bi,bj) = cg2d_r(I  ,J  ,bi,bj)
CcnhDebugEnds
           etaN = etaN
     &     +cg2d_q(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8(etaN, myThid)
CcnhDebugStarts
C      WRITE(0,*) ' CG2D: Iteration ',it2d-1,' etaN = ',etaN
CcnhDebugEnds
       cgBeta   = etaN/etaNM1
CcnhDebugStarts
C      WRITE(0,*) ' CG2D: Iteration ',it2d-1,' beta = ',cgBeta
CcnhDebugEnds
       etaNM1 = etaN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_s(I,J,bi,bj) = cg2d_q(I,J,bi,bj)
     &                       + cgBeta*cg2d_s(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--    Do exchanges that require messages i.e. between
C--    processes.
C      _EXCH_XY_R8( cg2d_s, myThid )
#ifdef LETS_MAKE_JAM
      CALL EXCH_XY_O1_R8_JAM( cg2d_s )
#else
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = 1
       CALL EXCH_RL( cg2d_s,
     I            OLw, OLe, OLs, OLn, myNz,
     I            exchWidthX, exchWidthY,
     I            FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )
#endif

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
       alpha = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_q(I,J,bi,bj) = 
     &     aW2d(I  ,J  ,bi,bj)*cg2d_s(I-1,J  ,bi,bj)
     &    +aW2d(I+1,J  ,bi,bj)*cg2d_s(I+1,J  ,bi,bj)
     &    +aS2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J-1,bi,bj)
     &    +aS2d(I  ,J+1,bi,bj)*cg2d_s(I  ,J+1,bi,bj)
     &    -aW2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
     &    -aW2d(I+1,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
     &    -aS2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
     &    -aS2d(I  ,J+1,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
     &    -freeSurfFac*_rA(i,j,bi,bj)* horiVertRatio*
     &     cg2d_s(I  ,J  ,bi,bj)/deltaTMom/deltaTMom*cg2dNorm
          alpha = alpha+cg2d_s(I,J,bi,bj)*cg2d_q(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_SUM_R8(alpha,myThid)
CcnhDebugStarts
C      WRITE(0,*) ' CG2D: Iteration ',it2d-1,' SUM(s*q)= ',alpha
CcnhDebugEnds
       alpha = etaN/alpha
CcnhDebugStarts
C      WRITE(0,*) ' CG2D: Iteration ',it2d-1,' alpha= ',alpha
CcnhDebugEnds
     
C==    Update solution and residual vectors
C      Now compute "interior" points.
       err = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_x(I,J,bi,bj)=cg2d_x(I,J,bi,bj)+alpha*cg2d_s(I,J,bi,bj)
           cg2d_r(I,J,bi,bj)=cg2d_r(I,J,bi,bj)-alpha*cg2d_q(I,J,bi,bj)
           err = err+cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8( err   , myThid )
       err = SQRT(err)
       actualIts      = it2d
       actualResidual = err
       IF ( err .LT. cg2dTargetResidual ) GOTO 11
C      _EXCH_XY_R8(cg2d_r, myThid )
#ifdef LETS_MAKE_JAM
      CALL EXCH_XY_O1_R8_JAM( cg2d_r )
#else
       OLw        = 1
       OLe        = 1
       OLn        = 1
       OLs        = 1
       exchWidthX = 1
       exchWidthY = 1
       myNz       = 1
       CALL EXCH_RL( cg2d_r,
     I             OLw, OLe, OLs, OLn, myNz,
     I             exchWidthX, exchWidthY,
     I             FORWARD_SIMULATION, EXCH_IGNORE_CORNERS, myThid )
#endif

   10 CONTINUE
   11 CONTINUE

C--   Un-normalise the answer
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          cg2d_x(I  ,J  ,bi,bj) = cg2d_x(I  ,J  ,bi,bj)/rhsNorm
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     The following exchange was moved up to solve_for_pressure
C     for compatibility with TAMC.
C     _EXCH_XY_R8(cg2d_x, myThid )
      _BEGIN_MASTER( myThid )
       WRITE(0,'(A,I6,1PE30.14)') ' CG2D iters, err = ', 
     & actualIts, actualResidual
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

      RETURN
      END
