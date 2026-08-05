#include "CPP_OPTIONS.h"
#ifdef TARGET_NEC_SX
C     set a sensible default for the outer loop unrolling parameter that can
C     be overriden in the Makefile with the DEFINES macro or in CPP_OPTIONS.h
#ifndef CG2D_OUTERLOOPITERS
# define CG2D_OUTERLOOPITERS 10
#endif
#endif /* TARGET_NEC_SX */

CBOP
C     !ROUTINE: CG2D
C     !INTERFACE:
      SUBROUTINE CG2D(
     U                cg2d_b, cg2d_x,
     O                firstResidual, minResidualSq, lastResidual,
     U                numIters, nIterMin,
     I                myThid )
C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE CG2D
C     | o Two-dimensional grid problem conjugate-gradient inverter
C     |   (with preconditioner).
C     *================================================================*
C     | Con. grad is an iterative procedure for solving Ax = b.
C     | It requires the A be symmetric.
C     | This implementation assumes A is a five-diagonal matrix
C     | of the form that arises in the discrete representation of
C     | the del^2 operator in a two-dimensional space.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

C     !INPUT/OUTPUT PARAMETERS:
C     cg2d_b    :: The source term or "right hand side" (output: normalised RHS)
C     cg2d_x    :: The solution (input: first guess)
C     firstResidual :: the initial residual before any iterations
C     minResidualSq :: the lowest residual reached (squared)
C     lastResidual  :: the actual residual reached
C     numIters  :: Inp: the maximum number of iterations allowed
C                  Out: the actual number of iterations used
C     nIterMin  :: Inp: decide to store (if >=0) or not (if <0) lowest res. sol.
C                  Out: iteration number corresponding to lowest residual
C     myThid    :: Thread on which I am working.
      _RL  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  firstResidual
      _RL  minResidualSq
      _RL  lastResidual
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj     :: tile index in X and Y.
C     i, j, it2d :: Loop counters ( it2d counts CG iterations )
C     actualIts  :: actual CG iteration number
C     err_sq     :: Measure of the square of the residual of Ax - b.
C     eta_qrN    :: Used in computing search directions; suffix N and NM1
C     eta_qrNM1     denote current and previous iterations respectively.
C     cgBeta     :: coeff used to update conjugate direction vector "s".
C     alpha      :: coeff used to update solution & residual
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a useful
C                   debugging/trouble shooting diagnostic. For neumann problems
C                   sumRHS needs to be ~0 or it converge at a non-zero residual.
C     cg2d_min   :: used to store solution corresponding to lowest residual.
C     msgBuf     :: Informational/error message buffer
C--   local working array (used to be in CG2D.h common block:
C     cg2d_q     :: Intermediate matrix-vector product term
C     cg2d_r     ::   *same*
C     cg2d_s     ::   *same*
      INTEGER bi, bj
      INTEGER i, j, it2d
      INTEGER actualIts
      _RL    err_sq,  errTile(nSx,nSy)
      _RL    eta_qrN, eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1
      _RL    cgBeta
      _RL    alpha,   alphaTile(nSx,nSy)
      _RL    sumRHS,  sumRHStile(nSx,nSy)
      _RL    rhsMax
      _RL    rhsNorm
      _RL    cg2d_min(1:sNx,1:sNy,nSx,nSy)
      _RL    cg2d_q  (1:sNx,1:sNy,nSx,nSy)
      _RL    cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL    cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
#ifdef CG2D_SINGLECPU_SUM
      _RL    localBuf(1:sNx,1:sNy,nSx,nSy)
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP

C--   Initialise auxiliary constant, some output variable and inverter
      minResidualSq = -1. _d 0
      eta_qrNM1     =  1. _d 0

C--   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)*cg2dNorm
          rhsMax = MAX(ABS(cg2d_b(i,j,bi,bj)),rhsMax)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF (cg2dNormaliseRHS) THEN
C-  Normalise RHS :
       _GLOBAL_MAX_RL( rhsMax, myThid )
       rhsNorm = 1. _d 0
       IF ( rhsMax .NE. 0. ) rhsNorm = 1. _d 0 / rhsMax
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)*rhsNorm
           cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)*rhsNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C- end Normalise RHS
      ENDIF

C--   Update overlaps
      CALL EXCH_XY_RL( cg2d_x, myThid )

C--   Initial residual calculation
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C-    Initialise local working arrays:
        DO j=0,sNy+1
         DO i=0,sNx+1
          cg2d_r(i,j,bi,bj) = 0. _d 0
          cg2d_s(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        IF ( nIterMin.GE.0 ) THEN
C-    Initialise saved solution
         DO j=1,sNy
          DO i=1,sNx
           cg2d_min(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        sumRHStile(bi,bj) = 0. _d 0
        errTile(bi,bj)    = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
        DO j=1,sNy
         DO i=1,sNx
          cg2d_r(i,j,bi,bj) = cg2d_b(i,j,bi,bj) -
     &    (aW2d(i  ,j  ,bi,bj)*cg2d_x(i-1,j  ,bi,bj)
     &    +aW2d(i+1,j  ,bi,bj)*cg2d_x(i+1,j  ,bi,bj)
     &    +aS2d(i  ,j  ,bi,bj)*cg2d_x(i  ,j-1,bi,bj)
     &    +aS2d(i  ,j+1,bi,bj)*cg2d_x(i  ,j+1,bi,bj)
     &    +aC2d(i  ,j  ,bi,bj)*cg2d_x(i  ,j  ,bi,bj)
     &    )
#ifdef CG2D_SINGLECPU_SUM
          localBuf(i,j,bi,bj) = cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
#else
          errTile(bi,bj)    = errTile(bi,bj)
     &                      + cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
#endif
          sumRHStile(bi,bj) = sumRHStile(bi,bj) + cg2d_b(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
#ifdef CG2D_SINGLECPU_SUM
      CALL GLOBAL_SUM_SINGLECPU_RL(localBuf, err_sq, 0, 0, myThid)
#else
      CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq, myThid )
#endif
      CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
      actualIts = 0
      firstResidual = SQRT(err_sq)
      IF ( nIterMin.GE.0 ) THEN
        nIterMin = 0
        minResidualSq = err_sq
      ENDIF

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardMessageUnit,'(A,1P2E22.14)')
     &  ' cg2d: Sum(rhs),rhsMax = ', sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF

      IF ( err_sq .LT. cg2dTolerance_sq ) GOTO 11

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it2d=1, numIters

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         eta_qrNtile(bi,bj) = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
         DO j=1,sNy
          DO i=1,sNx
           cg2d_q(i,j,bi,bj) =
     &      pC(i  ,j  ,bi,bj)*cg2d_r(i  ,j  ,bi,bj)
     &     +pW(i  ,j  ,bi,bj)*cg2d_r(i-1,j  ,bi,bj)
     &     +pW(i+1,j  ,bi,bj)*cg2d_r(i+1,j  ,bi,bj)
     &     +pS(i  ,j  ,bi,bj)*cg2d_r(i  ,j-1,bi,bj)
     &     +pS(i  ,j+1,bi,bj)*cg2d_r(i  ,j+1,bi,bj)
CcnhDebugStarts
c          cg2d_q(i,j,bi,bj) = cg2d_r(j  ,j  ,bi,bj)
CcnhDebugEnds
#ifdef CG2D_SINGLECPU_SUM
          localBuf(i,j,bi,bj) =
     &      cg2d_q(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
#else
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &     +cg2d_q(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef CG2D_SINGLECPU_SUM
       CALL GLOBAL_SUM_SINGLECPU_RL( localBuf,eta_qrN,0,0,myThid )
#else
       CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )
#endif
       cgBeta   = eta_qrN/eta_qrNM1
CcnhDebugStarts
c      WRITE(*,*) ' CG2D: Iteration ', it2d-1,
c    &            ' eta_qrN=', eta_qrN, ' beta=', cgBeta
CcnhDebugEnds
       eta_qrNM1 = eta_qrN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           cg2d_s(i,j,bi,bj) = cg2d_q(i,j,bi,bj)
     &                       + cgBeta*cg2d_s(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--    Do exchanges that require messages i.e. between processes.
       CALL EXCH_S3D_RL( cg2d_s, 1, myThid )

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         alphaTile(bi,bj) = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
         DO j=1,sNy
          DO i=1,sNx
           cg2d_q(i,j,bi,bj) =
     &     aW2d(i  ,j  ,bi,bj)*cg2d_s(i-1,j  ,bi,bj)
     &    +aW2d(i+1,j  ,bi,bj)*cg2d_s(i+1,j  ,bi,bj)
     &    +aS2d(i  ,j  ,bi,bj)*cg2d_s(i  ,j-1,bi,bj)
     &    +aS2d(i  ,j+1,bi,bj)*cg2d_s(i  ,j+1,bi,bj)
     &    +aC2d(i  ,j  ,bi,bj)*cg2d_s(i  ,j  ,bi,bj)
#ifdef CG2D_SINGLECPU_SUM
          localBuf(i,j,bi,bj) = cg2d_s(i,j,bi,bj)*cg2d_q(i,j,bi,bj)
#else
          alphaTile(bi,bj) = alphaTile(bi,bj)
     &                     + cg2d_s(i,j,bi,bj)*cg2d_q(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef CG2D_SINGLECPU_SUM
       CALL GLOBAL_SUM_SINGLECPU_RL(localBuf, alpha, 0, 0, myThid)
#else
       CALL GLOBAL_SUM_TILE_RL( alphaTile,  alpha,  myThid )
#endif
CcnhDebugStarts
c      WRITE(*,*) ' CG2D: Iteration ', it2d-1,
c    &            ' SUM(s*q)=', alpha, ' alpha=', eta_qrN/alpha
CcnhDebugEnds
       alpha = eta_qrN/alpha

C==    Update simultaneously solution and residual vectors (and Iter number)
C      Now compute "interior" points.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         errTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           cg2d_x(i,j,bi,bj)=cg2d_x(i,j,bi,bj)+alpha*cg2d_s(i,j,bi,bj)
           cg2d_r(i,j,bi,bj)=cg2d_r(i,j,bi,bj)-alpha*cg2d_q(i,j,bi,bj)
#ifdef CG2D_SINGLECPU_SUM
           localBuf(i,j,bi,bj) = cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
#else
           errTile(bi,bj) = errTile(bi,bj)
     &                    + cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       actualIts = it2d

#ifdef CG2D_SINGLECPU_SUM
       CALL GLOBAL_SUM_SINGLECPU_RL(localBuf, err_sq, 0, 0, myThid)
#else
       CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq,    myThid )
#endif
       IF ( printResidual ) THEN
        IF ( MOD( it2d-1, printResidualFreq ).EQ.0 ) THEN
         WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &    ' cg2d: iter=', it2d, ' ; resid.= ', SQRT(err_sq)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDIF
       IF ( err_sq .LT. cg2dTolerance_sq ) GOTO 11
       IF ( err_sq .LT. minResidualSq ) THEN
C-     Store lowest residual solution
         minResidualSq = err_sq
         nIterMin = it2d
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             cg2d_min(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
       ENDIF

       CALL EXCH_S3D_RL( cg2d_r, 1, myThid )

   10 CONTINUE
   11 CONTINUE

      IF ( nIterMin.GE.0 .AND. err_sq .GT. minResidualSq ) THEN
C-    use the lowest residual solution (instead of current one = last residual)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             cg2d_x(i,j,bi,bj) = cg2d_min(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      IF (cg2dNormaliseRHS) THEN
C--   Un-normalise the answer
c       rhsMax = 1. _d 0 / rhsNorm
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)/rhsNorm
c           cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)*rhsMax
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C--   Return parameters to caller
      lastResidual = SQRT(err_sq)
      numIters = actualIts

c#ifdef ALLOW_DEBUG
c     IF ( .FALSE. ) THEN
      IF ( debugLevel.GE.debLevE .AND. printResidualFreq.EQ.1 ) THEN
        _EXCH_XY_RL(cg2d_x, myThid )
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          errTile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
            sumRHS = cg2d_b(i,j,bi,bj) -
     &      (aW2d(i  ,j  ,bi,bj)*cg2d_x(i-1,j  ,bi,bj)
     &      +aW2d(i+1,j  ,bi,bj)*cg2d_x(i+1,j  ,bi,bj)
     &      +aS2d(i  ,j  ,bi,bj)*cg2d_x(i  ,j-1,bi,bj)
     &      +aS2d(i  ,j+1,bi,bj)*cg2d_x(i  ,j+1,bi,bj)
     &      +aC2d(i  ,j  ,bi,bj)*cg2d_x(i  ,j  ,bi,bj)
     &      )
            errTile(bi,bj) = errTile(bi,bj) + sumRHS*sumRHS
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq, myThid )
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,1PE21.14)')
     &    ' cg2d: A.x - b resid.=', SQRT(err_sq)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
      ENDIF
c#endif /* ALLOW_DEBUG */

      RETURN
      END
