#include "CPP_OPTIONS.h"
#ifdef TARGET_NEC_SX
C     set a sensible default for the outer loop unrolling parameter that can
C     be overriden in the Makefile with the DEFINES macro or in CPP_OPTIONS.h
#ifndef CG2D_OUTERLOOPITERS
# define CG2D_OUTERLOOPITERS 10
#endif
#endif /* TARGET_NEC_SX */

CBOP
C     !ROUTINE: CG2D_EX0
C     !INTERFACE:
      SUBROUTINE CG2D_EX0(
     U                cg2d_b, cg2d_x,
     O                firstResidual, minResidualSq, lastResidual,
     U                numIters, nIterMin,
     I                myThid )
C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE CG2D_EX0
C     | o Two-dimensional grid problem conjugate-gradient inverter
C     |   (with preconditioner).
C     | This is the disconnected-tile version (each tile treated
C     |   independently as a small domain, with locally periodic
C     |   BC at the edges.
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
      INTEGER actualIts(nSx,nSy)
      INTEGER minResIter(nSx,nSy)
      _RL    cg2dTolerance_sq
      _RL    err_sq,  errTile(nSx,nSy)
      _RL    eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1(nSx,nSy)
      _RL    cgBeta
      _RL    alpha,   alphaTile(nSx,nSy)
      _RL    sumRHS,  sumRHStile(nSx,nSy)
      _RL    rhsMax, rhsMaxLoc
      _RL    rhsNorm(nSx,nSy)
      _RL    minResTile(nSx,nSy)
      _RL    cg2d_min(1:sNx,1:sNy,nSx,nSy)
      _RL    cg2d_q  (1:sNx,1:sNy,nSx,nSy)
      _RL    cg2d_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL    cg2d_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP

C--   Initialise auxiliary constant, some output variable
      cg2dTolerance_sq = cg2dTolerance*cg2dTolerance

C--   Initialise inverter and Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        actualIts(bi,bj)  = 0
        minResIter(bi,bj) = 0
        minResTile(bi,bj) = -1. _d 0
        eta_qrNM1(bi,bj) =  1. _d 0
        rhsMaxLoc = 0. _d 0
        DO j=1,sNy
         DO i=1,sNx
          cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)*cg2dNorm
          rhsMaxLoc = MAX(ABS(cg2d_b(i,j,bi,bj)),rhsMaxLoc)
         ENDDO
        ENDDO
        rhsNorm(bi,bj) = 1. _d 0
        IF ( rhsMaxLoc .NE. 0. ) rhsNorm(bi,bj) = 1. _d 0 / rhsMaxLoc
        IF (cg2dNormaliseRHS) THEN
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)*rhsNorm(bi,bj)
           cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)*rhsNorm(bi,bj)
          ENDDO
         ENDDO
        ENDIF
        rhsMax = MAX( rhsMaxLoc, rhsMax )

       ENDDO
      ENDDO
      _GLOBAL_MAX_RL( rhsMax, myThid )

C--   Update overlaps
      CALL EXCH_XY_RL( cg2d_x, myThid )

C--   Initial residual calculation
      err_sq = 0.
      sumRHS = 0.
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
          errTile(bi,bj)    = errTile(bi,bj)
     &                      + cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
          sumRHStile(bi,bj) = sumRHStile(bi,bj) + cg2d_b(i,j,bi,bj)
         ENDDO
        ENDDO
        IF ( nIterMin.GE.0 ) THEN
          minResTile(bi,bj) = errTile(bi,bj)
        ENDIF
        err_sq = MAX( errTile(bi,bj), err_sq )
        sumRHS = MAX( ABS(sumRHStile(bi,bj)), sumRHS )

       ENDDO
      ENDDO
      CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
      _GLOBAL_MAX_RL( err_sq, myThid )
      _GLOBAL_MAX_RL( sumRHS, myThid )
      firstResidual = SQRT(err_sq)

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &  ' cg2d: Sum(rhs),rhsMax = ', sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF

c     IF ( err_sq .LT. cg2dTolerance_sq ) GOTO 11

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO it2d=1, numIters
      IF ( err_sq.GE.cg2dTolerance_sq ) THEN
       err_sq = 0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( errTile(bi,bj).GE.cg2dTolerance_sq ) THEN

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
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
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &     +cg2d_q(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
          ENDDO
         ENDDO

         cgBeta   = eta_qrNtile(bi,bj)/eta_qrNM1(bi,bj)
         eta_qrNM1(bi,bj) = eta_qrNtile(bi,bj)

         DO j=1,sNy
          DO i=1,sNx
           cg2d_s(i,j,bi,bj) = cg2d_q(i,j,bi,bj)
     &                       + cgBeta*cg2d_s(i,j,bi,bj)
          ENDDO
         ENDDO

C--    Do exchanges that require messages i.e. between processes.
         CALL FILL_HALO_LOCAL_RL(
     U                  cg2d_s(0,0,bi,bj),
     I                  1, 1, 1, 1, 1,
     I                  EXCH_IGNORE_CORNERS, bi, bj, myThid )

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
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
          alphaTile(bi,bj) = alphaTile(bi,bj)
     &                     + cg2d_s(i,j,bi,bj)*cg2d_q(i,j,bi,bj)
          ENDDO
         ENDDO
         alpha = eta_qrNtile(bi,bj)/alphaTile(bi,bj)

C==    Update simultaneously solution and residual vectors (and Iter number)
C      Now compute "interior" points.
         errTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           cg2d_x(i,j,bi,bj)=cg2d_x(i,j,bi,bj)+alpha*cg2d_s(i,j,bi,bj)
           cg2d_r(i,j,bi,bj)=cg2d_r(i,j,bi,bj)-alpha*cg2d_q(i,j,bi,bj)
           errTile(bi,bj) = errTile(bi,bj)
     &                    + cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
          ENDDO
         ENDDO
         actualIts(bi,bj) = it2d

         IF ( printResidual ) THEN
          IF ( MOD( it2d-1, printResidualFreq ).EQ.0 ) THEN
           WRITE(msgBuf,'(A,2I4,A,I6,A,1PE21.14)') ' cg2d(bi,bj=', bi,
     &      bj, '): iter=', it2d, ' ; resid.= ', SQRT(errTile(bi,bj))
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
         ENDIF
         IF ( errTile(bi,bj) .GE. cg2dTolerance_sq .AND.
     &        errTile(bi,bj) .LT. minResTile(bi,bj) ) THEN
C-     Store lowest residual solution
           minResTile(bi,bj) = errTile(bi,bj)
           minResIter(bi,bj) = it2d
           DO j=1,sNy
            DO i=1,sNx
             cg2d_min(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         CALL FILL_HALO_LOCAL_RL(
     U                  cg2d_r(0,0,bi,bj),
     I                  1, 1, 1, 1, 1,
     I                  EXCH_IGNORE_CORNERS, bi, bj, myThid )

        ENDIF
        err_sq = MAX( errTile(bi,bj), err_sq )
C-    end bi,bj loops
       ENDDO
       ENDDO
C-    end cg-2d iteration loop
      ENDIF
      ENDDO

c  11 CONTINUE

      IF ( nIterMin.GE.0 ) THEN
C-    use the lowest residual solution (instead of current one = last residual)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
c         minResidualSq = MAX( minResTile(bi,bj), minResidualSq )
c         nIterMin = MAX( minResIter(bi,bj), nIterMin )
         IF ( errTile(bi,bj) .GT. minResTile(bi,bj) ) THEN
          DO j=1,sNy
           DO i=1,sNx
             cg2d_x(i,j,bi,bj) = cg2d_min(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      IF (cg2dNormaliseRHS) THEN
C--   Un-normalise the answer
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)/rhsNorm(bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C--   Return parameters to caller
C     return largest Iter # and Max residual     in numIters and lastResidual
C     return lowest  Iter # and Min residual(^2) in nIterMin and minResidualSq
      _GLOBAL_MAX_RL( err_sq, myThid )
      nIterMin = numIters
      numIters = 0
      minResidualSq = err_sq
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         nIterMin = MIN( actualIts(bi,bj), nIterMin )
         numIters = MAX( actualIts(bi,bj), numIters )
         minResidualSq = MIN( errTile(bi,bj), minResidualSq )
       ENDDO
      ENDDO
      lastResidual = SQRT(err_sq)
      alpha = -nIterMin
      _GLOBAL_MAX_RL( alpha, myThid )
      nIterMin = NINT( -alpha )
      alpha = numIters
      _GLOBAL_MAX_RL( alpha, myThid )
      numIters = NINT( alpha )
      alpha = -minResidualSq
      _GLOBAL_MAX_RL( alpha, myThid )
      minResidualSq = -alpha

      RETURN
      END
