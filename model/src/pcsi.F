#include "CPP_OPTIONS.h"
#ifdef TARGET_NEC_SX
C     set a sensible default for the outer loop unrolling parameter that can
C     be overriden in the Makefile with the DEFINES macro or in CPP_OPTIONS.h
#ifndef CG2D_OUTERLOOPITERS
# define CG2D_OUTERLOOPITERS 10
#endif
#endif /* TARGET_NEC_SX */
C#define xy_rl

CBOP
C     !ROUTINE: PCSI
C     !INTERFACE:
      SUBROUTINE PCSI(
     U                pcsi_b, pcsi_x,
     O                firstResidual, minResidualSq, lastResidual,
     U                numIters, nIterMin,
     I                myThid )
C     !DESCRIPTION: \bv
C     *========================================================================*
C     | SUBROUTINE PCSI
C     | o Preconditioned Classical Stiefel Iteration (PCSi) solver for solving
C     | o the linear system $Ax=b$ (with preconditioner).
C     *========================================================================*
C     | It uses the two extreme eigenvalues of $A$ instead of the norm of
C     | residual. Thus, PCSI eliminates global reductions in each iteration.
C     | The eigenvalues are estimated by routine eigen_lanczos.
C     | PCSI supports all kinds of preconditioners supported by PCG/ChronGear,
C     | including diagonal and EVP preconditioning. It requires the A be
C     | symmetric and positive-definite. This implementation assumes A is a
C     | five-diagonal matrix of the form that arises in the discrete
C     | representation of the del^2 operator in a two-dimensional space.
C     *========================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"

C     !INPUT/OUTPUT PARAMETERS:
C     pcsi_b    :: The source term or "right hand side" (output: normalised RHS)
C     pcsi_x    :: The solution (input: first guess)
C     firstResidual :: the initial residual before any iterations
C     minResidualSq :: the lowest residual reached (squared)
C     lastResidual  :: the actual residual reached
C     numIters  :: Inp: the maximum number of iterations allowed
C                  Out: the actual number of iterations used
C     nIterMin  :: Inp: decide to store (if >=0) or not (if <0) lowest res. sol.
C                  Out: iteration number corresponding to lowest residual
C     myThid    :: Thread on which I am working.
      _RL  pcsi_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pcsi_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  firstResidual
      _RL  minResidualSq
      _RL  lastResidual
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid

#ifdef ALLOW_PCSI
C     !LOCAL VARIABLES:
C     bi, bj     :: tile index in X and Y.
C     i, j, it2d :: Loop counters ( it2d counts iterations )
C     actualIts  :: actual iteration number
C     err_sq     :: Measure of the square of the residual of Ax - b.
C     csalpha    :: iteration parameters by eigenvalues
C     csbeta     :: iteration parameters by eigenvalues
C     csy        :: iteration parameters by eigenvalues
C     csomga     :: iteration parameters by eigenvalues
C     pcsi_min   :: used to store solution corresponding to lowest residual.
C     pcsi_t     :: local copy of psci_x with smaller overlap
C     msgBuf     :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, it2d
      INTEGER actualIts
      _RL    pcsiTolerance_sq
      _RL    err_sq, errTile(nSx,nSy)
      _RL    rhsMax
      _RL    rhsNorm
      _RL    pcsi_min(1:sNx,1:sNy,nSx,nSy)
      _RL    pcsi_q(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL    pcsi_r(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL    pcsi_s(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL    pcsi_t(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL    csalpha,csbeta,csy,csomga,recip_csy
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP

C--   Compute iteration parameters by eigenvalues
      csalpha = 2. _d 0/(PcsiMaxEigs-PcsiMinEigs)
      csbeta = (PcsiMaxEigs+PcsiMinEigs)/(PcsiMaxEigs-PcsiMinEigs)
      csy = csbeta/csalpha
      csomga = 2. _d 0/csy
      recip_csy = 1. _d 0/csy

C--   Initialise auxiliary constant, some output variable and inverter
      pcsiTolerance_sq = PcsiTolerance*PcsiTolerance
      minResidualSq = -1. _d 0
      firstResidual = -1. _d 0

C--   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          pcsi_b(i,j,bi,bj) = pcsi_b(i,j,bi,bj)*pcsiNorm
          rhsMax = MAX(ABS(pcsi_b(i,j,bi,bj)),rhsMax)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF (pcsiNormaliseRHS) THEN
C-  Normalise RHS :
      _GLOBAL_MAX_RL( rhsMax, myThid )
      rhsNorm = 1. _d 0
      IF ( rhsMax .NE. 0. ) rhsNorm = 1. _d 0 / rhsMax
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          pcsi_b(i,j,bi,bj) = pcsi_b(i,j,bi,bj)*rhsNorm
          pcsi_x(i,j,bi,bj) = pcsi_x(i,j,bi,bj)*rhsNorm
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C- end Normalise RHS
      ENDIF

C--   Update overlaps
      CALL EXCH_XY_RL( pcsi_x, myThid )

C--   Initial residual calculation
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C-    Initialise local working arrays:
        DO j=0,sNy+1
         DO i=0,sNx+1
          pcsi_q(i,j,bi,bj) = 0. _d 0
          pcsi_r(i,j,bi,bj) = 0. _d 0
          pcsi_s(i,j,bi,bj) = 0. _d 0
          pcsi_t(i,j,bi,bj) = pcsi_x(i,j,bi,bj)
         ENDDO
        ENDDO
        IF ( nIterMin.GE.0 ) THEN
C-    Initialise saved solution
         DO j=1,sNy
          DO i=1,sNx
           pcsi_min(i,j,bi,bj) = pcsi_t(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        errTile(bi,bj) = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
        DO j=1,sNy
         DO i=1,sNx
          pcsi_r(i,j,bi,bj) = pcsi_b(i,j,bi,bj) -
     &    (aW2d(i  ,j  ,bi,bj)*pcsi_t(i-1,j  ,bi,bj)
     &    +aW2d(i+1,j  ,bi,bj)*pcsi_t(i+1,j  ,bi,bj)
     &    +aS2d(i  ,j  ,bi,bj)*pcsi_t(i  ,j-1,bi,bj)
     &    +aS2d(i  ,j+1,bi,bj)*pcsi_t(i  ,j+1,bi,bj)
     &    +aC2d(i  ,j  ,bi,bj)*pcsi_t(i  ,j  ,bi,bj)
     &    )
          errTile(bi,bj) = errTile(bi,bj)
     &                 + pcsi_r(i,j,bi,bj)*pcsi_r(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_S3D_RL( pcsi_r, 1,  myThid )

      CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq, myThid )

      it2d      = 0
      actualIts = 0
      firstResidual = SQRT(err_sq)
      IF ( nIterMin.GE.0 ) THEN
        nIterMin = 0
        minResidualSq = err_sq
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           pcsi_s(i,j,bi,bj) =
     &       pC(i  ,j  ,bi,bj)*pcsi_r(i  ,j  ,bi,bj)
     &      +pW(i  ,j  ,bi,bj)*pcsi_r(i-1,j  ,bi,bj)
     &      +pW(i+1,j  ,bi,bj)*pcsi_r(i+1,j  ,bi,bj)
     &      +pS(i  ,j  ,bi,bj)*pcsi_r(i  ,j-1,bi,bj)
     &      +pS(i  ,j+1,bi,bj)*pcsi_r(i  ,j+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          pcsi_q(i,j,bi,bj) = recip_csy*pcsi_s(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          pcsi_t(i,j,bi,bj) = pcsi_t(i,j,bi,bj) + pcsi_q(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_S3D_RL(pcsi_t, 1, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          pcsi_r(i,j,bi,bj) = pcsi_b(i,j,bi,bj) -
     &    (aW2d(i  ,j  ,bi,bj)*pcsi_t(i-1,j  ,bi,bj)
     &    +aW2d(i+1,j  ,bi,bj)*pcsi_t(i+1,j  ,bi,bj)
     &    +aS2d(i  ,j  ,bi,bj)*pcsi_t(i  ,j-1,bi,bj)
     &    +aS2d(i  ,j+1,bi,bj)*pcsi_t(i  ,j+1,bi,bj)
     &    +aC2d(i  ,j  ,bi,bj)*pcsi_t(i  ,j  ,bi,bj)
     &    )
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_S3D_RL( pcsi_r, 1,  myThid )

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &  ' pcsi: Sum(rhs),rhsMax = ', 0.,rhsMax
        _END_MASTER( myThid )
      ENDIF

C     iterate
      iterationLoop: DO it2d = 1, numIters
C--     step 3 : update iteration parameter
        csomga = 1. _d 0/(csy-csomga/(4. _d 0*csalpha*csalpha))

C--     step 4 : preconditioning
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             pcsi_s(i,j,bi,bj) =
     &         pC(i  ,j  ,bi,bj)*pcsi_r(i  ,j  ,bi,bj)
     &        +pW(i  ,j  ,bi,bj)*pcsi_r(i-1,j  ,bi,bj)
     &        +pW(i+1,j  ,bi,bj)*pcsi_r(i+1,j  ,bi,bj)
     &        +pS(i  ,j  ,bi,bj)*pcsi_r(i  ,j-1,bi,bj)
     &        +pS(i  ,j+1,bi,bj)*pcsi_r(i  ,j+1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C--     step 5 : compute X increment and update X and R
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            pcsi_q(i,j,bi,bj) = csomga*pcsi_s(i,j,bi,bj)+
     &                        (csy*csomga-1. _d 0)*pcsi_q(i,j,bi,bj)
            pcsi_t(i,j,bi,bj) = pcsi_t(i,j,bi,bj) + pcsi_q(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL EXCH_S3D_RL(pcsi_t, 1, myThid)

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            pcsi_r(i,j,bi,bj) = pcsi_b(i,j,bi,bj) -
     &      (aW2d(i  ,j  ,bi,bj)*pcsi_t(i-1,j  ,bi,bj)
     &      +aW2d(i+1,j  ,bi,bj)*pcsi_t(i+1,j  ,bi,bj)
     &      +aS2d(i  ,j  ,bi,bj)*pcsi_t(i  ,j-1,bi,bj)
     &      +aS2d(i  ,j+1,bi,bj)*pcsi_t(i  ,j+1,bi,bj)
     &      +aC2d(i  ,j  ,bi,bj)*pcsi_t(i  ,j  ,bi,bj)
     &      )
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        actualIts = it2d
C     test for convergence
        IF ((MOD(it2d, PcsiconvergenceCheckFreq) .EQ. 0) .AND.
     &          (it2d .GE. PcsiconvergenceCheckStart)) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           errTile(bi,bj) = 0. _d 0
           DO j=1,sNy
            DO i=1,sNx
             errTile(bi,bj) = errTile(bi,bj)
     &                    + pcsi_r(i,j,bi,bj)*pcsi_r(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq,    myThid )

         IF ( err_sq .LT. minResidualSq ) THEN
C-       Store lowest residual solution
           minResidualSq = err_sq
           nIterMin = it2d
           DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
             DO j=1,sNy
              DO i=1,sNx
               pcsi_min(i,j,bi,bj) = pcsi_t(i,j,bi,bj)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
         ENDIF

         IF ( printResidual ) THEN
          IF ( MOD( it2d-1, printResidualFreq ).EQ.0 ) THEN
           WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &          ' pcsi: iter=', it2d, ' ; resid.= ', SQRT(err_sq)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid )
          ENDIF
         ENDIF
         IF (err_sq .LT. pcsiTolerance_sq) THEN
           actualIts = it2d
           EXIT iterationLoop
         ENDIF
        ENDIF
C     We save one exchange by moving the exchange call to the end of the
C     loop. The full pcsi_r is only needed at the beginning of the loop.
        CALL EXCH_S3D_RL( pcsi_r, 1,  myThid )

      ENDDO iterationLoop

      IF ( nIterMin.GE.0 .AND. err_sq .GT. minResidualSq ) THEN
C-    use the lowest residual solution (instead of current one = last residual)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             pcsi_x(i,j,bi,bj) = pcsi_min(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ELSE
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             pcsi_x(i,j,bi,bj) = pcsi_t(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      IF (pcsiNormaliseRHS) THEN
C--   Un-normalise the answer
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            pcsi_x(i,j,bi,bj) = pcsi_x(i,j,bi,bj)/rhsNorm
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C--   Return parameters to caller
      lastResidual = SQRT(err_sq)
      numIters = actualIts

      IF ( printResidual ) THEN
       IF ( MOD( it2d-1, printResidualFreq ).EQ.0 ) THEN
        WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &   ' pcsi: iter=', numIters, ' ; resid.= ', lastResidual
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

#endif /* ALLOW_PCSI */

      RETURN
      END
