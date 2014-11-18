C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diag_cg2d.F,v 1.6 2014/11/18 23:53:04 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"
#undef DEBUG_DIAG_CG2D

CBOP
C     !ROUTINE: DIAG_CG2D
C     !INTERFACE:
      SUBROUTINE DIAG_CG2D(
     I                aW2d, aS2d, b2d,
     I                offDiagFactor, residCriter,
     O                firstResidual, minResidual, lastResidual,
     U                x2d, numIters,
     O                nIterMin,
     I                printResidFrq, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CG2D
C     | o Two-dimensional grid problem conjugate-gradient
C     |   inverter (with preconditioner).
C     *==========================================================*
C     | Con. grad is an iterative procedure for solving Ax = b.
C     | It requires the A be symmetric.
C     | This implementation assumes A is a five-diagonal
C     | matrix of the form that arises in the discrete
C     | representation of the del^2 operator in a
C     | two-dimensional space.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     b2d           :: The source term or "right hand side"
C     x2d           :: The solution
C     offDiagFactor :: preconditioner off-diagonal factor
C     residCriter   :: residual target for convergence
C     firstResidual :: the initial residual before any iterations
C     minResidual   :: the lowest residual reached
C     lastResidual  :: the actual residual reached
C     numIters  :: Entry: the maximum number of iterations allowed
C                  Exit:  the actual number of iterations used
C     nIterMin      :: iteration number corresponding to lowest residual
C     printResidFrq :: Frequency for printing residual in CG iterations
C     myThid        :: my Thread Id number
      _RS  aW2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  b2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  residCriter
      _RL  offDiagFactor
      _RL  firstResidual
      _RL  minResidual
      _RL  lastResidual
      _RL  x2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER numIters
      INTEGER nIterMin
      INTEGER printResidFrq
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ====
C     bi, bj     :: tile indices
C     eta_qrN    :: Used in computing search directions
C     eta_qrNM1     suffix N and NM1 denote current and
C     cgBeta        previous iterations respectively.
C     alpha
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a
C                   useful debuggin/trouble shooting diagnostic.
C                   For neumann problems sumRHS needs to be ~0.
C                   or they converge at a non-zero residual.
C     err        :: Measure of current residual of Ax - b, usually the norm.
C     i, j, it2d :: Loop counters ( it2d counts CG iterations )
      INTEGER bi, bj
      INTEGER i, j, it2d
      _RL    err,     errTile(nSx,nSy)
      _RL    eta_qrN, eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1
      _RL    cgBeta
      _RL    alpha,  alphaTile(nSx,nSy)
      _RL    sumRHS, sumRHStile(nSx,nSy)
      _RL    pW_tmp, pS_tmp
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP
      _RS  aC2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pW  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pS  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  q2d(0:sNx+1,0:sNy+1,nSx,nSy)
#ifdef DEBUG_DIAG_CG2D
      CHARACTER*(10) sufx
      _RL  r2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      _RL  r2d(0:sNx+1,0:sNy+1,nSx,nSy)
#endif
      _RL  s2d(0:sNx+1,0:sNy+1,nSx,nSy)
      _RL  x2dm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DIAG_CG2D',myThid)
#endif

C--   Set matrice main diagnonal:
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C-    Initialise overlap regions (in case EXCH call do not fill them all)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
           aC2d(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
           aC2d(i,j,bi,bj) = -( ( aW2d(i,j,bi,bj)+aW2d(i+1,j,bi,bj) )
     &                         +( aS2d(i,j,bi,bj)+aS2d(i,j+1,bi,bj) )
     &                        )
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_XY_RS(aC2d, myThid)

C--   Initialise preconditioner
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy+1
         DO i=1,sNx+1
          IF ( aC2d(i,j,bi,bj) .EQ. 0. ) THEN
           pC(i,j,bi,bj) = 1. _d 0
          ELSE
           pC(i,j,bi,bj) = 1. _d 0 / aC2d(i,j,bi,bj)
          ENDIF
          pW_tmp = aC2d(i,j,bi,bj)+aC2d(i-1,j,bi,bj)
          IF ( pW_tmp .EQ. 0. ) THEN
           pW(i,j,bi,bj) = 0.
          ELSE
           pW(i,j,bi,bj) = -aW2d(i,j,bi,bj)*offDiagFactor
     &                     *4. _d 0/( pW_tmp*pW_tmp )
          ENDIF
          pS_tmp = aC2d(i,j,bi,bj)+aC2d(i,j-1,bi,bj)
          IF ( pS_tmp .EQ. 0. ) THEN
           pS(i,j,bi,bj) = 0.
          ELSE
           pS(i,j,bi,bj) = -aS2d(i,j,bi,bj)*offDiagFactor
     &                     *4. _d 0/( pS_tmp*pS_tmp )
          ENDIF
c         pC(i,j,bi,bj) = 1.
c         pW(i,j,bi,bj) = 0.
c         pS(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Initialise inverter
      eta_qrNM1 = 1. _d 0

      CALL EXCH_XY_RL( x2d, myThid )

C--   Initial residual calculation
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy+1
         DO i=0,sNx+1
          r2d(i,j,bi,bj) = 0.
          s2d(i,j,bi,bj) = 0.
          x2dm(i,j,bi,bj) = x2d(i,j,bi,bj)
         ENDDO
        ENDDO
        sumRHStile(bi,bj) = 0. _d 0
        errTile(bi,bj)    = 0. _d 0
        DO j=1,sNy
         DO i=1,sNx
          r2d(i,j,bi,bj) = b2d(i,j,bi,bj) -
     &      (aW2d(i  ,j  ,bi,bj)*x2d(i-1,j  ,bi,bj)
     &      +aW2d(i+1,j  ,bi,bj)*x2d(i+1,j  ,bi,bj)
     &      +aS2d(i  ,j  ,bi,bj)*x2d(i  ,j-1,bi,bj)
     &      +aS2d(i  ,j+1,bi,bj)*x2d(i  ,j+1,bi,bj)
     &      +aC2d(i  ,j  ,bi,bj)*x2d(i  ,j  ,bi,bj)
     &      )
          errTile(bi,bj) = errTile(bi,bj)
     &                  + r2d(i,j,bi,bj)*r2d(i,j,bi,bj)
          sumRHStile(bi,bj) = sumRHStile(bi,bj) + b2d(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef DEBUG_DIAG_CG2D
      CALL EXCH_XY_RL ( r2d, myThid )
#else
      CALL EXCH_S3D_RL( r2d, 1, myThid )
#endif
      CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
      IF ( printResidFrq.GE.1 )
     &  CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
      err = SQRT(err)
      it2d = 0
      firstResidual = err
      minResidual   = err
      nIterMin = it2d

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidFrq.GE.1
        IF ( printResidual ) THEN
         WRITE(msgBuf,'(2A,I6,A,1PE17.9,A,1PE14.6)')' diag_cg2d:',
     &    ' iter=', it2d, ' ; resid.=', err, ' ; sumRHS=', sumRHS
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
        _END_MASTER( myThid )
      ENDIF
#ifdef DEBUG_DIAG_CG2D
      IF ( printResidFrq.GE.1 ) THEN
        WRITE(sufx,'(I10.10)') 0
        CALL WRITE_FLD_XY_RL( 'r2d.',sufx, r2d, 1, myThid )
      ENDIF
#endif

      IF ( err .LT. residCriter ) GOTO 11

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it2d=1, numIters

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         eta_qrNtile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           q2d(i,j,bi,bj) =
     &        pC(i  ,j  ,bi,bj)*r2d(i  ,j  ,bi,bj)
     &       +pW(i  ,j  ,bi,bj)*r2d(i-1,j  ,bi,bj)
     &       +pW(i+1,j  ,bi,bj)*r2d(i+1,j  ,bi,bj)
     &       +pS(i  ,j  ,bi,bj)*r2d(i  ,j-1,bi,bj)
     &       +pS(i  ,j+1,bi,bj)*r2d(i  ,j+1,bi,bj)
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &       +q2d(i,j,bi,bj)*r2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )
       cgBeta   = eta_qrN/eta_qrNM1
       eta_qrNM1 = eta_qrN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           s2d(i,j,bi,bj) = q2d(i,j,bi,bj)
     &                    + cgBeta*s2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--    Do exchanges that require messages i.e. between processes.
       CALL EXCH_S3D_RL( s2d, 1, myThid )

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         alphaTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           q2d(i,j,bi,bj) =
     &       aW2d(i  ,j  ,bi,bj)*s2d(i-1,j  ,bi,bj)
     &      +aW2d(i+1,j  ,bi,bj)*s2d(i+1,j  ,bi,bj)
     &      +aS2d(i  ,j  ,bi,bj)*s2d(i  ,j-1,bi,bj)
     &      +aS2d(i  ,j+1,bi,bj)*s2d(i  ,j+1,bi,bj)
     &      +aC2d(i  ,j  ,bi,bj)*s2d(i  ,j  ,bi,bj)
           alphaTile(bi,bj) = alphaTile(bi,bj)
     &                      + s2d(i,j,bi,bj)*q2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL( alphaTile,  alpha,  myThid )
       alpha = eta_qrN/alpha

C==    Update solution and residual vectors
C      Now compute "interior" points.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         errTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           x2d(i,j,bi,bj)=x2d(i,j,bi,bj)+alpha*s2d(i,j,bi,bj)
           r2d(i,j,bi,bj)=r2d(i,j,bi,bj)-alpha*q2d(i,j,bi,bj)
           errTile(bi,bj) = errTile(bi,bj)
     &                    + r2d(i,j,bi,bj)*r2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
       err = SQRT(err)
       IF ( printResidual ) THEN
        IF ( MOD( it2d-1, printResidFrq ).EQ.0 ) THEN
         WRITE(msgBuf,'(A,I6,A,1PE17.9)')
     &    ' diag_cg2d: iter=', it2d, ' ; resid.=', err
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDIF
       IF ( err .LT. residCriter ) GOTO 11
       IF ( err .LT. minResidual ) THEN
C-     Store lowest residual solution
         minResidual = err
         nIterMin = it2d
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             x2dm(i,j,bi,bj) = x2d(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
       ENDIF

#ifdef DEBUG_DIAG_CG2D
       CALL EXCH_XY_RL( r2d, myThid )
       IF ( printResidFrq.GE.1 ) THEN
        WRITE(sufx,'(I10.10)') it2d
        CALL WRITE_FLD_XY_RL( 'r2d.',sufx, r2d, 1, myThid )
        CALL WRITE_FLD_XY_RL( 'x2d.',sufx, x2d, 1, myThid )
       ENDIF
#else
       CALL EXCH_S3D_RL( r2d, 1, myThid )
#endif

   10 CONTINUE
      it2d = numIters
   11 CONTINUE

C--   Return parameters to caller
      lastResidual = err
      numIters = it2d

      IF ( err .GT. minResidual ) THEN
C-    use the lowest residual solution (instead of current one <-> last residual)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             x2d(i,j,bi,bj) = x2dm(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
c     CALL EXCH_XY_RL( x2d, myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DIAG_CG2D',myThid)
#endif

      RETURN
      END
