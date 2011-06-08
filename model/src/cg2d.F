C $Header: /u/gcmpack/MITgcm/model/src/cg2d.F,v 1.54 2011/06/08 01:46:34 jmc Exp $
C $Name:  $

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
     I                cg2d_b,
     U                cg2d_x,
     O                firstResidual,
     O                lastResidual,
     U                numIters,
     I                myThid )
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
#include "CG2D.h"
c#include "GRID.h"
c#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     cg2d_b    :: The source term or "right hand side"
C     cg2d_x    :: The solution
C     firstResidual :: the initial residual before any iterations
C     lastResidual  :: the actual residual reached
C     numIters  :: Entry: the maximum number of iterations allowed
C                  Exit:  the actual number of iterations used
C     myThid    :: Thread on which I am working.
      _RL  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  firstResidual
      _RL  lastResidual
      INTEGER numIters
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ====
C     actualIts      :: Number of iterations taken
C     actualResidual :: residual
C     bi, bj     :: Block index in X and Y.
C     eta_qrN    :: Used in computing search directions
C     eta_qrNM1     suffix N and NM1 denote current and
C     cgBeta        previous iterations respectively.
C     alpha
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a
C                   useful debuggin/trouble shooting diagnostic.
C                   For neumann problems sumRHS needs to be ~0.
C                   or they converge at a non-zero residual.
C     err        :: Measure of residual of Ax - b, usually the norm.
C     I, J, it2d :: Loop counters ( it2d counts CG iterations )
      INTEGER actualIts
      _RL    actualResidual
      INTEGER bi, bj
      INTEGER I, J, it2d
c     INTEGER ks
      _RL    err,    errTile(nSx,nSy)
      _RL    eta_qrN,eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1
      _RL    cgBeta
      _RL    alpha,  alphaTile(nSx,nSy)
      _RL    sumRHS, sumRHStile(nSx,nSy)
      _RL    rhsMax
      _RL    rhsNorm
#ifdef CG2D_SINGLECPU_SUM
      _RL    localBuf(1:sNx,1:sNy,nSx,nSy)
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP


CcnhDebugStarts
C     CHARACTER*(MAX_LEN_FNAM) suff
CcnhDebugEnds


C--   Initialise inverter
      eta_qrNM1 = 1. _d 0

CcnhDebugStarts
C     _EXCH_XY_RL( cg2d_b, myThid )
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

      IF (cg2dNormaliseRHS) THEN
C-  Normalise RHS :
#ifdef LETS_MAKE_JAM
C     _GLOBAL_MAX_RL( rhsMax, myThid )
      rhsMax=1.
#else
      _GLOBAL_MAX_RL( rhsMax, myThid )
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
C- end Normalise RHS
      ENDIF

C--   Update overlaps
c     CALL EXCH_XY_RL( cg2d_b, myThid )
      CALL EXCH_XY_RL( cg2d_x, myThid )
CcnhDebugStarts
C     CALL PLOT_FIELD_XYRL( cg2d_b, 'CG2D.1 CG2D_B' , 1, myThid )
C     suff = 'normalised'
C     CALL WRITE_FLD_XY_RL (  'cg2d_b.',suff,    cg2d_b, 1, myThid)
CcnhDebugEnds

C--   Initial residual calculation
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-1,sNy+1
         DO I=1-1,sNx+1
          cg2d_s(I,J,bi,bj) = 0.
         ENDDO
        ENDDO
        sumRHStile(bi,bj) = 0. _d 0
        errTile(bi,bj)    = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
        DO J=1,sNy
         DO I=1,sNx
c         ks = kSurfC(I,J,bi,bj)
          cg2d_r(I,J,bi,bj) = cg2d_b(I,J,bi,bj) -
     &    (aW2d(I  ,J  ,bi,bj)*cg2d_x(I-1,J  ,bi,bj)
     &    +aW2d(I+1,J  ,bi,bj)*cg2d_x(I+1,J  ,bi,bj)
     &    +aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J-1,bi,bj)
     &    +aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J+1,bi,bj)
     &    +aC2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
     &    )
c    &    -aW2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
c    &    -aW2d(I+1,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
c    &    -aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
c    &    -aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
c    &    -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)*recip_Bo(i,j,bi,bj)
c    &                        *cg2d_x(I  ,J  ,bi,bj)
c    &                        /deltaTMom/deltaTfreesurf*cg2dNorm
c    &    )
#ifdef CG2D_SINGLECPU_SUM
          localBuf(I,J,bi,bj) = cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
#else
          errTile(bi,bj)    = errTile(bi,bj)
     &                      + cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          sumRHStile(bi,bj) = sumRHStile(bi,bj) + cg2d_b(I,J,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
#ifdef CG2D_SINGLECPU_SUM
      CALL GLOBAL_SUM_SINGLECPU_RL(localBuf, err, 0, 0, myThid)
      CALL GLOBAL_SUM_SINGLECPU_RL(cg2d_b, sumRHS, OLx, OLy, myThid)
#else
      CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
      CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
#endif
       err = SQRT(err)
       actualIts      = 0
       actualResidual = err

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &  ' cg2d: Sum(rhs),rhsMax = ', sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF
      firstResidual=actualResidual

      IF ( err .LT. cg2dTolerance ) GOTO 11

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
#ifdef CG2D_SINGLECPU_SUM
          localBuf(I,J,bi,bj) =
     &      cg2d_q(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
#else
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &     +cg2d_q(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
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
CcnhDebugStarts
C      WRITE(*,*) ' CG2D: Iteration ',it2d-1,' eta_qrN = ',eta_qrN
CcnhDebugEnds
       cgBeta   = eta_qrN/eta_qrNM1
CcnhDebugStarts
C      WRITE(*,*) ' CG2D: Iteration ',it2d-1,' beta = ',cgBeta
CcnhDebugEnds
       eta_qrNM1 = eta_qrN

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
         DO J=1,sNy
          DO I=1,sNx
c          ks = kSurfC(I,J,bi,bj)
           cg2d_q(I,J,bi,bj) =
     &     aW2d(I  ,J  ,bi,bj)*cg2d_s(I-1,J  ,bi,bj)
     &    +aW2d(I+1,J  ,bi,bj)*cg2d_s(I+1,J  ,bi,bj)
     &    +aS2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J-1,bi,bj)
     &    +aS2d(I  ,J+1,bi,bj)*cg2d_s(I  ,J+1,bi,bj)
     &    +aC2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
c    &    -aW2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
c    &    -aW2d(I+1,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
c    &    -aS2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
c    &    -aS2d(I  ,J+1,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
c    &    -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)*recip_Bo(i,j,bi,bj)
c    &                        *cg2d_s(I  ,J  ,bi,bj)
c    &                        /deltaTMom/deltaTfreesurf*cg2dNorm
#ifdef CG2D_SINGLECPU_SUM
          localBuf(I,J,bi,bj) = cg2d_s(I,J,bi,bj)*cg2d_q(I,J,bi,bj)
#else
          alphaTile(bi,bj) = alphaTile(bi,bj)
     &                     + cg2d_s(I,J,bi,bj)*cg2d_q(I,J,bi,bj)
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
C      WRITE(*,*) ' CG2D: Iteration ',it2d-1,' SUM(s*q)= ',alpha
CcnhDebugEnds
       alpha = eta_qrN/alpha
CcnhDebugStarts
C      WRITE(*,*) ' CG2D: Iteration ',it2d-1,' alpha= ',alpha
CcnhDebugEnds

C==    Update solution and residual vectors
C      Now compute "interior" points.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         errTile(bi,bj) = 0. _d 0
         DO J=1,sNy
          DO I=1,sNx
           cg2d_x(I,J,bi,bj)=cg2d_x(I,J,bi,bj)+alpha*cg2d_s(I,J,bi,bj)
           cg2d_r(I,J,bi,bj)=cg2d_r(I,J,bi,bj)-alpha*cg2d_q(I,J,bi,bj)
#ifdef CG2D_SINGLECPU_SUM
           localBuf(I,J,bi,bj) = cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
#else
           errTile(bi,bj) = errTile(bi,bj)
     &                    + cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef CG2D_SINGLECPU_SUM
       CALL GLOBAL_SUM_SINGLECPU_RL(localBuf, err, 0, 0, myThid)
#else
       CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
#endif
       err = SQRT(err)
       actualIts      = it2d
       actualResidual = err
       IF ( printResidual ) THEN
        IF ( MOD( it2d-1, printResidualFreq ).EQ.0 ) THEN
         WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &    ' cg2d: iter=', actualIts, ' ; resid.= ', actualResidual
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDIF
       IF ( err .LT. cg2dTolerance ) GOTO 11

       CALL EXCH_S3D_RL( cg2d_r, 1, myThid )

   10 CONTINUE
   11 CONTINUE

      IF (cg2dNormaliseRHS) THEN
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
      ENDIF

C     The following exchange was moved up to solve_for_pressure
C     for compatibility with TAMC.
C     _EXCH_XY_RL(cg2d_x, myThid )

C--   Return parameters to caller
      lastResidual = actualResidual
      numIters = actualIts

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
C     _GLOBAL_SUM_RL( err   , myThid )
C     write(*,*) 'cg2d: Ax - b = ',SQRT(err)
CcnhDebugEnds

      RETURN
      END
