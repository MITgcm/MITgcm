C $Header: /u/gcmpack/MITgcm/model/src/cg2d_sr.F,v 1.4 2011/06/08 01:46:34 jmc Exp $
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
C     !ROUTINE: CG2D_SR
C     !INTERFACE:
      SUBROUTINE CG2D_SR(
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
C     |
C     | This version implements the single-reduction CG algorithm of
C     | d Azevedo, Eijkhout, and Romine (Lapack Working Note 56, 1999).
C     | C. Wolfe, November 2009, clwolfe@ucsd.edu
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
C     myThid    :: Thread on which I am working.
C     cg2d_b    :: The source term or "right hand side"
C     cg2d_x    :: The solution
C     firstResidual :: the initial residual before any iterations
C     lastResidual  :: the actual residual reached
C     numIters  :: Entry: the maximum number of iterations allowed
C                  Exit:  the actual number of iterations used
      _RL  cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  firstResidual
      _RL  lastResidual
      INTEGER numIters
      INTEGER myThid

#ifdef ALLOW_SRCG

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
      _RL    err,    errTile(nSx,nSy)
      _RL    eta_qrN,eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1
      _RL    cgBeta
      _RL    alpha,  alphaTile(nSx,nSy)
      _RL    sigma,  sigmaTile(nSx,nSy)
      _RL    delta,  deltaTile(nSx,nSy)
      _RL    sumRHS, sumRHStile(nSx,nSy)
      _RL    rhsMax
      _RL    rhsNorm
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP


C--   Initialise inverter
      eta_qrNM1 = 1. _d 0

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
      _GLOBAL_MAX_RL( rhsMax, myThid )
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
      CALL EXCH_XY_RL( cg2d_x, myThid )

C--   Initial residual calculation
      err    = 0. _d 0
      sumRHS = 0. _d 0
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
          cg2d_r(I,J,bi,bj) = cg2d_b(I,J,bi,bj) -
     &    (aW2d(I  ,J  ,bi,bj)*cg2d_x(I-1,J  ,bi,bj)
     &    +aW2d(I+1,J  ,bi,bj)*cg2d_x(I+1,J  ,bi,bj)
     &    +aS2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J-1,bi,bj)
     &    +aS2d(I  ,J+1,bi,bj)*cg2d_x(I  ,J+1,bi,bj)
     &    +aC2d(I  ,J  ,bi,bj)*cg2d_x(I  ,J  ,bi,bj)
     &    )
          errTile(bi,bj)    = errTile(bi,bj)
     &                      + cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          sumRHStile(bi,bj) = sumRHStile(bi,bj) + cg2d_b(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_S3D_RL( cg2d_r, 1,  myThid )

      CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
      CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )

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

C DER (1999) do one iteration outside of the loop to start things up.
C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
      eta_qrN = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        eta_qrNtile(bi,bj) = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
        DO J=1,sNy
         DO I=1,sNx
           cg2d_y(I,J,bi,bj) =
     &       pC(I  ,J  ,bi,bj)*cg2d_r(I  ,J  ,bi,bj)
     &      +pW(I  ,J  ,bi,bj)*cg2d_r(I-1,J  ,bi,bj)
     &      +pW(I+1,J  ,bi,bj)*cg2d_r(I+1,J  ,bi,bj)
     &      +pS(I  ,J  ,bi,bj)*cg2d_r(I  ,J-1,bi,bj)
     &      +pS(I  ,J+1,bi,bj)*cg2d_r(I  ,J+1,bi,bj)
           cg2d_s(I,J,bi,bj)  = cg2d_y(I,J,bi,bj)
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &      +cg2d_y(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_S3D_RL( cg2d_s, 1, myThid )
      CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )

      eta_qrNM1 = eta_qrN

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
      alpha = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        alphaTile(bi,bj) = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
        DO J=1,sNy
         DO I=1,sNx
          cg2d_q(I,J,bi,bj) =
     &      aW2d(I  ,J  ,bi,bj)*cg2d_s(I-1,J  ,bi,bj)
     &     +aW2d(I+1,J  ,bi,bj)*cg2d_s(I+1,J  ,bi,bj)
     &     +aS2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J-1,bi,bj)
     &     +aS2d(I  ,J+1,bi,bj)*cg2d_s(I  ,J+1,bi,bj)
     &     +aC2d(I  ,J  ,bi,bj)*cg2d_s(I  ,J  ,bi,bj)
          alphaTile(bi,bj) = alphaTile(bi,bj)
     &                     + cg2d_s(I,J,bi,bj)*cg2d_q(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( alphaTile,  alpha,  myThid )

      sigma = eta_qrN/alpha

C==   Update solution and residual vectors
C     Now compute "interior" points.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        errTile(bi,bj) = 0. _d 0
        DO J=1,sNy
         DO I=1,sNx
          cg2d_x(I,J,bi,bj)=cg2d_x(I,J,bi,bj)+sigma*cg2d_s(I,J,bi,bj)
          cg2d_r(I,J,bi,bj)=cg2d_r(I,J,bi,bj)-sigma*cg2d_q(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_S3D_RL( cg2d_r,1, myThid )

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it2d=1, numIters

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
C--    z = M^-1 r
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
         DO J=1,sNy
          DO I=1,sNx
           cg2d_y(I,J,bi,bj) =
     &       pC(I  ,J  ,bi,bj)*cg2d_r(I  ,J  ,bi,bj)
     &      +pW(I  ,J  ,bi,bj)*cg2d_r(I-1,J  ,bi,bj)
     &      +pW(I+1,J  ,bi,bj)*cg2d_r(I+1,J  ,bi,bj)
     &      +pS(I  ,J  ,bi,bj)*cg2d_r(I  ,J-1,bi,bj)
     &      +pS(I  ,J+1,bi,bj)*cg2d_r(I  ,J+1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL EXCH_S3D_RL( cg2d_y, 1, myThid )

C==    v = A.z
C--    eta_qr = <z,r>
C--    delta = <z,v>
C--    Do the error calcuation here to consolidate global reductions
       eta_qrN = 0. _d 0
       delta   = 0. _d 0
       err = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         eta_qrNtile(bi,bj) = 0. _d 0
         deltaTile(bi,bj)   = 0. _d 0
         errTile(bi,bj) = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG2D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
         DO J=1,sNy
          DO I=1,sNx
           cg2d_v(I,J,bi,bj) =
     &       aW2d(I  ,J  ,bi,bj)*cg2d_y(I-1,J  ,bi,bj)
     &      +aW2d(I+1,J  ,bi,bj)*cg2d_y(I+1,J  ,bi,bj)
     &      +aS2d(I  ,J  ,bi,bj)*cg2d_y(I  ,J-1,bi,bj)
     &      +aS2d(I  ,J+1,bi,bj)*cg2d_y(I  ,J+1,bi,bj)
     &      +aC2d(I  ,J  ,bi,bj)*cg2d_y(I  ,J  ,bi,bj)
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &      +cg2d_y(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
           deltaTile(bi,bj) = deltaTile(bi,bj)
     &      +cg2d_y(I,J,bi,bj)*cg2d_v(I,J,bi,bj)
           errTile(bi,bj) = errTile(bi,bj)
     &                    + cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C      CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )
C      CALL GLOBAL_SUM_TILE_RL( deltaTile,delta,myThid )
C      CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         sumPhi(1,bi,bj) = eta_qrNtile(bi,bj)
         sumPhi(2,bi,bj) = deltaTile(bi,bj)
         sumPhi(3,bi,bj) = errTile(bi,bj)
        ENDDO
       ENDDO

C     global_vec_sum_r8 does not call BAR2 on input
       CALL BAR2( myThid)
       CALL GLOBAL_VEC_SUM_R8(3,3,sumPhi,myThid)

       eta_qrN = sumPhi(1,1,1)
       delta   = sumPhi(2,1,1)
       err     = sumPhi(3,1,1)

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

       cgBeta   = eta_qrN/eta_qrNM1
       eta_qrNM1 = eta_qrN
       alpha = delta - cgBeta**2*alpha
       sigma = eta_qrN/alpha

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_s(I,J,bi,bj) = cg2d_y(I,J,bi,bj)
     &                       + cgBeta*cg2d_s(I,J,bi,bj)
           cg2d_x(I,J,bi,bj) = cg2d_x(I,J,bi,bj)
     &                       + sigma*cg2d_s(I,J,bi,bj)
           cg2d_q(I,J,bi,bj) = cg2d_v(I,J,bi,bj)
     &                       + cgBeta*cg2d_q(I,J,bi,bj)
           cg2d_r(I,J,bi,bj) = cg2d_r(I,J,bi,bj)
     &                       - sigma*cg2d_q(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

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

C--   Return parameters to caller
      lastResidual=actualResidual
      numIters=actualIts

C     The following exchange was moved up to solve_for_pressure
C     for compatibility with TAMC.
C     _EXCH_XY_R8(cg2d_x, myThid )
c     _BEGIN_MASTER( myThid )
c      WRITE(*,'(A,I6,1PE30.14)') ' CG2D iters, err = ',
c    & actualIts, actualResidual
c     _END_MASTER( myThid )

#endif /* ALLOW_SRCG */
      RETURN
      END
