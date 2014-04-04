C $Header: /u/gcmpack/MITgcm/model/src/cg2d_nsa.F,v 1.7 2014/04/04 20:54:11 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CML THIS DOES NOT WORK +++++
#undef ALLOW_LOOP_DIRECTIVE
CBOP
C     !ROUTINE: CG2D_NSA
C     !INTERFACE:
      SUBROUTINE CG2D_NSA(
     U                cg2d_b, cg2d_x,
     O                firstResidual, minResidualSq, lastResidual,
     U                numIters, nIterMin,
     I                myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CG2D_NSA
C     | o Two-dimensional grid problem conjugate-gradient
C     |   inverter (with preconditioner).
C     | o This version is used only in the case when the matrix
C     |   operator is not "self-adjoint" (NSA). Any remaining
C     |   residuals will immediately reported to the department
C     |   of homeland security.
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
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     cg2d_b    :: The source term or "right hand side" (Output: normalised RHS)
C     cg2d_x    :: The solution (Input: first guess)
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

#ifdef ALLOW_CG2D_NSA
C     !LOCAL VARIABLES:
C     === Local variables ====
C     bi, bj     :: tile index in X and Y.
C     i, j, it2d :: Loop counters ( it2d counts CG iterations )
C     actualIts  :: actual CG iteration number
C     err_sq     :: Measure of the square of the residual of Ax - b.
C     eta_qrN    :: Used in computing search directions; suffix N and NM1
C     eta_qrNM1     denote current and previous iterations respectively.
C     recip_eta_qrNM1 :: reciprocal of eta_qrNM1
C     cgBeta     :: coeff used to update conjugate direction vector "s".
C     alpha      :: coeff used to update solution & residual
C     alphaSum   :: to avoid the statement: alpha = 1./alpha (for TAMC/TAF)
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a useful
C                   debugging/trouble shooting diagnostic. For neumann problems
C                   sumRHS needs to be ~0 or it converge at a non-zero residual.
C     cg2d_min   :: used to store solution corresponding to lowest residual.
C     msgBuf     :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, it2d
      INTEGER actualIts
      _RL    cg2dTolerance_sq
      _RL    err_sq,  errTile(nSx,nSy)
      _RL    eta_qrN, eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1, recip_eta_qrNM1
      _RL    cgBeta,  alpha
      _RL    alphaSum,alphaTile(nSx,nSy)
      _RL    sumRHS,  sumRHStile(nSx,nSy)
      _RL    rhsMax,  rhsNorm
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( numIters .GT. numItersMax ) THEN
        WRITE(msgBuf,'(A,I10)')
     &    'CG2D_NSA: numIters > numItersMax =', numItersMax
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CG2D_NSA'
      ENDIF
      IF ( cg2dNormaliseRHS ) THEN
        WRITE(msgBuf,'(A)') 'CG2D_NSA: cg2dNormaliseRHS is disabled'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &    'set cg2dTargetResWunit (instead of cg2dTargetResidual)'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CG2D_NSA'
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = myThid - 1
          max1 = nTx*nTy
          act2 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Initialise auxiliary constant, some output variable and inverter
      cg2dTolerance_sq = cg2dTolerance*cg2dTolerance
      minResidualSq = -1. _d 0
      eta_qrNM1     =  1. _d 0
      recip_eta_qrNM1= 1. _d 0

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

#ifndef ALLOW_AUTODIFF_TAMC
      IF (cg2dNormaliseRHS) THEN
C-    Normalise RHS :
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
C-    end Normalise RHS
      ENDIF
#endif /* ndef ALLOW_AUTODIFF_TAMC */

C--   Update overlaps
      CALL EXCH_XY_RL( cg2d_x, myThid )

C--   Initial residual calculation
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_b = comlev1_cg2d, key = ikey, byte = isbyte
CADJ STORE cg2d_x = comlev1_cg2d, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        errTile(bi,bj)    = 0. _d 0
        sumRHStile(bi,bj) = 0. _d 0
        DO j=0,sNy+1
         DO i=0,sNx+1
          cg2d_s(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
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
       ENDDO
      ENDDO

c     CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
      CALL EXCH_XY_RL ( cg2d_r, myThid )
      CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
      actualIts = 0
      IF ( err_sq .NE. 0. ) THEN
        firstResidual = SQRT(err_sq)
      ELSE
        firstResidual = 0.
      ENDIF

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &  ' cg2d: Sum(rhs),rhsMax = ', sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
Cml   begin main solver loop
#if ((defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_LOOP_DIRECTIVE))
CADJ LOOP = iteration, cg2d_x = comlev_cg2d_iter
#endif /* ALLOW_AUTODIFF_TAMC and ALLOW_LOOP_DIRECTIVE */
      DO it2d=1, numIters
#ifdef ALLOW_LOOP_DIRECTIVE
CML      it2d = 0
CML      DO WHILE ( err_sq .GT. cg2dTolerance_sq .and. it2d .LT. numIters )
CML      it2d = it2d+1
#endif /* ALLOW_LOOP_DIRECTIVE */

#ifdef ALLOW_AUTODIFF_TAMC
       icg2dkey = (ikey-1)*numItersMax + it2d
CADJ STORE err_sq = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
      IF ( err_sq .GE. cg2dTolerance_sq ) THEN

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_r = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
CADJ STORE cg2d_s = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         eta_qrNtile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           cg2d_z(i,j,bi,bj) =
     &      pC(i  ,j  ,bi,bj)*cg2d_r(i  ,j  ,bi,bj)
     &     +pW(i  ,j  ,bi,bj)*cg2d_r(i-1,j  ,bi,bj)
     &     +pW(i+1,j  ,bi,bj)*cg2d_r(i+1,j  ,bi,bj)
     &     +pS(i  ,j  ,bi,bj)*cg2d_r(i  ,j-1,bi,bj)
     &     +pS(i  ,j+1,bi,bj)*cg2d_r(i  ,j+1,bi,bj)
           eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &     +cg2d_z(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CMLCADJ STORE eta_qrNM1 = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
CADJ STORE recip_eta_qrNM1 = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
CML       cgBeta   = eta_qrN/eta_qrNM1
       cgBeta   = eta_qrN*recip_eta_qrNM1
Cml    store normalisation factor for the next interation (in case there is one).
CML    store the inverse of the normalization factor for higher precision
CML       eta_qrNM1 = eta_qrN
       recip_eta_qrNM1 = 1. _d 0/eta_qrN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           cg2d_s(i,j,bi,bj) = cg2d_z(i,j,bi,bj)
     &                       + cgBeta*cg2d_s(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--    Do exchanges that require messages i.e. between
C--    processes.
c      CALL EXCH_S3D_RL( cg2d_s, 1, myThid )
       CALL EXCH_XY_RL ( cg2d_s, myThid )

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
#ifdef ALLOW_AUTODIFF_TAMC
#ifndef ALLOW_LOOP_DIRECTIVE
CADJ STORE cg2d_s = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* not ALLOW_LOOP_DIRECTIVE */
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         alphaTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           cg2d_q(i,j,bi,bj) =
     &     aW2d(i  ,j  ,bi,bj)*cg2d_s(i-1,j  ,bi,bj)
     &    +aW2d(i+1,j  ,bi,bj)*cg2d_s(i+1,j  ,bi,bj)
     &    +aS2d(i  ,j  ,bi,bj)*cg2d_s(i  ,j-1,bi,bj)
     &    +aS2d(i  ,j+1,bi,bj)*cg2d_s(i  ,j+1,bi,bj)
     &    +aC2d(i  ,j  ,bi,bj)*cg2d_s(i  ,j  ,bi,bj)
           alphaTile(bi,bj) = alphaTile(bi,bj)
     &                      + cg2d_s(i,j,bi,bj)*cg2d_q(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL( alphaTile, alphaSum, myThid )
       alpha = eta_qrN/alphaSum

C==    Update simultaneously solution and residual vectors (and Iter number)
C      Now compute "interior" points.
#ifdef ALLOW_AUTODIFF_TAMC
#ifndef ALLOW_LOOP_DIRECTIVE
CADJ STORE cg2d_r = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_LOOP_DIRECTIVE */
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         errTile(bi,bj) = 0. _d 0
         DO j=1,sNy
          DO i=1,sNx
           cg2d_x(i,j,bi,bj)=cg2d_x(i,j,bi,bj)+alpha*cg2d_s(i,j,bi,bj)
           cg2d_r(i,j,bi,bj)=cg2d_r(i,j,bi,bj)-alpha*cg2d_q(i,j,bi,bj)
           errTile(bi,bj) = errTile(bi,bj)
     &                    + cg2d_r(i,j,bi,bj)*cg2d_r(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       actualIts = it2d

       CALL GLOBAL_SUM_TILE_RL( errTile,    err_sq,    myThid )
       IF ( printResidual ) THEN
        IF ( MOD( it2d-1, printResidualFreq ).EQ.0 ) THEN
         WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &    ' cg2d: iter=', it2d, ' ; resid.= ', SQRT(err_sq)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDIF

c      CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
       CALL EXCH_XY_RL ( cg2d_r, myThid )

Cml   end of if "err >= cg2dTolerance" block ; end main solver loop
      ENDIF
      ENDDO

#ifndef ALLOW_AUTODIFF_TAMC
      IF (cg2dNormaliseRHS) THEN
C--   Un-normalise the answer
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            cg2d_x(i,j,bi,bj) = cg2d_x(i,j,bi,bj)/rhsNorm
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ndef ALLOW_AUTODIFF_TAMC */

C--   Return parameters to caller
      IF ( err_sq .NE. 0. ) THEN
        lastResidual = SQRT(err_sq)
      ELSE
        lastResidual = 0.
      ENDIF
      numIters = actualIts

#endif /* ALLOW_CG2D_NSA */
      RETURN
      END

#if ((defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_LOOP_DIRECTIVE))

C     These routines are routinely part of the TAMC/TAF library that is
C     not included in the MITcgm, therefore they are mimicked here.

      subroutine adstore(chardum,int1,idow,int2,int3,icount)

      implicit none

#include "SIZE.h"
#include "tamc.h"

      character*(*) chardum
      integer int1, int2, int3, idow, icount

C     the length of this vector must be greater or equal
C     twice the number of timesteps
      integer nidow
#ifdef ALLOW_TAMC_CHECKPOINTING
      parameter ( nidow = 2*nchklev_1*nchklev_2*nchklev_3 )
#else
      parameter ( nidow = 1000000 )
#endif /* ALLOW_TAMC_CHECKPOINTING */
      integer istoreidow(nidow)
      common /istorecommon/ istoreidow

      print *, 'adstore: ', chardum, int1, idow, int2, int3, icount

      if ( icount .gt. nidow ) then
         print *, 'adstore: error: icount > nidow = ', nidow
         stop 'ABNORMAL STOP in adstore'
      endif

      istoreidow(icount) = idow

      return
      end

      subroutine adresto(chardum,int1,idow,int2,int3,icount)

      implicit none

#include "SIZE.h"
#include "tamc.h"

      character*(*) chardum
      integer int1, int2, int3, idow, icount

C     the length of this vector must be greater or equal
C     twice the number of timesteps
      integer nidow
#ifdef ALLOW_TAMC_CHECKPOINTING
      parameter ( nidow = 2*nchklev_1*nchklev_2*nchklev_3 )
#else
      parameter ( nidow = 1000000 )
#endif /* ALLOW_TAMC_CHECKPOINTING */
      integer istoreidow(nidow)
      common /istorecommon/ istoreidow

      print *, 'adresto: ', chardum, int1, idow, int2, int3, icount

      if ( icount .gt. nidow ) then
         print *, 'adstore: error: icount > nidow = ', nidow
         stop 'ABNORMAL STOP in adstore'
      endif

      idow = istoreidow(icount)

      return
      end
#endif /* ALLOW_AUTODIFF_TAMC and ALLOW_LOOP_DIRECTIVE */
