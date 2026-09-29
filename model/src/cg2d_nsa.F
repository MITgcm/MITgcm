#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
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
C     *================================================================*
C     | SUBROUTINE CG2D_NSA
C     | o Two-dimensional grid problem conjugate-gradient inverter
C     |   (with preconditioner).
C     | o This version is used only in the case when the matrix
C     |   operator is Not "Self-Adjoint" (NSA). Any remaining residuals
C     |   will be immediately reported to the National Security Agency
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     cg2d_b    :: The source term or "right hand side" (Output: normalised RHS)
C     cg2d_x    :: The solution (Input: first guess)
C     firstResidual :: the initial residual before any iterations
C     minResidualSq :: the lowest residual reached (squared)
C     lastResidual  :: the actual residual reached
C     numIters  :: Inp: the maximum number of iterations allowed
C                  Out: the actual number of iterations used
C     nIterMin  :: Inp: decide to store (if >=0) or not (if <0) lowest res. sol.
C                  Out: iteration number corresponding to lowest residual
C     myThid    :: my Thread Id number
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
C     bi, bj     :: tile index in X and Y.
C     i, j, it2d :: Loop counters ( it2d counts CG iterations )
C     actualIts  :: actual CG iteration number
C     err_sq     :: Measure of the square of the residual of Ax - b.
C     eta_qrN    :: Used in computing search directions; suffix N and NM1
C     eta_qrNM1     denote current and previous iterations respectively.
C     recip_eta_qrNM1 :: reciprocal of eta_qrNM1
C     cgBeta     :: coeff used to update conjugate direction vector "s".
C     alpha      :: coeff used to update solution & residual
C     alphaSum   :: to avoid the statement: alpha = 1./alpha (for TAF)
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a useful
C                   debugging/trouble shooting diagnostic. For neumann problems
C                   sumRHS needs to be ~0 or it converge at a non-zero residual.
C     msgBuf     :: Informational/error message buffer
C--   local working array (used to be in CG2D.h common block:
C     cg2d_q     :: Intermediate matrix-vector product term
C     cg2d_r     ::   *same*
C     cg2d_s     ::   *same*
C     cg2d_z     :: Intermediate matrix-vector product term
C                :: reduces the number of recomputation in adjoint mode
C                :: this field is superfluous if your cg2d is self-adjoint.
      INTEGER bi, bj
      INTEGER i, j, it2d
      INTEGER actualIts
#ifdef ALLOW_AUTODIFF_TAMC
C     ikey       :: tape key (depends on iteration it2d)
      INTEGER ikey
#else
      _RL    rhsNorm
#endif
      _RL    err_sq,  errTile(nSx,nSy)
      _RL    eta_qrN, eta_qrNtile(nSx,nSy)
      _RL    eta_qrNM1, recip_eta_qrNM1
      _RL    cgBeta,  alpha
      _RL    alphaSum,alphaTile(nSx,nSy)
      _RL    sumRHS,  sumRHStile(nSx,nSy)
      _RL    rhsMax
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
      _RL    cg2d_z(1:sNx,1:sNy,nSx,nSy)
      _RL    cg2d_q(1:sNx,1:sNy,nSx,nSy)
C     Fields cg2d_r and ct2d_s are exchanged and since there are no AD-exchanges
C     for fields with smaller overlap, they are defined with full overlap size
      _RL    cg2d_r(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL    cg2d_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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

C--   Initialise auxiliary constant, some output variable and inverter
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
CADJ STORE cg2d_b = comlev1_cg2d, key = ikey_dynamics, byte = isbyte
CADJ STORE cg2d_x = comlev1_cg2d, key = ikey_dynamics, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C-    Initialise local working arrays:
        DO j=1,sNy
         DO i=1,sNx
          cg2d_q(i,j,bi,bj) = 0. _d 0
          cg2d_z(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          cg2d_r(i,j,bi,bj) = 0. _d 0
          cg2d_s(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        errTile(bi,bj)    = 0. _d 0
        sumRHStile(bi,bj) = 0. _d 0
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
        WRITE(standardMessageUnit,'(A,1P2E22.14)')
     &  ' cg2d_nsa: Sum(rhs),rhsMax = ', sumRHS,rhsMax
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
       ikey = it2d + (ikey_dynamics-1)*numItersMax
CADJ STORE err_sq = comlev1_cg2d_iter, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
       IF ( it2d .LE. cg2dMinItersNSA .OR.
     &      err_sq .GE. cg2dTolerance_sq ) THEN

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_r = comlev1_cg2d_iter, key = ikey, byte = isbyte
CADJ STORE cg2d_s = comlev1_cg2d_iter, key = ikey, byte = isbyte
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
CMLCADJ STORE eta_qrNM1 = comlev1_cg2d_iter, key = ikey, byte = isbyte
CADJ STORE recip_eta_qrNM1 = comlev1_cg2d_iter, key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
CML       cgBeta   = eta_qrN/eta_qrNM1
       cgBeta   = eta_qrN*recip_eta_qrNM1
Cml    store normalisation factor for the next interation (in case there is one).
CML    store the inverse of the normalization factor for higher precision
CML       eta_qrNM1 = eta_qrN
       recip_eta_qrNM1 = 0. _d 0
       IF ( eta_qrN .NE. 0. _d 0 ) recip_eta_qrNM1 = 1. _d 0/eta_qrN

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
CADJ STORE cg2d_s = comlev1_cg2d_iter, key = ikey, byte = isbyte
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
       alpha = 0. _d 0
       IF ( alphaSum .NE. 0 _d 0 ) alpha = eta_qrN/alphaSum

C==    Update simultaneously solution and residual vectors (and Iter number)
C      Now compute "interior" points.
#ifdef ALLOW_AUTODIFF_TAMC
#ifndef ALLOW_LOOP_DIRECTIVE
CADJ STORE cg2d_r = comlev1_cg2d_iter, key = ikey, byte = isbyte
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
     &    ' cg2d_nsa: iter=', it2d, ' ; resid.= ', SQRT(err_sq)
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

C     These routines are routinely part of the TAF library that is
C     not included in the MITcgm, therefore they are mimicked here.

      SUBROUTINE ADSTORE(chardum,int1,idow,int2,int3,icount)

      IMPLICIT NONE

#include "SIZE.h"
#include "tamc.h"

      CHARACTER*(*) chardum
      INTEGER int1, int2, int3, idow, icount

C     the length of this vector must be greater or equal
C     twice the number of timesteps
      INTEGER nidow
#ifdef ALLOW_TAMC_CHECKPOINTING
      PARAMETER ( nidow = 2*nchklev_1*nchklev_2*nchklev_3 )
#else
      PARAMETER ( nidow = 1000000 )
#endif /* ALLOW_TAMC_CHECKPOINTING */
      INTEGER istoreidow(nidow)
      COMMON /istorecommon/ istoreidow

      PRINT *, 'ADSTORE: ', chardum, int1, idow, int2, int3, icount

      IF ( icount .GT. nidow ) THEN
         PRINT *, 'adstore: error: icount > nidow = ', nidow
         STOP 'ABNORMAL STOP in ADSTORE'
      ENDIF

      istoreidow(icount) = idow

      RETURN
      END

      SUBROUTINE ADRESTO(chardum,int1,idow,int2,int3,icount)

      IMPLICIT NONE

#include "SIZE.h"
#include "tamc.h"

      CHARACTER*(*) chardum
      INTEGER int1, int2, int3, idow, icount

C     the length of this vector must be greater or equal
C     twice the number of timesteps
      INTEGER nidow
#ifdef ALLOW_TAMC_CHECKPOINTING
      PARAMETER ( nidow = 2*nchklev_1*nchklev_2*nchklev_3 )
#else
      PARAMETER ( nidow = 1000000 )
#endif /* ALLOW_TAMC_CHECKPOINTING */
      INTEGER istoreidow(nidow)
      COMMON /istorecommon/ istoreidow

      PRINT *, 'ADRESTO: ', chardum, int1, idow, int2, int3, icount

      IF ( icount .GT. nidow ) THEN
         PRINT *, 'ADRESTO: error: icount > nidow = ', nidow
         STOP 'ABNORMAL STOP in ADRESTO'
      ENDIF

      idow = istoreidow(icount)

      RETURN
      END
#endif /* ALLOW_AUTODIFF_TAMC and ALLOW_LOOP_DIRECTIVE */
