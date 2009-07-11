C $Header: /u/gcmpack/MITgcm/model/src/cg2d_nsa.F,v 1.3 2009/07/11 21:45:44 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#ifdef ALLOW_USE_MPI
C HACK to avoid global_max
# define ALLOW_CONST_RHSMAX
#endif

CML THIS DOES NOT WORK +++++
#undef ALLOW_LOOP_DIRECTIVE
CBOP
C     !ROUTINE: CG2D_NSA
C     !INTERFACE:
      SUBROUTINE CG2D_NSA(
     I                cg2d_b,
     U                cg2d_x,
     O                firstResidual,
     O                lastResidual,
     U                numIters,
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
c#include "GRID.h"
c#include "SURFACE.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

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

#ifdef ALLOW_CG2D_NSA
C     !LOCAL VARIABLES:
C     === Local variables ====
C     actualIts      :: Number of iterations taken
C     actualResidual :: residual
C     bi, bj     :: Block index in X and Y.
C     eta_qrN    :: Used in computing search directions
C     eta_qrNM1     suffix N and NM1 denote current and
C     cgBeta        previous iterations respectively.
C     recip_eta_qrNM1 :: reciprocal of eta_qrNM1
C     alpha
C     alpha_aux  :: to avoid the statement: alpha = 1./alpha (for TAMC/TAF)
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a
C                   useful debuggin/trouble shooting diagnostic.
C                   For neumann problems sumRHS needs to be ~0.
C                   or they converge at a non-zero residual.
C     err        :: Measure of residual of Ax - b, usually the norm.
C     err_sq     :: square of err (for TAMC/TAF)
C     I, J, it2d :: Loop counters ( it2d counts CG iterations )
      INTEGER actualIts
      _RL    actualResidual
      INTEGER bi, bj
      INTEGER I, J, it2d

      _RL    err
      _RL    err_sq
      _RL    eta_qrN
      _RL    eta_qrNM1
      _RL    recip_eta_qrNM1
      _RL    cgBeta
      _RL    alpha
      _RL    alpha_aux
      _RL    sumRHS
      _RL    rhsMax, rhsMaxGlobal
      _RL    rhsNorm
      _RL    cg2dTolerance_sq
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      IF ( numIters .GT. numItersMax ) THEN
         WRITE(standardMessageUnit,'(A,I10)')
     &        'CG2D_NSA: numIters > numItersMax = ', numItersMax
         STOP 'NON-NORMAL in CG2D_NSA'
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

CcnhDebugStarts
C     CHARACTER*(MAX_LEN_FNAM) suff
CcnhDebugEnds

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = myThid - 1
          max1 = nTx*nTy
          act2 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Initialise inverter
      eta_qrNM1 = 1. _d 0
      recip_eta_qrNM1 = 1./eta_qrNM1

CcnhDebugStarts
C     _EXCH_XY_RL( cg2d_b, myThid )
C     CALL PLOT_FIELD_XYRL( cg2d_b, 'CG2D.0 CG2D_B' , 1, myThid )
C     suff = 'unnormalised'
C     CALL WRITE_FLD_XY_RL (  'cg2d_b.',suff,    cg2d_b, 1, myThid)
C     STOP
CcnhDebugEnds

C--   Normalise RHS
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_b = comlev1_cg2d, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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
C     -  Normalise RHS :
#ifdef LETS_MAKE_JAM
C     _GLOBAL_MAX_RL( rhsMax, myThid )
      rhsMaxGlobal=1.
#else
#ifdef ALLOW_CONST_RHSMAX
      rhsMaxGlobal=1.
#else
      rhsMaxGlobal=rhsMax
      _GLOBAL_MAX_RL( rhsMaxGlobal, myThid )
#endif /* ALLOW_CONST_RHSMAX */
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhsNorm = comlev1_cg2d, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
      IF ( rhsMaxGlobal .NE. 0. ) THEN
       rhsNorm = 1. _d 0 / rhsMaxGlobal
      ELSE
       rhsNorm = 1. _d 0
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_b = comlev1_cg2d, key = ikey, byte = isbyte
CADJ STORE cg2d_x = comlev1_cg2d, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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
      err = 0. _d 0
      err_sq = 0. _d 0
      sumRHS = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_b = comlev1_cg2d, key = ikey, byte = isbyte
CADJ STORE cg2d_x = comlev1_cg2d, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-1,sNy+1
         DO I=1-1,sNx+1
          cg2d_s(I,J,bi,bj) = 0.
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
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
c    &    -freeSurfFac*_rA(i,j,bi,bj)*recip_Bo(i,j,bi,bj)*
c    &     cg2d_x(I  ,J  ,bi,bj)/deltaTMom/deltaTfreesurf*cg2dNorm
cML     &     cg2d_x(I  ,J  ,bi,bj)/deltaTMom/deltaTMom*cg2dNorm
c    &    )
          err_sq         = err_sq         +
     &     cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          sumRHS            = sumRHS            +
     &     cg2d_b(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c     CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
      CALL EXCH_XY_RL ( cg2d_r, myThid )
      _GLOBAL_SUM_RL( sumRHS, myThid )
      _GLOBAL_SUM_RL( err_sq, myThid )
       IF ( err_sq .NE. 0. ) THEN
          err = SQRT(err_sq)
       ELSE
          err = 0.
       ENDIF
       actualIts      = 0
       actualResidual = err

      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &  ' cg2d: Sum(rhs),rhsMax = ', sumRHS,rhsMaxGlobal
        _END_MASTER( myThid )
      ENDIF
C     _BARRIER
c     _BEGIN_MASTER( myThid )
c      WRITE(*,'(A,I6,1PE30.14)') ' CG2D_NSA iters, err = ',
c    & actualIts, actualResidual
c     _END_MASTER( myThid )
      firstResidual=actualResidual
      cg2dTolerance_sq = cg2dTolerance**2

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
CMLCADJ STORE err = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
CADJ STORE err_sq = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
CML      IF ( err .LT. cg2dTolerance ) THEN
      IF ( err_sq .LT. cg2dTolerance_sq ) THEN
Cml      DO NOTHING
      ELSE

CcnhDebugStarts
C      WRITE(*,*) ' CG2D_NSA: Iteration ',it2d-1,' residual = ',
C    &  actualResidual
CcnhDebugEnds
C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
       eta_qrN = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE cg2d_r = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
CADJ STORE cg2d_s = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_z(I,J,bi,bj) =
     &      pC(I  ,J  ,bi,bj)*cg2d_r(I  ,J  ,bi,bj)
     &     +pW(I  ,J  ,bi,bj)*cg2d_r(I-1,J  ,bi,bj)
     &     +pW(I+1,J  ,bi,bj)*cg2d_r(I+1,J  ,bi,bj)
     &     +pS(I  ,J  ,bi,bj)*cg2d_r(I  ,J-1,bi,bj)
     &     +pS(I  ,J+1,bi,bj)*cg2d_r(I  ,J+1,bi,bj)
CcnhDebugStarts
C          cg2d_z(I,J,bi,bj) = cg2d_r(I  ,J  ,bi,bj)
CcnhDebugEnds
           eta_qrN = eta_qrN
     &     +cg2d_z(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_RL(eta_qrN, myThid)
CcnhDebugStarts
C      WRITE(*,*) ' CG2D_NSA: Iteration ',it2d-1,' eta_qrN = ',eta_qrN
CcnhDebugEnds
#ifdef ALLOW_AUTODIFF_TAMC
CMLCADJ STORE eta_qrNM1 = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
CADJ STORE recip_eta_qrNM1 = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
CML       cgBeta   = eta_qrN/eta_qrNM1
       cgBeta   = eta_qrN*recip_eta_qrNM1
CcnhDebugStarts
C      WRITE(*,*) ' CG2D_NSA: Iteration ',it2d-1,' beta = ',cgBeta
CcnhDebugEnds
Cml    store normalisation factor for the next interation
Cml    (in case there is one).
CML    store the inverse of the normalization factor for higher precision
CML       eta_qrNM1 = eta_qrN
       recip_eta_qrNM1 = 1./eta_qrN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_s(I,J,bi,bj) = cg2d_z(I,J,bi,bj)
     &                       + cgBeta*cg2d_s(I,J,bi,bj)
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
       alpha     = 0. _d 0
       alpha_aux = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
#ifndef ALLOW_LOOP_DIRECTIVE
CADJ STORE cg2d_s = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* not ALLOW_LOOP_DIRECTIVE */
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
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
c    &    -freeSurfFac*_rA(i,j,bi,bj)*recip_Bo(i,j,bi,bj)*
c    &     cg2d_s(I  ,J  ,bi,bj)/deltaTMom/deltaTfreesurf*cg2dNorm
cML     &     cg2d_s(I  ,J  ,bi,bj)/deltaTMom/deltaTMom*cg2dNorm
           alpha_aux = alpha_aux+cg2d_s(I,J,bi,bj)*cg2d_q(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_SUM_RL(alpha_aux,myThid)
CcnhDebugStarts
C      WRITE(*,*) ' CG2D_NSA: Iteration ',it2d-1,' SUM(s*q)= ',alpha_aux
CcnhDebugEnds
       alpha = eta_qrN/alpha_aux
CcnhDebugStarts
C      WRITE(*,*) ' CG2D_NSA: Iteration ',it2d-1,' alpha= ',alpha
CcnhDebugEnds

C==    Update solution and residual vectors
C      Now compute "interior" points.
       err    = 0. _d 0
       err_sq = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
#ifndef ALLOW_LOOP_DIRECTIVE
CADJ STORE cg2d_r = comlev1_cg2d_iter, key = icg2dkey, byte = isbyte
#endif /* ALLOW_LOOP_DIRECTIVE */
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_x(I,J,bi,bj)=cg2d_x(I,J,bi,bj)+alpha*cg2d_s(I,J,bi,bj)
           cg2d_r(I,J,bi,bj)=cg2d_r(I,J,bi,bj)-alpha*cg2d_q(I,J,bi,bj)
           err_sq = err_sq+cg2d_r(I,J,bi,bj)*cg2d_r(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_RL( err_sq   , myThid )
       if ( err_sq .ne. 0. ) then
          err = SQRT(err_sq)
       else
          err = 0.
       end if
       actualIts      = it2d
       actualResidual = err

c      CALL EXCH_S3D_RL( cg2d_r, 1, myThid )
       CALL EXCH_XY_RL ( cg2d_r, myThid )

Cml   end of IF ( err .LT. cg2dTolerance ) THEN; ELSE
      ENDIF
Cml     end main solver loop
      ENDDO

      IF (cg2dNormaliseRHS) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhsNorm = comlev1_cg2d, key = ikey, byte = isbyte
CADJ STORE cg2d_x = comlev1_cg2d, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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
c     _BEGIN_MASTER( myThid )
c      WRITE(*,'(A,I6,1PE30.14)') ' CG2D_NSA iters, err = ',
c    & actualIts, actualResidual
c     _END_MASTER( )

C--   Return parameters to caller
      lastResidual=actualResidual
      numIters=actualIts

#endif /* ALLOW_CG2D_NSA */
      RETURN
      END

# if ((defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_LOOP_DIRECTIVE))
C
C     These routines are routinely part of the TAMC/TAF library that is
C     not included in the MITcgm, therefore they are mimicked here.
C
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
# endif /* ALLOW_AUTODIFF_TAMC and ALLOW_LOOP_DIRECTIVE */

