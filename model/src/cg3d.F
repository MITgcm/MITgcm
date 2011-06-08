C $Header: /u/gcmpack/MITgcm/model/src/cg3d.F,v 1.24 2011/06/08 01:46:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#ifdef TARGET_NEC_SX
C     set a sensible default for the outer loop unrolling parameter that can
C     be overriden in the Makefile with the DEFINES macro or in CPP_OPTIONS.h
#ifndef CG3D_OUTERLOOPITERS
# define CG3D_OUTERLOOPITERS 10
#endif
#endif /* TARGET_NEC_SX */

CBOP
C     !ROUTINE: CG3D
C     !INTERFACE:
      SUBROUTINE CG3D(
     I                cg3d_b,
     U                cg3d_x,
     O                firstResidual,
     O                lastResidual,
     U                numIters,
     I                myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CG3D
C     | o Three-dimensional grid problem conjugate-gradient
C     |   inverter (with preconditioner).
C     *==========================================================*
C     | Con. grad is an iterative procedure for solving Ax = b.
C     | It requires the A be symmetric.
C     | This implementation assumes A is a seven-diagonal
C     | matrix of the form that arises in the discrete
C     | representation of the del^2 operator in a
C     | three-dimensional space.
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
#include "GRID.h"
#include "SURFACE.h"
#include "CG3D.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     cg3d_b    :: The source term or "right hand side"
C     cg3d_x    :: The solution
C     firstResidual :: the initial residual before any iterations
C     lastResidual  :: the actual residual reached
C     numIters  :: Entry: the maximum number of iterations allowed
C               :: Exit:  the actual number of iterations used
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL     cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     cg3d_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     firstResidual
      _RL     lastResidual
      INTEGER numIters
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC

C     !LOCAL VARIABLES:
C     === Local variables ====
C     actualIts      :: Number of iterations taken
C     actualResidual :: residual
C     bi,bj      :: tile indices
C     eta_qrN    :: Used in computing search directions
C     eta_qrNM1     suffix N and NM1 denote current and
C     cgBeta        previous iterations respectively.
C     alpha
C     sumRHS     :: Sum of right-hand-side. Sometimes this is a
C                   useful debuggin/trouble shooting diagnostic.
C                   For neumann problems sumRHS needs to be ~0.
C                   or they converge at a non-zero residual.
C     err        :: Measure of residual of Ax - b, usually the norm.
C     i, j, k    :: Loop counters
C     it3d       :: Loop counter for CG iterations
C     msgBuf     :: Informational/error message buffer
      INTEGER actualIts
      _RL     actualResidual
      INTEGER bi, bj
      INTEGER i, j, k, it3d
      INTEGER km1, kp1
      _RL     maskM1, maskP1
      _RL     err,    errTile(nSx,nSy)
      _RL     eta_qrN,eta_qrNtile(nSx,nSy)
      _RL     eta_qrNM1
      _RL     cgBeta
      _RL     alpha , alphaTile(nSx,nSy)
      _RL     sumRHS, sumRHStile(nSx,nSy)
      _RL     rhsMax
      _RL     rhsNorm
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
      _RL     surfFac
#ifdef NONLIN_FRSURF
      INTEGER ks
      _RL     surfTerm(sNx,sNy)
#endif /* NONLIN_FRSURF */
CEOP

      IF ( select_rStar .NE. 0 ) THEN
        surfFac = freeSurfFac
      ELSE
        surfFac = 0.
      ENDIF
#ifdef NONLIN_FRSURF
      DO j=1,sNy
       DO i=1,sNx
         surfTerm(i,j) = 0.
       ENDDO
      ENDDO
#endif /* NONLIN_FRSURF */

C--   Initialise inverter
      eta_qrNM1 = 1. _d 0

C--   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)*cg3dNorm
     &                          * maskC(i,j,k,bi,bj)
           rhsMax = MAX(ABS(cg3d_b(i,j,k,bi,bj)),rhsMax)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_MAX_RL( rhsMax, myThid )
      rhsNorm = 1. _d 0
      IF ( rhsMax .NE. 0. ) rhsNorm = 1. _d 0 / rhsMax
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)*rhsNorm
           cg3d_x(i,j,k,bi,bj) = cg3d_x(i,j,k,bi,bj)*rhsNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlaps
c     _EXCH_XYZ_RL( cg3d_b, myThid )
      _EXCH_XYZ_RL( cg3d_x, myThid )

C--   Initial residual calculation (with free-Surface term)
      err    = 0. _d 0
      sumRHS = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        errTile(bi,bj)    = 0. _d 0
        sumRHStile(bi,bj) = 0. _d 0
#ifdef NONLIN_FRSURF
        IF ( select_rStar .NE. 0 ) THEN
          DO j=1,sNy
           DO i=1,sNx
             surfTerm(i,j) = 0.
           ENDDO
          ENDDO
          DO k=1,Nr
           DO j=1,sNy
            DO i=1,sNx
             surfTerm(i,j) = surfTerm(i,j)
     &         +cg3d_x(i,j,k,bi,bj)*drF(k)*h0FacC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
          DO j=1,sNy
           DO i=1,sNx
             ks = kSurfC(i,j,bi,bj)
             surfTerm(i,j) = surfTerm(i,j)*cg3dNorm
     &              *recip_Rcol(i,j,bi,bj)*recip_Rcol(i,j,bi,bj)
     &              *rA(i,j,bi,bj)*deepFac2F(ks)
     &              *recip_Bo(i,j,bi,bj)/deltaTMom/deltaTfreesurf
           ENDDO
          ENDDO
        ENDIF
#endif /* NONLIN_FRSURF */
        DO k=1,Nr
         km1 = MAX(k-1, 1 )
         kp1 = MIN(k+1, Nr)
         maskM1 = 1. _d 0
         maskP1 = 1. _d 0
         IF ( k .EQ. 1 ) maskM1 = 0. _d 0
         IF ( k .EQ. Nr) maskP1 = 0. _d 0
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
         DO j=1,sNy
          DO i=1,sNx
           cg3d_r(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &     -( 0.
     &       +aW3d( i, j, k, bi,bj)*cg3d_x(i-1,j, k, bi,bj)
     &       +aW3d(i+1,j, k, bi,bj)*cg3d_x(i+1,j, k, bi,bj)
     &       +aS3d( i, j, k, bi,bj)*cg3d_x( i,j-1,k, bi,bj)
     &       +aS3d( i,j+1,k, bi,bj)*cg3d_x( i,j+1,k, bi,bj)
     &       +aV3d( i, j, k, bi,bj)*cg3d_x( i, j,km1,bi,bj)*maskM1
     &       +aV3d( i, j,kp1,bi,bj)*cg3d_x( i, j,kp1,bi,bj)*maskP1
     &       +aC3d( i, j, k, bi,bj)*cg3d_x( i, j, k, bi,bj)
#ifdef NONLIN_FRSURF
     &       -surfFac*surfTerm(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
#endif /* NONLIN_FRSURF */
     &      )
           errTile(bi,bj) = errTile(bi,bj)
     &                    +cg3d_r(i,j,k,bi,bj)*cg3d_r(i,j,k,bi,bj)
           sumRHStile(bi,bj) = sumRHStile(bi,bj)+cg3d_b(i,j,k,bi,bj)
          ENDDO
         ENDDO
         DO J=1-1,sNy+1
          DO I=1-1,sNx+1
           cg3d_s(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
       CALL EXCH_S3D_RL( cg3d_r, Nr, myThid )
c      CALL EXCH_S3D_RL( cg3d_s, Nr, myThid )
       CALL GLOBAL_SUM_TILE_RL( sumRHStile, sumRHS, myThid )
       CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
      IF ( debugLevel.GE.debLevC .AND. diagFreq.GT.0. ) THEN
        CALL WRITE_FLD_S3D_RL(
     I        'cg3d_r_I', 'I10', 1, Nr, cg3d_r, myIter, myThid )
      ENDIF

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &     ' cg3d: Sum(rhs),rhsMax = ',sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF

      actualIts      = 0
      actualResidual = SQRT(err)
      firstResidual=actualResidual

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 10 it3d=1, numIters

       IF ( actualResidual .LT. cg3dTargetResidual ) GOTO 11
C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
C      Note. On the next two loops over all tiles the inner loop ranges
C            in sNx and sNy are expanded by 1 to avoid a communication
C            step. However this entails a bit of gynamastics because we only
C            want eta_qrN for the interior points.
       eta_qrN = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         eta_qrNtile(bi,bj) = 0. _d 0
         DO k=1,1
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1-1,sNy+1
           DO i=1-1,sNx+1
            cg3d_q(i,j,k,bi,bj) = zMC(i,j,k,bi,bj)
     &                        *cg3d_r(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO k=2,Nr
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1-1,sNy+1
           DO i=1-1,sNx+1
            cg3d_q(i,j,k,bi,bj) = zMC(i,j,k,bi,bj)
     &                      *( cg3d_r(i,j,k,bi,bj)
     &                         -zML(i,j,k,bi,bj)*cg3d_q(i,j,k-1,bi,bj)
     &                       )
           ENDDO
          ENDDO
         ENDDO
         DO k=Nr,Nr
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &                        +cg3d_q(i,j,k,bi,bj)*cg3d_r(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO k=Nr-1,1,-1
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1-1,sNy+1
           DO i=1-1,sNx+1
            cg3d_q(i,j,k,bi,bj) = cg3d_q(i,j,k,bi,bj)
     &                         -zMU(i,j,k,bi,bj)*cg3d_q(i,j,k+1,bi,bj)
           ENDDO
          ENDDO
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            eta_qrNtile(bi,bj) = eta_qrNtile(bi,bj)
     &                        +cg3d_q(i,j,k,bi,bj)*cg3d_r(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( eta_qrNtile,eta_qrN,myThid )
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' eta_qrN = ',eta_qrN
CcnhDebugEnds
       cgBeta   = eta_qrN/eta_qrNM1
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' beta = ',cgBeta
CcnhDebugEnds
       eta_qrNM1 = eta_qrN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1-1,sNy+1
           DO i=1-1,sNx+1
            cg3d_s(i,j,k,bi,bj) = cg3d_q(i,j,k,bi,bj)
     &                   + cgBeta*cg3d_s(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
       alpha = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         alphaTile(bi,bj) = 0. _d 0
#ifdef NONLIN_FRSURF
         IF ( select_rStar .NE. 0 ) THEN
          DO j=1,sNy
           DO i=1,sNx
             surfTerm(i,j) = 0.
           ENDDO
          ENDDO
          DO k=1,Nr
           DO j=1,sNy
            DO i=1,sNx
             surfTerm(i,j) = surfTerm(i,j)
     &         +cg3d_s(i,j,k,bi,bj)*drF(k)*h0FacC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
          DO j=1,sNy
           DO i=1,sNx
             ks = kSurfC(i,j,bi,bj)
             surfTerm(i,j) = surfTerm(i,j)*cg3dNorm
     &              *recip_Rcol(i,j,bi,bj)*recip_Rcol(i,j,bi,bj)
     &              *rA(i,j,bi,bj)*deepFac2F(ks)
     &              *recip_Bo(i,j,bi,bj)/deltaTMom/deltaTfreesurf
           ENDDO
          ENDDO
         ENDIF
#endif /* NONLIN_FRSURF */
         IF ( Nr .GT. 1 ) THEN
          k=1
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            cg3d_q(i,j,k,bi,bj) =
     &        aW3d( i, j, k, bi,bj)*cg3d_s(i-1,j, k, bi,bj)
     &       +aW3d(i+1,j, k, bi,bj)*cg3d_s(i+1,j, k, bi,bj)
     &       +aS3d( i, j, k, bi,bj)*cg3d_s( i,j-1,k, bi,bj)
     &       +aS3d( i,j+1,k, bi,bj)*cg3d_s( i,j+1,k, bi,bj)
     &       +aV3d( i, j,k+1,bi,bj)*cg3d_s( i, j,k+1,bi,bj)
     &       +aC3d( i, j, k, bi,bj)*cg3d_s( i, j, k, bi,bj)
#ifdef NONLIN_FRSURF
     &       -surfFac*surfTerm(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
#endif /* NONLIN_FRSURF */
            alphaTile(bi,bj) = alphaTile(bi,bj)
     &             +cg3d_s(i,j,k,bi,bj)*cg3d_q(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ELSE
          k=1
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            cg3d_q(i,j,k,bi,bj) =
     &        aW3d( i, j, k, bi,bj)*cg3d_s(i-1,j, k, bi,bj)
     &       +aW3d(i+1,j, k, bi,bj)*cg3d_s(i+1,j, k, bi,bj)
     &       +aS3d( i, j, k, bi,bj)*cg3d_s( i,j-1,k, bi,bj)
     &       +aS3d( i,j+1,k, bi,bj)*cg3d_s( i,j+1,k, bi,bj)
     &       +aC3d( i, j, k, bi,bj)*cg3d_s( i, j, k, bi,bj)
#ifdef NONLIN_FRSURF
     &       -surfFac*surfTerm(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
#endif /* NONLIN_FRSURF */
            alphaTile(bi,bj) = alphaTile(bi,bj)
     &             +cg3d_s(i,j,k,bi,bj)*cg3d_q(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDIF
         DO k=2,Nr-1
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            cg3d_q(i,j,k,bi,bj) =
     &        aW3d( i, j, k, bi,bj)*cg3d_s(i-1,j, k, bi,bj)
     &       +aW3d(i+1,j, k, bi,bj)*cg3d_s(i+1,j, k, bi,bj)
     &       +aS3d( i, j, k, bi,bj)*cg3d_s( i,j-1,k, bi,bj)
     &       +aS3d( i,j+1,k, bi,bj)*cg3d_s( i,j+1,k, bi,bj)
     &       +aV3d( i, j, k, bi,bj)*cg3d_s( i, j,k-1,bi,bj)
     &       +aV3d( i, j,k+1,bi,bj)*cg3d_s( i, j,k+1,bi,bj)
     &       +aC3d( i, j, k, bi,bj)*cg3d_s( i, j, k, bi,bj)
#ifdef NONLIN_FRSURF
     &       -surfFac*surfTerm(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
#endif /* NONLIN_FRSURF */
            alphaTile(bi,bj) = alphaTile(bi,bj)
     &             +cg3d_s(i,j,k,bi,bj)*cg3d_q(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         IF ( Nr .GT. 1 ) THEN
          k=Nr
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            cg3d_q(i,j,k,bi,bj) =
     &        aW3d( i, j, k, bi,bj)*cg3d_s(i-1,j, k, bi,bj)
     &       +aW3d(i+1,j, k, bi,bj)*cg3d_s(i+1,j, k, bi,bj)
     &       +aS3d( i, j, k, bi,bj)*cg3d_s( i,j-1,k, bi,bj)
     &       +aS3d( i,j+1,k, bi,bj)*cg3d_s( i,j+1,k, bi,bj)
     &       +aV3d( i, j, k, bi,bj)*cg3d_s( i, j,k-1,bi,bj)
     &       +aC3d( i, j, k, bi,bj)*cg3d_s( i, j, k, bi,bj)
#ifdef NONLIN_FRSURF
     &       -surfFac*surfTerm(i,j)*drF(k)*h0FacC(i,j,k,bi,bj)
#endif /* NONLIN_FRSURF */
            alphaTile(bi,bj) = alphaTile(bi,bj)
     &             +cg3d_s(i,j,k,bi,bj)*cg3d_q(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL( alphaTile,  alpha,  myThid )
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' SUM(s*q)= ',alpha
CcnhDebugEnds
       alpha = eta_qrN/alpha
CcnhDebugStarts
C      WRITE(*,*) ' CG3D: Iteration ',it3d-1,' alpha= ',alpha
CcnhDebugEnds

C==    Update solution and residual vectors
C      Now compute "interior" points.
       err = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         errTile(bi,bj) = 0. _d 0
         DO k=1,Nr
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=1,sNy
           DO i=1,sNx
            cg3d_x(i,j,k,bi,bj)=cg3d_x(i,j,k,bi,bj)
     &            +alpha*cg3d_s(i,j,k,bi,bj)
            cg3d_r(i,j,k,bi,bj)=cg3d_r(i,j,k,bi,bj)
     &            -alpha*cg3d_q(i,j,k,bi,bj)
            errTile(bi,bj) = errTile(bi,bj)
     &            +cg3d_r(i,j,k,bi,bj)*cg3d_r(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( errTile,    err,    myThid )
       err = SQRT(err)
       actualIts      = it3d
       actualResidual = err
       IF ( printResidual ) THEN
        IF ( MOD( it3d-1, printResidualFreq ).EQ.0 ) THEN
         WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &    ' cg3d: iter=', actualIts, ' ; resid.= ', actualResidual
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDIF
       IF ( actualResidual .LT. cg3dTargetResidual ) GOTO 11
       CALL EXCH_S3D_RL( cg3d_r, Nr, myThid )

   10 CONTINUE
   11 CONTINUE

      IF ( debugLevel.GE.debLevC .AND. diagFreq.GT.0. ) THEN
        CALL WRITE_FLD_S3D_RL(
     I        'cg3d_r_F', 'I10', 1, Nr, cg3d_r, myIter, myThid )
      ENDIF

C--   Un-normalise the answer
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           cg3d_x(i,j,k,bi,bj) = cg3d_x(i,j,k,bi,bj)/rhsNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      lastResidual = actualResidual
      numIters = actualIts

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
