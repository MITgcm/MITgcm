C $Header: /u/gcmpack/MITgcm/model/src/cg3d_ex0.F,v 1.1 2012/05/14 13:21:59 jmc Exp $
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
C     !ROUTINE: CG3D_EX0
C     !INTERFACE:
      SUBROUTINE CG3D_EX0(
     U                cg3d_b, cg3d_x,
     O                firstResidual, lastResidual,
     U                numIters,
     I                myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CG3D_EX0
C     | o Three-dimensional grid problem conjugate-gradient
C     |   inverter (with preconditioner).
C     | This is the disconnected-tile version (each tile treated
C     |   independently as a small domain, with locally periodic
C     |   BC at the edges.
C     *==========================================================*
C     | Con. grad is an iterative procedure for solving Ax = b.
C     | It requires the A be symmetric.
C     | This implementation assumes A is a seven-diagonal matrix
C     | of the form that arises in the discrete representation of
C     | the del^2 operator in a three-dimensional space.
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
C     cg3d_b    :: The source term or "right hand side" (output: normalised RHS)
C     cg3d_x    :: The solution (input: first guess)
C     firstResidual :: the initial residual before any iterations
C     minResidualSq :: the lowest residual reached (squared)
CC    lastResidual  :: the actual residual reached
C     numIters  :: Inp: the maximum number of iterations allowed
C                  Out: the actual number of iterations used
CC    nIterMin  :: Inp: decide to store (if >=0) or not (if <0) lowest res. sol.
CC                 Out: iteration number corresponding to lowest residual
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
C     bi, bj     :: tile index in X and Y.
C     i, j, k    :: Loop counters
C     it3d       :: Loop counter for CG iterations
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
      INTEGER bi, bj
      INTEGER i, j, k, it3d
      INTEGER actualIts(nSx,nSy)
      INTEGER km1, kp1
      _RL     maskM1, maskP1
      _RL     cg3dTolerance_sq
      _RL     err_sq,  errTile(nSx,nSy)
      _RL     eta_qrNtile(nSx,nSy)
      _RL     eta_qrNM1(nSx,nSy)
      _RL     cgBeta
      _RL     alpha ,  alphaTile(nSx,nSy)
      _RL     sumRHS,  sumRHStile(nSx,nSy)
      _RL     rhsMax, rhsMaxLoc
      _RL     rhsNorm(nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL printResidual
      _RL     surfFac
#ifdef NONLIN_FRSURF
      INTEGER ks
      _RL     surfTerm(sNx,sNy)
#endif /* NONLIN_FRSURF */
CEOP

C--   Initialise auxiliary constant, some output variable
      cg3dTolerance_sq = cg3dTargetResidual*cg3dTargetResidual
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

C--   Initialise inverter and   Normalise RHS
      rhsMax = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        actualIts(bi,bj) = 0
        eta_qrNM1(bi,bj) =  1. _d 0
        rhsMaxLoc = 0. _d 0
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)*cg3dNorm
     &                          * maskC(i,j,k,bi,bj)
           rhsMaxLoc = MAX(ABS(cg3d_b(i,j,k,bi,bj)),rhsMaxLoc)
          ENDDO
         ENDDO
        ENDDO
        rhsNorm(bi,bj) = 1. _d 0
        IF ( rhsMaxLoc .NE. 0. ) rhsNorm(bi,bj) = 1. _d 0 / rhsMaxLoc
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)*rhsNorm(bi,bj)
           cg3d_x(i,j,k,bi,bj) = cg3d_x(i,j,k,bi,bj)*rhsNorm(bi,bj)
          ENDDO
         ENDDO
        ENDDO
        rhsMax = MAX( rhsMaxLoc, rhsMax )

       ENDDO
      ENDDO
      _GLOBAL_MAX_RL( rhsMax, myThid )

C--   Update overlaps
      _EXCH_XYZ_RL( cg3d_x, myThid )

C--   Initial residual calculation (with free-Surface term)
      err_sq = 0.
      sumRHS = 0.
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
         DO j=0,sNy+1
          DO i=0,sNx+1
           cg3d_s(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
        err_sq = MAX( errTile(bi,bj), err_sq )
        sumRHS = MAX( ABS(sumRHStile(bi,bj)), sumRHS )
       ENDDO
      ENDDO
       CALL EXCH_S3D_RL( cg3d_r, Nr, myThid )
      _GLOBAL_MAX_RL( err_sq, myThid )
      _GLOBAL_MAX_RL( sumRHS, myThid )
      IF ( debugLevel.GE.debLevC .AND. diagFreq.GT.0. ) THEN
        CALL WRITE_FLD_S3D_RL(
     I        'cg3d_r_I', 'I10', 1, Nr, cg3d_r, myIter, myThid )
      ENDIF
      firstResidual = SQRT(err_sq)

      printResidual = .FALSE.
      IF ( debugLevel .GE. debLevZero ) THEN
        _BEGIN_MASTER( myThid )
        printResidual = printResidualFreq.GE.1
        WRITE(standardmessageunit,'(A,1P2E22.14)')
     &     ' cg3d: Sum(rhs),rhsMax = ',sumRHS,rhsMax
        _END_MASTER( myThid )
      ENDIF

c     IF ( err_sq .LT. cg3dTolerance_sq ) GOTO 11

C     >>>>>>>>>>>>>>> BEGIN SOLVER <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO it3d=1, numIters
      IF ( err_sq.GE.cg3dTolerance_sq ) THEN
       err_sq = 0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( errTile(bi,bj).GE.cg3dTolerance_sq ) THEN

C--    Solve preconditioning equation and update
C--    conjugate direction vector "s".
C      Note. On the next two loops over all tiles the inner loop ranges
C            in sNx and sNy are expanded by 1 to avoid a communication
C            step. However this entails a bit of gynamastics because we only
C            want eta_qrN for the interior points.
         eta_qrNtile(bi,bj) = 0. _d 0
         DO k=1,1
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=0,sNy+1
           DO i=0,sNx+1
            cg3d_q(i,j,k,bi,bj) = zMC(i,j,k,bi,bj)
     &                        *cg3d_r(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO k=2,Nr
#ifdef TARGET_NEC_SX
!CDIR OUTERUNROLL=CG3D_OUTERLOOPITERS
#endif /* TARGET_NEC_SX */
          DO j=0,sNy+1
           DO i=0,sNx+1
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
          DO j=0,sNy+1
           DO i=0,sNx+1
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

         cgBeta   = eta_qrNtile(bi,bj)/eta_qrNM1(bi,bj)
         eta_qrNM1(bi,bj) = eta_qrNtile(bi,bj)

         DO k=1,Nr
          DO j=0,sNy+1
           DO i=0,sNx+1
            cg3d_s(i,j,k,bi,bj) = cg3d_q(i,j,k,bi,bj)
     &                   + cgBeta*cg3d_s(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO

C==    Evaluate laplace operator on conjugate gradient vector
C==    q = A.s
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
         alpha = eta_qrNtile(bi,bj)/alphaTile(bi,bj)

C==    Update simultaneously solution and residual vectors (and Iter number)
C      Now compute "interior" points.
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
         actualIts(bi,bj) = it3d

         IF ( printResidual ) THEN
          IF ( MOD( it3d-1, printResidualFreq ).EQ.0 ) THEN
           WRITE(msgBuf,'(A,2I4,A,I6,A,1PE21.14)') ' cg3d(bi,bj=', bi,
     &      bj, '): iter=', it3d, ' ; resid.= ', SQRT(errTile(bi,bj))
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
         ENDIF

         CALL FILL_HALO_LOCAL_RL(
     U                  cg3d_r(0,0,1,bi,bj),
     I                  1, 1, 1, 1, Nr,
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

      IF ( debugLevel.GE.debLevC .AND. diagFreq.GT.0. ) THEN
        CALL WRITE_FLD_S3D_RL(
     I        'cg3d_r_F', 'I10', 1, Nr, cg3d_r, myIter, myThid )
      ENDIF

C--   Un-normalise the answer
      numIters = 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           cg3d_x(i,j,k,bi,bj) = cg3d_x(i,j,k,bi,bj)/rhsNorm(bi,bj)
          ENDDO
         ENDDO
        ENDDO
        numIters = MAX( actualIts(bi,bj), numIters )
       ENDDO
      ENDDO

C--   Return parameters to caller
C     return largest Iter # and Max residual     in numIters and lastResidual
      _GLOBAL_MAX_RL( err_sq, myThid )
      lastResidual = SQRT(err_sq)
      alpha = numIters
      _GLOBAL_MAX_RL( alpha, myThid )
      numIters = NINT( alpha )

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
