#include "CPP_OPTIONS.h"

C--  File eigen_lanczos.F: lanczos method  S/R:
C--   Contents
C--   o EIGEN_LANCZOS
C--   o RATQR

CBOP
C     !ROUTINE: EIGEN_LANCZOS
C     !INTERFACE:
      SUBROUTINE EIGEN_LANCZOS(
     U                u, v,
     I                myThid )
C     !DESCRIPTION:
C     *================================================================*
C     | SUBROUTINE EIGEN_LANCZOS
C     | o This routine implements the Lanczos based method to solve the
C     |   maximum and minimum eigenvalues of the coefficient matrix $A$.
C     | o Or $AM$ if a preconditioner $M^{-1}$ is provided.
C     *================================================================*
C     | References:
C     | o Hu, Y., Huang, X., Wang, X., Fu, H., Xu, S., Ruan, H., ...
C     |   & Yang, G. (2013). A scalable barotropic mode solver for the
C     |   parallel ocean program. In Euro-Par 2013 Parallel Processing
C     |   (pp. 739-750). Springer Berlin Heidelberg.
C     | o Paige, C. C. (1980). Accuracy and effectiveness of the
C     |   Lanczos algorithm for the symmetric eigenproblem.
C     |   Linear algebra and its applications, 34, 235-258.
C     *================================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"
#ifdef ALLOW_USE_MPI
# include "EESUPPORT.h"
#endif /* ALLOW_USE_MPI */

C     !INPUT/OUTPUT PARAMETERS:
C     u, v      :: max and min eigenvalues
C     myThid    :: Thread on which I am working.
      _RL  u, v
      INTEGER myThid

#ifdef ALLOW_PCSI
C     !LOCAL VARIABLES:
      _RL LanczosconvergenceCriterion
      PARAMETER ( LanczosconvergenceCriterion  = 0.001 )
      INTEGER maxlanczosstep
      PARAMETER ( maxlanczosstep = 1000 )

      _RL valpha(maxlanczosstep), vbeta(maxlanczosstep)
      _RL malpha(maxlanczosstep), mbeta(maxlanczosstep)
      _RL eigen_q0(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL eigen_r (1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL eigen_q1(1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL eigen_s (1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL eigen_p (1-1:sNx+1,1-1:sNy+1,nSx,nSy)
      _RL sumtile(nSx,nSy)
      _RL eigen_alpha, eigen_beta, eigen_csc, mineig
      INTEGER  i, j, bi, bj
      INTEGER  m, info
#ifdef ALLOW_USE_MPI
      INTEGER mpiierr
#endif /* ALLOW_USE_MPI */
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C     set random vector R, intialize vectors
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-1,sNy+1
         DO i=1-1,sNx+1
          eigen_r (i,j,bi,bj) = 1. _d 0
          eigen_q0(i,j,bi,bj) = 0. _d 0
          eigen_q1(i,j,bi,bj) = 0. _d 0
          eigen_s (i,j,bi,bj) = 0. _d 0
          eigen_p (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     proconditioning s = M*r
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          eigen_s(i,j,bi,bj) =
     &    pW(i  ,j  ,bi,bj)*eigen_r(i-1,j  ,bi,bj)
     &   +pW(i+1,j  ,bi,bj)*eigen_r(i+1,j  ,bi,bj)
     &   +pS(i  ,j  ,bi,bj)*eigen_r(i  ,j-1,bi,bj)
     &   +pS(i  ,j+1,bi,bj)*eigen_r(i  ,j+1,bi,bj)
     &   +pC(i  ,j  ,bi,bj)*eigen_r(i  ,j  ,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        sumtile(bi,bj) = 0. _d 0
        DO j=1,sNy
         DO i=1,sNx
          sumtile(bi,bj) = sumtile(bi,bj) +
     &                   eigen_r(i,j,bi,bj)*eigen_s(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( sumtile, eigen_csc, myThid )
      eigen_csc = SQRT(-eigen_csc)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-1,sNy+1
         DO i=1-1,sNx+1
          eigen_q1(i,j,bi,bj) = eigen_r(i,j,bi,bj)/eigen_csc
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_S3D_RL( eigen_q1, 1, myThid )

      eigen_beta = 0. _d 0
      u       = 0. _d 0
      v       = 0. _d 0
      mineig  = 1. _d 0

      lanczos_iter: DO m = 1, maxlanczosstep

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            eigen_p(i,j,bi,bj) =
     &      pW(i  ,j  ,bi,bj)*eigen_q1(i-1,j  ,bi,bj)
     &     +pW(i+1,j  ,bi,bj)*eigen_q1(i+1,j  ,bi,bj)
     &     +pS(i  ,j  ,bi,bj)*eigen_q1(i  ,j-1,bi,bj)
     &     +pS(i  ,j+1,bi,bj)*eigen_q1(i  ,j+1,bi,bj)
     &     +pC(i  ,j  ,bi,bj)*eigen_q1(i  ,j  ,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL EXCH_S3D_RL( eigen_p, 1, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            eigen_r(i,j,bi,bj) =
     &      aW2d(i  ,j  ,bi,bj)*eigen_p(i-1,j  ,bi,bj)
     &     +aW2d(i+1,j  ,bi,bj)*eigen_p(i+1,j  ,bi,bj)
     &     +aS2d(i  ,j  ,bi,bj)*eigen_p(i  ,j-1,bi,bj)
     &     +aS2d(i  ,j+1,bi,bj)*eigen_p(i  ,j+1,bi,bj)
     &     +aC2d(i  ,j  ,bi,bj)*eigen_p(i  ,j  ,bi,bj)
     &     -eigen_beta*eigen_q0(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          sumtile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
            sumtile(bi,bj) = sumtile(bi,bj) +
     &                   eigen_p(i,j,bi,bj)*eigen_r(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( sumtile, eigen_csc, myThid )
        eigen_alpha = -eigen_csc

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            eigen_r(i,j,bi,bj) = eigen_r(i,j,bi,bj)-
     &               eigen_alpha*eigen_q1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL EXCH_S3D_RL( eigen_r, 1, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            eigen_s(i,j,bi,bj) =
     &      pW(i  ,j  ,bi,bj)*eigen_r(i-1,j  ,bi,bj)
     &     +pW(i+1,j  ,bi,bj)*eigen_r(i+1,j  ,bi,bj)
     &     +pS(i  ,j  ,bi,bj)*eigen_r(i  ,j-1,bi,bj)
     &     +pS(i  ,j+1,bi,bj)*eigen_r(i  ,j+1,bi,bj)
     &     +pC(i  ,j  ,bi,bj)*eigen_r(i  ,j  ,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          sumtile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
            sumtile(bi,bj) = sumtile(bi,bj) +
     &                     eigen_r(i,j,bi,bj)*eigen_s(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( sumtile, eigen_csc, myThid )
        eigen_beta = SQRT(-eigen_csc)

        valpha(m) = eigen_alpha
        vbeta(m)  = eigen_beta

C       Gershgorin circle theorem to estimate the largest eigenvaule
        IF ( m .EQ. 1 ) THEN
          u = valpha(1) + vbeta(1)
        ELSE
          u = MAX(u, valpha(m) + vbeta(m) + vbeta(m-1))
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            eigen_q0(i,j,bi,bj) = eigen_q1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            eigen_q1(i,j,bi,bj) = eigen_r(i,j,bi,bj)/eigen_beta
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL EXCH_S3D_RL( eigen_q1, 1, myThid )

C        write(*,*) "alpha beta ", m,eigen_alpha,eigen_beta

C       compute eigenvalues of the tridiagonal matrix T every 10 steps
        IF ((mod(m,10) == 0) .or. (m ==maxlanczosstep)) then
         IF ( myProcId == 0 ) THEN
          DO i = 1, m-1
           malpha(i)  = valpha(i)
           mbeta(i+1) = vbeta(i)
          END DO
          malpha(m) = valpha(m)
          mbeta(1) = 0.0
          info = 0
C         compute smallest eigenvalue
          CALL RATQR(m, 1.D-08, malpha, mbeta, v, info)
          IF (info .NE. 0) THEN
           WRITE(*,*)
     &     'eigen_Lanczos: error estimating smallest eigenvalue !!!'
           EXIT
          END IF
         END IF

#ifdef ALLOW_USE_MPI
         CALL MPI_BCAST(v,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_MODEL,
     &                 mpiierr)
#endif /* ALLOW_USE_MPI */

         WRITE(msgBuf,'(A,I6,A,1P2E21.14)')
     &    ' Lanczos: steps =', m , ' ; eigs:= ', u, v
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )

         ! check convergence of the smallest eigenvalue
         IF (abs(1-v/mineig ) .LT.  LanczosconvergenceCriterion) THEN
          EXIT lanczos_iter
         END IF
         mineig = v  ! save previous value
        END IF
      END DO lanczos_iter

#endif /* ALLOW_PCSI */
      RETURN
      END SUBROUTINE EIGEN_LANCZOS

C-----------------------------------------------------------------------
      SUBROUTINE  RATQR(n, eps1, d, e, mineig,ierr )
CBOP
C     !DESCRIPTION:
C      This subroutine finds the algebraically smallest
C      eigenvalue of a positive definite symmetric tridiagonal matrix by the
C      rational QR method with Newton corrections.
C      Follow EISPACK lib subroutine RATQR
C
      IMPLICIT NONE
C     !INPUT/OUTPUT PARAMETERS:
C     n       :: matrix order
C     d       :: diagonal elements
C     e       :: off-diagonal elements, e(1) is arbitrary
C     eps1    :: convergence tolerance
C     mineig  :: smallest eigevalue
C     ierr    :: status flag
      INTEGER maxlanczosstep
      PARAMETER ( maxlanczosstep = 1000 )

      INTEGER  n
      _RL      d(maxlanczosstep), e(maxlanczosstep)
      _RL      eps1, mineig
      INTEGER  ierr

#ifdef ALLOW_PCSI
C     !LOCAL VARIABLES:
      _RL     bd(maxlanczosstep), w(maxlanczosstep)
      _RL     f, ep, delta, err
C     local counters
      INTEGER i, ii
      _RL     p, q, qp,r,s,tot
CEOP
      ierr = 0
      w(1:n) = d(1:n)
      err = 0. _d 0
      s = 0. _d 0
      tot = w(1)
      q = 0. _d 0

      DO i = 1, n
       p = q
       bd(i) = e(i)*e(i)
       q = 0. _d 0
       IF ( i /= n ) THEN
         q = abs ( e(i+1) )
       END IF
       Tot = min ( w(i) - p - q, tot )
      END DO
      bd(1) = 0. _d 0
      IF ( tot < 0. _d 0  ) THEN
        tot = 0. _d 0
      ELSE
        w(1:n) = w(1:n) - tot
      ENDIF

C     QR transformation.
      DO while (.true.)
        tot = tot + s
        delta = w(n) - s
        i = n
        IF ( delta <= eps1 ) THEN
          exit
        END IF
        f = bd(n) / delta
        qp = delta + f
        p = 1. _d 0
        DO ii = 1, n - 1
          i = n - ii
          q = w(i) - s - f
          r = q / qp
          p = p * r + 1. _d 0
          ep = f * r
          w(i+1) = qp + ep
          delta = q - ep
C         check convergence
          IF ( delta <= eps1 ) EXIT
          f = bd(i) / q
          qp = delta + f
          bd(i+1) = qp * ep
        END DO
C       check convergence
        if ( delta <= eps1 ) exit
        w(1) = qp
        s = qp / p
C       Set error: irregular end of iteration.
        IF ( tot + s <= tot ) THEN
          ierr = 1
          RETURN
        ENDIF
      ENDDO
      w(1) = tot
      err = err + abs ( delta)
      bd(1) = err
      mineig = w(1)

#endif /* ALLOW_PCSI */
      RETURN
      END SUBROUTINE RATQR
