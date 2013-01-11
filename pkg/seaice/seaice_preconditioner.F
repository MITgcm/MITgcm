C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_preconditioner.F,v 1.15 2013/01/11 20:18:57 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

C--   File seaice_preconditioner.F:
C--   Contents
C--   o SEAICE_PRECONDITIONER
C--   o SEAICE_PRECOND_RSHU
C--   o SEAICE_PRECOND_RSHV

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_PRECONDITIONER
C     !INTERFACE:
      SUBROUTINE SEAICE_PRECONDITIONER(
     U     duIce, dvIce,
     I     zetaPre, etaPre, etaZpre, dwatPre,
     I     newtonIter, krylovIter, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_PRECONDITIONER
C     | o Preconditioner for Jacobian-free Newton-Krylov solver,
C     |   compute improved first guess solution du/vIce, with
C     |   suboptimal solver, here LSOR
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
C     krylovIter :: current iterate of Krylov iteration
C     *Pre are precomputed and held fixed during the Krylov iteration
      _RL  zetaPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL   etaPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaZPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  dwatPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER newtonIter
      INTEGER krylovIter
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     du/vIce :: solution vector
      _RL duIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

#if ( defined (SEAICE_CGRID) && \
      defined (SEAICE_ALLOW_JFNK) && \
      defined (SEAICE_ALLOW_DYNAMICS) )
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj  :: Loop counters

      INTEGER i, j, m, bi, bj, j1, j2, im, jm
      INTEGER iCount
      INTEGER k
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL WFAU, WFAV
      _RL AA3

C     diagonals of coefficient matrices
      _RL AU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL AV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     RHS
      _RL rhsU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsU0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhsV0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, u(j+/-1)
      _RL uRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(i+/-1)
      _RL vRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     abbreviations
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     auxillary fields
      _RL URT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CUU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VRT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CVV  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS SINWAT
      _RL COSWAT
      _RL coriFac
      _RL fricFac
      LOGICAL printResidual
      _RL residUini, residVini, residUend, residVend
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      printResidual = debugLevel.GE.debLevC
     &  .AND. DIFFERENT_MULTIPLE( SEAICE_monFreq, myTime, deltaTClock )

C     convergence is affected with coriFac = fricFac = 1
      coriFac = 0. _d 0
      fricFac = coriFac
C     surface level
      k = 1
C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C     copy relaxation parameters
      WFAU=SEAICE_LSRrelaxU
      WFAV=SEAICE_LSRrelaxV
C
C     Initialise
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rhsU (I,J,bi,bj) = 0. _d 0
          rhsV (I,J,bi,bj) = 0. _d 0
          rhsU0(I,J,bi,bj) = duIce(I,J,bi,bj)
          rhsV0(I,J,bi,bj) = dvIce(I,J,bi,bj)
C     first guess for the increment is 0.
          duIce(I,J,bi,bj) = 0. _d 0
          dvIce(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
C     some abbreviations
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=0,sNy
         DO I=0,sNx
          etaPlusZeta (I,J,bi,bj)= etaPre(I,J,bi,bj)+zetaPre(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj)=zetaPre(I,J,bi,bj)- etaPre(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
C     calculate coefficients of tridiagonal matrices for both u- and
C     v-equations
C
      CALL SEAICE_CALC_LSR_COEFFS(
     I     etaPlusZeta, zetaMinusEta, etaZpre, dwatPre,
     O     AU, BU, CU, AV, BV, CV, uRt1, uRt2, vRt1, vRt2,
     I     myTime, myIter, myThid )

#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver from modifying OB values:
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          IF ( maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj) .EQ. 0. ) THEN
           AU(I,J,bi,bj)   = ZERO
           BU(I,J,bi,bj)   = ONE
           CU(I,J,bi,bj)   = ZERO
           uRt1(I,J,bi,bj) = ZERO
           uRt2(I,J,bi,bj) = ZERO
          ENDIF
          IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
           AV(I,J,bi,bj)   = ZERO
           BV(I,J,bi,bj)   = ONE
           CV(I,J,bi,bj)   = ZERO
           vRt1(I,J,bi,bj) = ZERO
           vRt2(I,J,bi,bj) = ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* OBCS_UVICE_OLD */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A,I3,A)')
     &        'Uice pre iter (SEAICE_PRECONDITIONER',
     &      newtonIter, ',', krylovIter, ')'
        CALL DEBUG_STATS_RL( 1, UICE, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A,I3,A)')
     &        'Vice pre iter (SEAICE_PRECONDITIONER',
     &      newtonIter, ',', krylovIter, ')'
        CALL DEBUG_STATS_RL( 1, VICE, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

C--   Calculate initial residual of the linearised system
      IF ( printResidual ) THEN
C     set up right-hand side now (will be redone in each iteration)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           rhsU(I,J,bi,bj) = rhsU0(I,J,bi,bj)
           rhsV(I,J,bi,bj) = rhsV0(I,J,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_PRECOND_RSHU (
     I        zetaMinusEta, etaPlusZeta, etaZpre, dvIce,
     I        dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     O        rhsU,
     I        bi,bj,myThid )
         CALL SEAICE_PRECOND_RSHV (
     I        zetaMinusEta, etaPlusZeta, etaZpre, duIce,
     I        dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     O        rhsV,
     I        bi,bj,myThid )
#ifndef OBCS_UVICE_OLD
         DO J=1,sNy
          DO I=1,sNx
           IF ( maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj) .EQ. 0. ) THEN
            rhsU(I,J,bi,bj) = duIce(I,J,bi,bj)
           ENDIF
           IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
            rhsV(I,J,bi,bj) = dvIce(I,J,bi,bj)
           ENDIF
          ENDDO
         ENDDO
#endif /* OBCS_UVICE_OLD */
        ENDDO
       ENDDO
       CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, duIce, dvIce,
     O                  residUini, residVini, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C NOW DO ITERATION

C ITERATION START -----------------------------------------------------

      iCount = SOLV_MAX_ITERS

      DO m = 1, SOLV_MAX_ITERS

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C     save du/vIce prior to iteration
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uTmp(I,J,bi,bj)=duIce(I,J,bi,bj)
           vTmp(I,J,bi,bj)=dvIce(I,J,bi,bj)
          ENDDO
         ENDDO

C     set up right-hand sides for u- and v-equations
         DO j=1,sNy
          DO i=1,sNx
           rhsU(I,J,bi,bj) = rhsU0(I,J,bi,bj)
#ifdef SEAICE_LSR_SYMMETRIC_SWEEP
           rhsV(I,J,bi,bj) = rhsV0(I,J,bi,bj)
#endif /* SEAICE_LSR_SYMMETRIC_SWEEP */
          ENDDO
         ENDDO
         CALL SEAICE_PRECOND_RSHU (
     I        zetaMinusEta, etaPlusZeta, etaZpre, dvIce,
     I        dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     U        rhsU,
     I        bi,bj,myThid )
#ifdef SEAICE_LSR_SYMMETRIC_SWEEP
         CALL SEAICE_PRECOND_RSHV (
     I        zetaMinusEta, etaPlusZeta, etaZpre, duIce,
     I        dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     U        rhsV,
     I        bi,bj,myThid )
#endif /* SEAICE_LSR_SYMMETRIC_SWEEP */
#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver to modify OB values:
         DO J=1,sNy
          DO I=1,sNx
           IF ( maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj) .EQ. 0. ) THEN
            rhsU(I,J,bi,bj) = duIce(I,J,bi,bj)
           ENDIF
#ifdef SEAICE_LSR_SYMMETRIC_SWEEP
           IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
            rhsV(I,J,bi,bj) = dvIce(I,J,bi,bj)
           ENDIF
#endif /* SEAICE_LSR_SYMMETRIC_SWEEP */
          ENDDO
         ENDDO
#endif /* OBCS_UVICE_OLD */

C Solve for uIce :
         DO J=1,sNy
          DO I=1,sNx
           AA3 = ZERO
           IF (I.EQ.1)   AA3 = AA3 - AU(I,J,bi,bj)*duIce(I-1,J,bi,bj)
           IF (I.EQ.sNx) AA3 = AA3 - CU(I,J,bi,bj)*duIce(I+1,J,bi,bj)

           URT(I,J)=rhsU(I,J,bi,bj)
     &          + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &          + uRt1(I,J,bi,bj)*uTmp(I,J-1,bi,bj)
     &          + uRt2(I,J,bi,bj)*uTmp(I,J+1,bi,bj)
#else
     &          + uRt1(I,J,bi,bj)*duIce(I,J-1,bi,bj)
     &          + uRt2(I,J,bi,bj)*duIce(I,J+1,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
           URT(I,J)=URT(I,J)* seaiceMaskU(I,J,bi,bj)
          ENDDO

          DO I=1,sNx
           CUU(I,J)=CU(I,J,bi,bj)
          ENDDO
          CUU(1,J)=CUU(1,J)/BU(1,J,bi,bj)
          URT(1,J)=URT(1,J)/BU(1,J,bi,bj)
#ifdef SEAICE_VECTORIZE_LSR
         ENDDO
C     start a new loop with reversed order to support automatic vectorization
         DO I=2,sNx
          IM=I-1
          DO J=1,sNy
#else /* do not SEAICE_VECTORIZE_LSR */
          DO I=2,sNx
           IM=I-1
#endif /* SEAICE_VECTORIZE_LSR */
           CUU(I,J)=CUU(I,J)/(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J))
           URT(I,J)=(URT(I,J)-AU(I,J,bi,bj)*URT(IM,J))
     &           /(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J))
          ENDDO
#ifdef SEAICE_VECTORIZE_LSR
         ENDDO
C     go back to original order
         DO J=1,sNy
#endif /* SEAICE_VECTORIZE_LSR */
          DO I=1,sNx-1
           J1=sNx-I
           J2=J1+1
           URT(J1,J)=URT(J1,J)-CUU(J1,J)*URT(J2,J)
          ENDDO
          DO I=1,sNx
           duIce(I,J,bi,bj)=uTmp(I,J,bi,bj)
     &          +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
          ENDDO
         ENDDO

#ifndef SEAICE_LSR_SYMMETRIC_SWEEP
        ENDDO
       ENDDO
C     ideally one would like to get rid off this exchange
       CALL EXCH_UV_XY_RL( duIce, dvIce, .TRUE., myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C     set up right-hand-side for v-equation
         DO j=1,sNy
          DO i=1,sNx
           rhsV(I,J,bi,bj) = rhsV0(I,J,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_PRECOND_RSHV (
     I        zetaMinusEta, etaPlusZeta, etaZpre, duIce,
     I        dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     U        rhsV,
     I        bi,bj,myThid )
#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver to modify OB values:
         DO J=1,sNy
          DO I=1,sNx
           IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
            rhsV(I,J,bi,bj) = dvIce(I,J,bi,bj)
           ENDIF
          ENDDO
         ENDDO
#endif /* OBCS_UVICE_OLD */
#endif /* SEAICE_LSR_SYMMETRIC_SWEEP */

C Solve for dvIce
         DO I=1,sNx
          DO J=1,sNy
           AA3 = ZERO
           IF (J.EQ.1)   AA3 = AA3 - AV(I,J,bi,bj)*dvIce(I,J-1,bi,bj)
           IF (J.EQ.sNy) AA3 = AA3 - CV(I,J,bi,bj)*dvIce(I,J+1,bi,bj)

           VRT(I,J)=rhsV(I,J,bi,bj)
     &          + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &          + vRt1(I,J,bi,bj)*vTmp(I-1,J,bi,bj)
     &          + vRt2(I,J,bi,bj)*vTmp(I+1,J,bi,bj)
#else
     &          + vRt1(I,J,bi,bj)*dvIce(I-1,J,bi,bj)
     &          + vRt2(I,J,bi,bj)*dvIce(I+1,J,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
           VRT(I,J)=VRT(I,J)* seaiceMaskV(I,J,bi,bj)
          ENDDO

          DO J=1,sNy
           CVV(I,J)=CV(I,J,bi,bj)
          ENDDO
          CVV(I,1)=CVV(I,1)/BV(I,1,bi,bj)
          VRT(I,1)=VRT(I,1)/BV(I,1,bi,bj)
          DO J=2,sNy
           JM=J-1
           CVV(I,J)=CVV(I,J)/(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
           VRT(I,J)=(VRT(I,J)-AV(I,J,bi,bj)*VRT(I,JM))
     &          /(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
          ENDDO
          DO J=1,sNy-1
           J1=sNy-J
           J2=J1+1
           VRT(I,J1)=VRT(I,J1)-CVV(I,J1)*VRT(I,J2)
          ENDDO
          DO J=1,sNy
           dvIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &          +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
          ENDDO
         ENDDO

C     end bi,bj-loops
        ENDDO
       ENDDO

       CALL EXCH_UV_XY_RL( duIce, dvIce, .TRUE., myThid )

      ENDDO
C ITERATION END -----------------------------------------------------

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( printResidual ) THEN
C--   Calculate final residual of the linearised system
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, duIce, dvIce,
     O                  residUend, residVend, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
        _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(A,A,1X,1P2E16.8)')
     &       ' SEAICE_PRECONDITIONER: Residual Initial Uice,Vice     =',
     &       '     ', residUini, residVini
        WRITE(standardMessageUnit,'(A,I4,A,I4,A,I6,1P2E16.8)')
     &       ' SEAICE_PRECONDITIONER (iter=',newtonIter,',',
     &       krylovIter, ') iters, U/VResid=',
     &       iCount, residUend, residVend
        _END_MASTER( myThid )
      ENDIF
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A,I3,A)')
     &        'Uice post iter (SEAICE_PRECONDITIONER',
     &      newtonIter, ',', krylovIter, ')'
        CALL DEBUG_STATS_RL( 1, UICE, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A,I3,A)')
     &        'Vice post iter (SEAICE_PRECONDITIONER',
     &      newtonIter, ',', krylovIter, ')'
        CALL DEBUG_STATS_RL( 1, VICE, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

C     APPLY MASKS
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          duIce(I,J,bi,bj)=duIce(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          dvIce(I,J,bi,bj)=dvIce(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_PRECOND_RSHU
C     !INTERFACE:
      SUBROUTINE SEAICE_PRECOND_RSHU (
     I     zetaMinusEta, etaPlusZeta, etaZpre, vIceLoc,
     I     dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     U     rhsU,
     I     bi,bj,myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_PRECOND_RSHU
C     | o Calculate the right-hand-side of the u-momentum equation
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZpre     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dwatPre     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL coriFac, fricFac
      _RS SINWAT
      _RL COSWAT
      _RL rhsU        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER I,J,K
C     contribution of sigma on righ hand side
      _RL sig11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacM

C     surface level
      k = 1

C     contribution of sigma11 to rhs
      DO J=1,sNy
       DO I=0,sNx
        sig11(I,J) = zetaMinusEta(I,J,bi,bj)
     &       * ( vIceLoc(I,J+1,bi,bj) - vIceLoc(I,J,bi,bj) )
     &       * _recip_dyF(I,J,bi,bj)
     &       + etaPlusZeta(I,J,bi,bj) * k2AtC(I,J,bi,bj)
     &       * 0.5 _d 0 * (vIceLoc(I,J+1,bi,bj)+vIceLoc(I,J,bi,bj))
       ENDDO
      ENDDO
C     contribution of sigma12 to rhs of u-equation
      DO J=1,sNy+1
       DO I=1,sNx
        hFacM = seaiceMaskV(I,J,bi,bj) - seaiceMaskV(I-1,J,bi,bj)
        sig12(I,J) = etaZpre(I,J,bi,bj) * (
     &       ( vIceLoc(I,J,bi,bj) - vIceLoc(I-1,J,bi,bj) )
     &       * _recip_dxV(I,J,bi,bj)
     &       - k1AtZ(I,J,bi,bj)
     &       * 0.5 _d 0 * (vIceLoc(I,J,bi,bj)+vIceLoc(I-1,J,bi,bj))
     &       )
C     free slip conditions (sig12=0) are taken care of by masking sig12
     &       *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &       *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     no slip boundary conditions (v(i-1)=-v(i))
C     v(i)+v(i-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with v(i)-v(i-1)
     &       + etaZpre(I,J,bi,bj) * _recip_dxV(I,J,bi,bj)
     &       * ( vIceLoc(I,J,bi,bj) + vIceLoc(I-1,J,bi,bj) )
     &       * hFacM * 2. _d 0
       ENDDO
      ENDDO

      DO J=1,sNy
       DO I=1,sNx
C     contribution to the rhs part of grad(sigma)_x
        rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj)
     &       + recip_rAw(I,J,bi,bj) * seaiceMaskU(I,J,bi,bj) *
     &       ( _dyF(I  ,J  ,bi,bj)*sig11(I  ,J  )
     &       - _dyF(I-1,J  ,bi,bj)*sig11(I-1,J  )
     &       + _dxV(I  ,J+1,bi,bj)*sig12(I  ,J+1)
     &       - _dxV(I  ,J  ,bi,bj)*sig12(I  ,J  ) )
       ENDDO
      ENDDO
      IF ( fricFac+coriFac .NE. 0. _d 0 ) THEN
C     neglected for preconditioning step
       DO J=1,sNy
        DO I=1,sNx
         rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj)
     &        - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &        ( dwatPre(I  ,J,bi,bj) * 0.5 _d 0 *
     &        (vVel(I  ,J  ,k,bi,bj)-vIceLoc(I  ,J  ,bi,bj)
     &        +vVel(I  ,J+1,k,bi,bj)-vIceLoc(I  ,J+1,bi,bj))
     &        + dwatPre(I-1,J,bi,bj) * 0.5 _d 0 *
     &        (vVel(I-1,J  ,k,bi,bj)-vIceLoc(I-1,J  ,bi,bj)
     &        +vVel(I-1,J+1,k,bi,bj)-vIceLoc(I-1,J+1,bi,bj))
     &        ) * fricFac
C-    add Coriolis term
         rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj) + 0.5 _d 0 *
     &        ( seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &        *0.5 _d 0*(vIceLoc( i ,j,bi,bj)+vIceLoc( i ,j+1,bi,bj))
     &        + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &        *0.5 _d 0*(vIceLoc(i-1,j,bi,bj)+vIceLoc(i-1,j+1,bi,bj))
     &        ) * coriFac
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SEAICE_PRECOND_RSHV
C     !INTERFACE:
      SUBROUTINE SEAICE_PRECOND_RSHV (
     I     zetaMinusEta, etaPlusZeta, etaZpre, uIceLoc,
     I     dwatPre, coriFac, fricFac, SINWAT, COSWAT,
     U     rhsV,
     I     bi,bj,myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_PRECOND_RSHV
C     | o Calculate the right-hand-side of the v-momentum equation
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaZpre     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uIceLoc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dwatPre     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL coriFac, fricFac
      _RS SINWAT
      _RL COSWAT
      _RL rhsV        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER I,J,K
C     contribution of sigma on righ hand side
      _RL sig22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacM

C     surface level
      k = 1

C     contribution of sigma22 to rhs
      DO J=0,sNy
       DO I=1,sNx
        sig22(I,J) =  zetaMinusEta(I,J,bi,bj)
     &       * ( uIceLoc(I+1,J,bi,bj) - uIceLoc(I,J,bi,bj) )
     &       * _recip_dxF(I,J,bi,bj)
     &       + etaPlusZeta(I,J,bi,bj) * k1AtC(I,J,bi,bj)
     &       * 0.5 _d 0 * (uIceLoc(I+1,J,bi,bj)+uIceLoc(I,J,bi,bj))
       ENDDO
      ENDDO
C     contribution of sigma12 to rhs of v-equation
      DO J=1,sNy
       DO I=1,sNx+1
        hFacM = seaiceMaskU(i,j,bi,bj) - seaiceMaskU(i,j-1,bi,bj)
        sig12(I,J) = etaZpre(I,J,bi,bj) * (
     &       ( uIceLoc(I,J,bi,bj) - uIceLoc(I,J-1,bi,bj) )
     &       * _recip_dyU(I,J,bi,bj)
     &       - k2AtZ(I,J,bi,bj)
     &       * 0.5 _d 0 * (uIceLoc(I,J,bi,bj)+uIceLoc(I,J-1,bi,bj))
     &       )
C     free slip conditions (sig12=0) are taken care of by masking sig12,
     &       *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &       *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     no slip boundary conditions (u(j-1)=-u(j))
C     u(j)+u(j-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with u(j)-u(j-1)
     &       + etaZpre(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &       * ( uIceLoc(I,J,bi,bj) + uIceLoc(I,J-1,bi,bj) )
     &       * hFacM * 2. _d 0
       ENDDO
      ENDDO

      DO J=1,sNy
       DO I=1,sNx
C     contribution to the rhs part of grad(sigma)_y
        rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj)
     &       + recip_rAs(I,J,bi,bj) * seaiceMaskV(I,J,bi,bj) *
     &       ( _dyU(I+1,J  ,bi,bj) * sig12(I+1,J  )
     &       - _dyU(I  ,J  ,bi,bj) * sig12(I  ,J  )
     &       + _dxF(I  ,J  ,bi,bj) * sig22(I  ,J  )
     &       - _dxF(I  ,J-1,bi,bj) * sig22(I  ,J-1) )
       ENDDO
      ENDDO
C     neglected for preconditioning step
      IF ( fricFac+coriFac .NE. 0. _d 0 ) THEN
       DO J=1,sNy
        DO I=1,sNx
         rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj)
     &        + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &        ( dwatPre(I,J  ,bi,bj) * 0.5 _d 0 *
     &        (uVel(I  ,J  ,k,bi,bj)-uIceLoc(I  ,J  ,bi,bj)
     &        +uVel(I+1,J  ,k,bi,bj)-uIceLoc(I+1,J  ,bi,bj))
     &        + dwatPre(I,J-1,bi,bj) * 0.5 _d 0 *
     &        (uVel(I  ,J-1,k,bi,bj)-uIceLoc(I  ,J-1,bi,bj)
     &        +uVel(I+1,J-1,k,bi,bj)-uIceLoc(I+1,J-1,bi,bj))
     &        ) * fricFac
C-    add Coriolis term
         rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj) - 0.5 _d 0 *
     &        ( seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &        *0.5 _d 0*(uIceLoc(i  ,j  ,bi,bj)+uIceLoc(i+1,  j,bi,bj))
     &        + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &        *0.5 _d 0*(uIceLoc(i  ,j-1,bi,bj)+uIceLoc(i+1,j-1,bi,bj))
     &        ) * coriFac
        ENDDO
       ENDDO
      ENDIF

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
