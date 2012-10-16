C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_preconditioner.F,v 1.1 2012/10/16 06:59:30 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_PRECONDITIONER
C     !INTERFACE:
      SUBROUTINE SEAICE_PRECONDITIONER( 
     U     duIce, dvIce, 
     I     zetaPre, etaPre, dwatPre,
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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
C     krylovIter :: current iterate of Krylov iteration
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER newtonIter
      INTEGER krylovIter
C     du/vIce :: solution vector
      _RL duIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     zetaPre/etaPre are precomputed viscosities held fixed
C     during the Krylov iteration
      _RL zetaPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  etaPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dwatPre(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

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
      INTEGER ICOUNT1, ICOUNT2
      INTEGER k
      INTEGER SOLV_MAX_TMP
      LOGICAL doIterate4u, doIterate4v
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL AA3, S1, S2, S1A, S2A
      _RL hFacM, hFacP

C     coefficients of ice velocities in coefficient matrix
C     for both U and V-equation
C     XX: double derivative in X
C     YY: double derivative in Y
C     XM: metric term with derivative in X
C     YM: metric term with derivative in Y
      _RL UXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL UYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
C     coefficients for lateral points, u(j+/-1)
      _RL uRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     coefficients for lateral points, v(i+/-1)
      _RL vRt1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vRt2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     abbreviations
      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etaMeanZ    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     contribution of sigma on righ hand side
      _RL sig11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     auxillary fields
      _RL URT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CUU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VRT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CVV  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL COSWAT
      _RS SINWAT
      LOGICAL printResidual
      _RL massU, massV
      _RL residUini, residVini, residUend, residVend
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      printResidual = debugLevel.GE.debLevA
     &  .AND. DIFFERENT_MULTIPLE( SEAICE_monFreq, myTime, deltaTClock )
      massU = -1. _d 0
      massV = -1. _d 0

C     surface level
      k = 1
C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C SET SOME VALUES
      WFAU1=0.95 _d 0
      WFAV1=0.95 _d 0
      WFAU2=ZERO
      WFAV2=ZERO

      S1A=0.80 _d 0
      S2A=0.80 _d 0
      WFAU=WFAU1
      WFAV=WFAV1

      S1 = 0.
      S2 = 0.

C     Only do the predictor time step
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
CML   I am not so sure about this
CML          uIceNm1(i,j,bi,bj) = 0. _d 0
CML          vIceNm1(i,j,bi,bj) = 0. _d 0
          uIceC(I,J,bi,bj)   = 0. _d 0
          vIceC(I,J,bi,bj)   = 0. _d 0
          rhsU(I,J,bi,bj)    = 0. _d 0
          rhsV(I,J,bi,bj)    = 0. _d 0
          rhsU(I,J,bi,bj)    = duIce(I,J,bi,bj)
          rhsV(I,J,bi,bj)    = dvIce(I,J,bi,bj)
C     first guess for the increment is 0.
          duIce(I,J,bi,bj)   = 0. _d 0
          dvIce(I,J,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C
        DO J=1,sNy
         DO I=1,sNx
          DRAGS(I,J,bi,bj) = dwatPre(I,J,bi,bj)*COSWAT
C     set up anti symmetric drag force and add in current force
C     ( remember to average to correct velocity points )
          rhsU(I,J,bi,bj)= rhsU(I,J,bi,bj) + FORCEX0(I,J,bi,bj)+
     &         0.5 _d 0 * ( dwatPre(I,J,bi,bj)+dwatPre(I-1,J,bi,bj) ) *
     &         COSWAT * uVel(I,J,k,bi,bj)
C     neglected for preconditioning step
CML     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
CML     &         ( dwatPre(I  ,J,bi,bj) * 0.5 _d 0 *
CML     &          (vVel(I  ,J  ,k,bi,bj)-vIceC(I  ,J  ,bi,bj)
CML     &          +vVel(I  ,J+1,k,bi,bj)-vIceC(I  ,J+1,bi,bj))
CML     &         + dwatPre(I-1,J,bi,bj) * 0.5 _d 0 *
CML     &          (vVel(I-1,J  ,k,bi,bj)-vIceC(I-1,J  ,bi,bj)
CML     &          +vVel(I-1,J+1,k,bi,bj)-vIceC(I-1,J+1,bi,bj))
CML     &         )
          rhsV(I,J,bi,bj)= rhsV(I,J,bi,bj) + FORCEY0(I,J,bi,bj)+
     &         0.5 _d 0 * ( dwatPre(I,J,bi,bj)+dwatPre(I,J-1,bi,bj) ) *
     &         COSWAT * vVel(I,J,k,bi,bj)
C     neglected for preconditioning step
CML     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
CML     &         ( dwatPre(I,J  ,bi,bj) * 0.5 _d 0 *
CML     &          (uVel(I  ,J  ,k,bi,bj)-uIceC(I  ,J  ,bi,bj)
CML     &          +uVel(I+1,J  ,k,bi,bj)-uIceC(I+1,J  ,bi,bj))
CML     &         + dwatPre(I,J-1,bi,bj) * 0.5 _d 0 *
CML     &          (uVel(I  ,J-1,k,bi,bj)-uIceC(I  ,J-1,bi,bj)
CML     &          +uVel(I+1,J-1,k,bi,bj)-uIceC(I+1,J-1,bi,bj))
CML     &         )
         ENDDO
        ENDDO
C     neglected for preconditioning step
CML        DO j=1,sNy
CML         DO i=1,sNx
CMLC-    add Coriolis term
CML          rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj) + HALF*
CML     &         ( seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
CML     &          *0.5 _d 0*( vIceC( i ,j,bi,bj)+vIceC( i ,j+1,bi,bj) )
CML     &         + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
CML     &          *0.5 _d 0*( vIceC(i-1,j,bi,bj)+vIceC(i-1,j+1,bi,bj) ) )
CML          rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj) - HALF*
CML     &         ( seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
CML     &         *0.5 _d 0*( uIceC(i  ,j  ,bi,bj)+uIceC(i+1,  j,bi,bj) )
CML     &         + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
CML     &         *0.5 _d 0*( uIceC(i  ,j-1,bi,bj)+uIceC(i+1,j-1,bi,bj) ) )
CML         ENDDO
CML        ENDDO
C      this is the rhs contribution of the time derivative
        DO j=1,sNy
         DO i=1,sNx
          rhsU(I,J,bi,bj)=rhsU(I,J,bi,bj)
     &         +seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *uIceNm1(i,j,bi,bj)
          rhsV(I,J,bi,bj)=rhsV(I,J,bi,bj)
     &         +seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *vIceNm1(i,j,bi,bj)
          rhsU(I,J,bi,bj)=rhsU(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          rhsV(I,J,bi,bj)=rhsV(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
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
        DO J=1,sNy+1
         DO I=1,sNx+1
          etaMeanZ (I,J,bi,bj) =
     &         (           ETA (I,J  ,bi,bj)  + ETA (I-1,J  ,bi,bj)
     &         +           ETA (I,J-1,bi,bj)  + ETA (I-1,J-1,bi,bj) )
     &         / MAX(1.D0,maskC(I,J,  k,bi,bj)+maskC(I-1,J,  k,bi,bj)
     &         +          maskC(I,J-1,k,bi,bj)+maskC(I-1,J-1,k,bi,bj) )
         ENDDO
        ENDDO
C     free-slip means no lateral stress, which is best achieved masking
C     eta on vorticity(=Z)-points; from now on we only need to worry
C     about the no-slip boundary conditions
        IF (.NOT.SEAICE_no_slip) THEN
         DO J=1,sNy+1
          DO I=1,sNx+1
           etaMeanZ (I,J,bi,bj) = etaMeanZ(I,J,bi,bj)
     &          *maskC(I,J,  k,bi,bj)*maskC(I-1,J,  k,bi,bj)
     &          *maskC(I,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
C     coefficients of uIce(I,J) and vIce(I,J) belonging to ...
        DO J=1,sNy
         DO I=0,sNx
C     ... d/dx (eta+zeta) d/dx u
          UXX(I,J,bi,bj) = _dyF(I,J,bi,bj) * etaPlusZeta(I,J,bi,bj)
     &         * _recip_dxF(I,J,bi,bj)
C     ... d/dx (zeta-eta) k1 u
          UXM(I,J,bi,bj) = _dyF(I,J,bi,bj) * zetaMinusEta(I,J,bi,bj)
     &         * k1AtC(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=1,sNy+1
         DO I=1,sNx
C     ... d/dy eta d/dy u
          UYY(I,J,bi,bj) = _dxV(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * _recip_dyU(I,J,bi,bj)
C     ... d/dy eta k2 u
          UYM(I,J,bi,bj) = _dxV(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * k2AtZ(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx+1
C     ... d/dx eta dv/dx
          VXX(I,J,bi,bj) = _dyU(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * _recip_dxV(I,J,bi,bj)
C     ... d/dx eta k1 v
          VXM(I,J,bi,bj) = _dyU(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
     &         * k1AtZ(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=0,sNy
         DO I=1,sNx
C     ... d/dy eta+zeta dv/dy
          VYY(I,J,bi,bj) = _dxF(I,J,bi,bj) * etaPlusZeta(I,J,bi,bj)
     &         * _recip_dyF(I,J,bi,bj)
C     ... d/dy (zeta-eta) k2 v
          VYM(I,J,bi,bj) = _dxF(I,J,bi,bj) * zetaMinusEta(I,J,bi,bj)
     &         * k2AtC(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Prepare for Solving uIce :

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     assemble coefficient matrix, beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of U(I,J+/-1) are counted on the right hand side
        DO J=1,sNy
         DO I=1,sNx
C     coefficients for UICE(I-1,J)
          AU(I,J,bi,bj)= ( - UXX(I-1,J,bi,bj) + UXM(I-1,J,bi,bj) )
     &         * seaiceMaskU(I,J,bi,bj)
C     coefficients for UICE(I+1,J)
          CU(I,J,bi,bj)= ( - UXX(I  ,J,bi,bj) - UXM(I  ,J,bi,bj) )
     &         * seaiceMaskU(I,J,bi,bj)
C     coefficients for UICE(I,J)
          BU(I,J,bi,bj)=(ONE - seaiceMaskU(I,J,bi,bj)) +
     &         ( UXX(I-1,J  ,bi,bj) + UXX(I,J,bi,bj)
     &         + UYY(I  ,J+1,bi,bj) + UYY(I,J,bi,bj)
     &         + UXM(I-1,J  ,bi,bj) - UXM(I,J,bi,bj)
     &         + UYM(I  ,J+1,bi,bj) - UYM(I,J,bi,bj)
     &         ) * seaiceMaskU(I,J,bi,bj)
C     coefficients of uIce(I,J-1)
          uRt1(I,J,bi,bj)= UYY(I,J  ,bi,bj) + UYM(I,J  ,bi,bj)
C     coefficients of uIce(I,J+1)
          uRt2(I,J,bi,bj)= UYY(I,J+1,bi,bj) - UYM(I,J+1,bi,bj)
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
C     for no slip, set u on boundary to zero: u(j+/-1)=-u(j)
C     for the free slip case sigma_12 = 0
        DO J=1,sNy
         DO I=1,sNx
          hFacM = seaiceMaskU(I,J-1,bi,bj)
          hFacP = seaiceMaskU(I,J+1,bi,bj)
C     copy contributions to coefficient of U(I,J)
C     beware of sign convection: uRt1/2 have the opposite sign convention
C     than BU, hence the minus sign
          BU(I,J,bi,bj)=BU(I,J,bi,bj) + seaiceMaskU(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM )
     &         * ( UYY(I  ,J  ,bi,bj) + UYM(I  ,J  ,bi,bj) )
     &         + ( 1. _d 0 - hFacP )
     &         * ( UYY(I  ,J+1,bi,bj) - UYM(I  ,J+1,bi,bj) ) )
C     reset coefficients of U(I,J-1) and U(I,J+1)
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * hFacM
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=1,sNy
         DO I=1,sNx
          AU(I,J,bi,bj)    = AU(I,J,bi,bj)    * recip_rAw(I,J,bi,bj)
          CU(I,J,bi,bj)    = CU(I,J,bi,bj)    * recip_rAw(I,J,bi,bj)
C     here we need ad in the contribution from the time derivative
C     and the symmetric drag term; must be done after normalizing
          BU(I,J,bi,bj)    = BU(I,J,bi,bj)    * recip_rAw(I,J,bi,bj)
     &         + seaiceMaskU(I,J,bi,bj) *
     &         ( seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I-1,J,bi,bj) ) )
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
         ENDDO
        ENDDO

C     now set up right-hand side
C     contribution of sigma11 to rhs
        DO J=1,sNy
         DO I=0,sNx
          sig11(I,J) = zetaMinusEta(I,J,bi,bj)
     &         * ( vIceC(I,J+1,bi,bj) - vIceC(I,J,bi,bj) )
     &         * _recip_dyF(I,J,bi,bj)
     &         + etaPlusZeta(I,J,bi,bj) * k2AtC(I,J,bi,bj)
     &         * 0.5 _d 0 * ( vIceC(I,J+1,bi,bj) + vIceC(I,J,bi,bj) )
     &         - 0.5 _d 0 * PRESS(I,J,bi,bj)
         ENDDO
        ENDDO
C     contribution of sigma12 to rhs of u-equation
        DO J=1,sNy+1
         DO I=1,sNx
          hFacM = seaiceMaskV(I,J,bi,bj) - seaiceMaskV(I-1,J,bi,bj)
          sig12(I,J) = etaMeanZ(I,J,bi,bj) * (
     &         ( vIceC(I,J,bi,bj) - vIceC(I-1,J,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj)
     &         - k1AtZ(I,J,bi,bj)
     &         * 0.5 _d 0 * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &         )
C     free slip conditions (sig12=0) are taken care of by masking sig12
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     no slip boundary conditions (v(i-1)=-v(i))
C     v(i)+v(i-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with v(i)-v(i-1)
     &         + etaMeanZ(I,J,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         * ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &         * hFacM * 2. _d 0
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
C     contribution to the rhs part of grad(sigma)_x
          rhsU(I,J,bi,bj) = rhsU(I,J,bi,bj)
     &         + recip_rAw(I,J,bi,bj) * seaiceMaskU(I,J,bi,bj) *
     &         ( _dyF(I  ,J  ,bi,bj)*sig11(I  ,J  )
     &         - _dyF(I-1,J  ,bi,bj)*sig11(I-1,J  )
     &         + _dxV(I  ,J+1,bi,bj)*sig12(I  ,J+1)
     &         - _dxV(I  ,J  ,bi,bj)*sig12(I  ,J  ) )
         ENDDO
        ENDDO

#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver to modify OB values:
        DO J=1,sNy
         DO I=1,sNx
          IF ( maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj) .EQ. 0. ) THEN
            AU(I,J,bi,bj) = ZERO
            BU(I,J,bi,bj) = ONE
            CU(I,J,bi,bj) = ZERO
            uRt1(I,J,bi,bj) = ZERO
            uRt2(I,J,bi,bj) = ZERO
            rhsU(I,J,bi,bj) = duIce(I,J,bi,bj)
          ENDIF
         ENDDO
        ENDDO
#endif /* OBCS_UVICE_OLD */

       ENDDO
      ENDDO

C--   Prepare for Solving dvIce :

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     assemble coefficient matrix, beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of U(I,J+/-1) are counted on the right hand side
        DO J=1,sNy
         DO I=1,sNx
C     coefficients for VICE(I,J-1)
          AV(I,J,bi,bj)=( - VYY(I,J-1,bi,bj) + VYM(I,J-1,bi,bj)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for VICE(I,J+1)
          CV(I,J,bi,bj)=( - VYY(I,J  ,bi,bj) - VYM(I,J  ,bi,bj)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for VICE(I,J)
          BV(I,J,bi,bj)= (ONE - seaiceMaskV(I,J,bi,bj)) +
     &         ( VXX(I,J,bi,bj) + VXX(I+1,J  ,bi,bj)
     &         + VYY(I,J,bi,bj) + VYY(I  ,J-1,bi,bj)
     &         - VXM(I,J,bi,bj) + VXM(I+1,J  ,bi,bj)
     &         - VYM(I,J,bi,bj) + VYM(I  ,J-1,bi,bj)
     &         ) * seaiceMaskV(I,J,bi,bj)
C     coefficients for V(I-1,J)
          vRt1(I,J,bi,bj) = VXX(I  ,J,bi,bj) + VXM(I  ,J,bi,bj)
C     coefficients for V(I+1,J)
          vRt2(I,J,bi,bj) = VXX(I+1,J,bi,bj) - VXM(I+1,J,bi,bj)
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
C     for no slip, set u on boundary to zero: v(i+/-1)=-v(i)
C     for the free slip case sigma_12 = 0
        DO J=1,sNy
         DO I=1,sNx
          hFacM = seaiceMaskV(i-1,j,bi,bj)
          hFacP = seaiceMaskV(i+1,j,bi,bj)
C     copy contributions to coefficient of V(I,J)
C     beware of sign convection: vRt1/2 have the opposite sign convention
C     than BV, hence the minus sign
          BV(I,J,bi,bj)=BV(I,J,bi,bj) + seaiceMaskV(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM )
     &         * ( VXX(I  ,J,bi,bj) + VXM(I  ,J,bi,bj) )
     &         + ( 1. _d 0 - hFacP )
     &         * ( VXX(I+1,J,bi,bj) - VXM(I+1,J,bi,bj) ) )
C     reset coefficients of V(I-1,J) and V(I+1,J)
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * hFacM
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=1,sNy
         DO I=1,sNx
          AV(I,J,bi,bj)    = AV(I,J,bi,bj)    * recip_rAs(I,J,bi,bj)
          CV(I,J,bi,bj)    = CV(I,J,bi,bj)    * recip_rAs(I,J,bi,bj)
C     here we need ad in the contribution from the time derivative
C     and the symmetric drag term; must be done after normalizing
          BV(I,J,bi,bj)    = BV(I,J,bi,bj)    * recip_rAs(I,J,bi,bj)
     &         + seaiceMaskV(I,J,bi,bj) *
     &         ( seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0 * ( DRAGS(I,J,bi,bj) + DRAGS(I,J-1,bi,bj) ) )
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
         ENDDO
        ENDDO

C     now set up right-hand-side
C     contribution of sigma22 to rhs
        DO J=0,sNy
         DO I=1,sNx
          sig22(I,J) =  zetaMinusEta(I,J,bi,bj)
     &         * ( uIceC(I+1,J,bi,bj) - uIceC(I,J,bi,bj) )
     &         * _recip_dxF(I,J,bi,bj)
     &         + etaPlusZeta(I,J,bi,bj) * k1AtC(I,J,bi,bj)
     &         * 0.5 _d 0 * ( uIceC(I+1,J,bi,bj) + uIceC(I,J,bi,bj) )
     &         - 0.5 _d 0 * PRESS(I,J,bi,bj)
         ENDDO
        ENDDO
C     contribution of sigma12 to rhs of v-equation
        DO J=1,sNy
         DO I=1,sNx+1
          hFacM = seaiceMaskU(i,j,bi,bj) - seaiceMaskU(i,j-1,bi,bj)
          sig12(I,J) = etaMeanZ(I,J,bi,bj) * (
     &         ( uIceC(I,J,bi,bj) - uIceC(I,J-1,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj)
     &         - k2AtZ(I,J,bi,bj)
     &         * 0.5 _d 0 * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &         )
C     free slip conditions (sig12=0) are taken care of by masking sig12,
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
C     no slip boundary conditions (u(j-1)=-u(j))
C     u(j)+u(j-1) = 0 is also taken care of by masking sig12, so that we
C     only need to deal with u(j)-u(j-1)
     &         + etaMeanZ(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &         * hFacM * 2. _d 0
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
C     contribution to the rhs part of grad(sigma)_y
          rhsV(I,J,bi,bj) = rhsV(I,J,bi,bj)
     &         + recip_rAs(I,J,bi,bj) * seaiceMaskV(I,J,bi,bj) *
     &         ( _dyU(I+1,J  ,bi,bj) * sig12(I+1,J  )
     &         - _dyU(I  ,J  ,bi,bj) * sig12(I  ,J  )
     &         + _dxF(I  ,J  ,bi,bj) * sig22(I  ,J  )
     &         - _dxF(I  ,J-1,bi,bj) * sig22(I  ,J-1) )
         ENDDO
        ENDDO

#ifndef OBCS_UVICE_OLD
C--     prevent tri-diagonal solver to modify OB values:
        DO J=1,sNy
         DO I=1,sNx
          IF ( maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj) .EQ. 0. ) THEN
            AV(I,J,bi,bj) = ZERO
            BV(I,J,bi,bj) = ONE
            CV(I,J,bi,bj) = ZERO
            vRt1(I,J,bi,bj) = ZERO
            vRt2(I,J,bi,bj) = ZERO
            rhsV(I,J,bi,bj) = dvIce(I,J,bi,bj)
          ENDIF
         ENDDO
        ENDDO
#endif /* OBCS_UVICE_OLD */

       ENDDO
      ENDDO

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
      IF ( printResidual .OR. LSR_mixIniGuess.GE.1 ) THEN
        CALL SEAICE_RESIDUAL(
     I                  rhsU, rhsV, uRt1, uRt2, vRt1, vRt2,
     I                  AU, BU, CU, AV, BV, CV, duIce, dvIce,
     U                  massU, massV,
     O                  residUini, residVini, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      doIterate4u = .TRUE.
      doIterate4v = .TRUE.

C ITERATION START -----------------------------------------------------

      SOLV_MAX_TMP = SOLV_MAX_ITERS

      ICOUNT1 = SOLV_MAX_TMP
      ICOUNT2 = SOLV_MAX_TMP

      DO m = 1, SOLV_MAX_TMP

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

C-jmc: get less TAF warnings when always (no if doIterate) saving duIce,dvIce:
C     save duIce prior to iteration, NOW SET U(3)=U(1)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uTmp(I,J,bi,bj)=duIce(I,J,bi,bj)
           ENDDO
          ENDDO
C     save dvIce prior to iteration, NOW SET V(3)=V(1)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vTmp(I,J,bi,bj)=dvIce(I,J,bi,bj)
           ENDDO
          ENDDO

          IF ( doIterate4u ) THEN
C Solve for uIce :
           DO J=1,sNy
            DO I=1,sNx
             AA3 = ZERO
             IF (I.EQ.1)   AA3 = AA3 - AU(I,J,bi,bj)*duIce(I-1,J,bi,bj)
             IF (I.EQ.sNx) AA3 = AA3 - CU(I,J,bi,bj)*duIce(I+1,J,bi,bj)

             URT(I,J)=rhsU(I,J,bi,bj)
     &            + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &            + uRt1(I,J,bi,bj)*uTmp(I,J-1,bi,bj)
     &            + uRt2(I,J,bi,bj)*uTmp(I,J+1,bi,bj)
#else
     &            + uRt1(I,J,bi,bj)*duIce(I,J-1,bi,bj)
     &            + uRt2(I,J,bi,bj)*duIce(I,J+1,bi,bj)
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
     &           +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
            ENDDO
           ENDDO
C--   end doIterate4u
          ENDIF

          IF ( doIterate4v ) THEN
C Solve for dvIce
           DO I=1,sNx
            DO J=1,sNy
             AA3 = ZERO
             IF (J.EQ.1)   AA3 = AA3 - AV(I,J,bi,bj)*dvIce(I,J-1,bi,bj)
             IF (J.EQ.sNy) AA3 = AA3 - CV(I,J,bi,bj)*dvIce(I,J+1,bi,bj)

             VRT(I,J)=rhsV(I,J,bi,bj)
     &            + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &            + vRt1(I,J,bi,bj)*vTmp(I-1,J,bi,bj)
     &            + vRt2(I,J,bi,bj)*vTmp(I+1,J,bi,bj)
#else
     &            + vRt1(I,J,bi,bj)*dvIce(I-1,J,bi,bj)
     &            + vRt2(I,J,bi,bj)*dvIce(I+1,J,bi,bj)
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
     &            /(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
            ENDDO
            DO J=1,sNy-1
             J1=sNy-J
             J2=J1+1
             VRT(I,J1)=VRT(I,J1)-CVV(I,J1)*VRT(I,J2)
            ENDDO
            DO J=1,sNy
             dvIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &           +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
            ENDDO
           ENDDO
C--   end doIterate4v
          ENDIF

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
     U                  massU, massV,
     O                  residUend, residVend, uTmp, vTmp,
     I                  printResidual, myIter, myThid )
        _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(A,1X,I6,1P3E16.8)')
     &       ' SEAICE_PRECONDITIONER: Residual Initial Uice,Vice=',
     &       residUini, residVini
        WRITE(standardMessageUnit,'(A,I4,A,I4,A,I6,1P2E16.8)')
     &       ' SEAICE_PRECONDITIONER (iter=',newtonIter,',',
     &       krylovIter, ') iters,dU,Resid=', iCount1, S1, residUend
        WRITE(standardMessageUnit,'(A,I4,A,I4,A,I6,1P2E16.8)')
     &       ' SEAICE_PRECONDITIONER (iter=',newtonIter,',',
     &       krylovIter, ') iters,dU,Resid=', iCount2, S2, residVend
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

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
