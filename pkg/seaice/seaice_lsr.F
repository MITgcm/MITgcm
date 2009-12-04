C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_lsr.F,v 1.54 2009/12/04 11:10:50 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_LSR( ilcall, myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE  SEAICE_LSR                                   |
C     | o Solve ice momentum equation with an LSR dynamics solver|
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997     |
C     |    and Zhang and Rothrock, MWR, 131,  845- 861, 2003)    |
C     |   Written by Jinlun Zhang, PSC/UW, Feb-2001              |
C     |                     zhang@apl.washington.edu             |
C     |==========================================================|
C     | C-grid version by Martin Losch                           |
C     | Since 2009/03/18: finite-Volume discretization of stress |
C     | divergence that includes all metric terms                |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      INTEGER ilcall
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS

#ifndef SEAICE_OLD_AND_BAD_DISCRETIZATION
C     |==========================================================|
C     | C-grid version by Martin Losch                           |
C     | Since 2009/03/18: finite-Volume discretization of stress |
C     | divergence that includes all metric terms                |
C     \==========================================================/

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, k, m, bi, bj, j1, j2, im, jm
      INTEGER ICOUNT1, ICOUNT2
      INTEGER kSrf
      INTEGER phexit

      _RL WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL AA3, S1, S2, S1A, S2A
      _RL hFacM, hFacP
      _RL eplus, eminus

C     coefficients of ice velocities in coefficient matrix
C     for both U and V-equation
C     XX: double derivative in X
C     YY: double derivative in Y
C     XM: metric term with derivative in X
C     YM: metric term with derivative in Y
      _RL UXX  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UYY  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UXM  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UYM  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL VXX  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL VYY  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL VXM  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL VYM  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     diagonals of coefficient matrices
      _RL AU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL BU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL CU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL AV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL BV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL CV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL FXY  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     coefficients for lateral points (u(j+/-1),v(i+/-1))
      _RL UVRT1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UVRT2(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     abbreviations
      _RL etaPlusZeta (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zetaMinusEta(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanZ    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     contribution of sigma on righ hand side
      _RL sig11(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sig22(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sig12(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
C     auxillary fields
      _RL URT  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL CUU  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL VRT  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL CVV  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL uTmp (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL vTmp (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C
      _RL COSWAT
      _RS SINWAT
      _RL TEMPVAR
      _RL UERR
      INTEGER iMin, iMax, jMin, jMax
#ifdef SEAICE_VECTORIZE_LSR
C     in this case, the copy of u(3)=u(1)/v(3)=v(1) needs to include
C     part of the overlap, because the overlap of u/vTmp is used
      PARAMETER ( iMin = 0, iMax = sNx+1, jMin = 0, jMax = sNy+1 )
#else
      PARAMETER ( iMin = 1, iMax = sNx, jMin = 1, jMax = sNy )
#endif
#ifdef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE
      _RL resnorm, EKnorm, counter
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */

C     surrface level
      kSrf = 1
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

      ICOUNT1=SOLV_MAX_ITERS
      ICOUNT2=SOLV_MAX_ITERS

      k = 1

      IF ( ilcall .EQ. 1 ) THEN
C NOW DO PREDICTOR TIME STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(i,j,bi,bj)=uIce(I,J,bi,bj)
           vIceNm1(i,j,bi,bj)=vIce(I,J,bi,bj)
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE uicec = comlev1_dynsol, kind=isbyte,
cphCADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
cphCADJ STORE vicec = comlev1_dynsol, kind=isbyte,
cphCADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif
      ELSE
C NOW DO MODIFIED EULER STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(I,J,bi,bj)=HALF*(uIce(I,J,bi,bj)+uIceNm1(i,j,bi,bj))
           vIce(I,J,bi,bj)=HALF*(vIce(I,J,bi,bj)+vIceNm1(i,j,bi,bj))
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( ilcall .GT. 2 ) THEN
C     for additional (pseudo-time)steps update u/vIceNm1
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(i,j,bi,bj)=uIce(I,J,bi,bj)
           vIceNm1(i,j,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
cph That's an important one! Note, that
cph * lsr is called twice, thus the icall index
cph * this storing is still outside the iteration loop
CADJ STORE uice = comlev1_dynsol, kind=isbyte,
CADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
CADJ STORE vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif /* ALLOW_AUTODIFF_TAMC */

      CALL SEAICE_CALC_STRAINRATES(
     I     uIceC, vIceC,
     O     e11, e22, e12,
     I     ilcall, myTime, myIter, myThid )

      CALL SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, hEffM, press0,
     O     eta, zeta, press,
     I     ilcall, myTime, myIter, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy+1
         DO i=0,sNx+1
C     set up non-linear water drag
          TEMPVAR = QUART*(
     &          (uIceC(I  ,J,bi,bj)-uVel(I  ,J,kSrf,bi,bj)
     &          +uIceC(I+1,J,bi,bj)-uVel(I+1,J,kSrf,bi,bj))**2
     &         +(vIceC(I,J  ,bi,bj)-vVel(I,J  ,kSrf,bi,bj)
     &          +vIceC(I,J+1,bi,bj)-vVel(I,J+1,kSrf,bi,bj))**2)
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag_south)**2 ) THEN
            DWATN(I,J,bi,bj)=QUART
           ELSE
            DWATN(I,J,bi,bj)=SEAICE_waterDrag_south*SQRT(TEMPVAR)
           ENDIF
          ELSE
           IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag)**2 ) THEN
            DWATN(I,J,bi,bj)=QUART
           ELSE
            DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
           ENDIF
          ENDIF
          DWATN(I,J,bi,bj) = DWATN(I,J,bi,bj) * HEFFM(I,J,bi,bj)
C     set up symmettric drag
          DRAGS(I,J,bi,bj) = DWATN(I,J,bi,bj)*COSWAT
         ENDDO
        ENDDO
C
        DO J=1,sNy
         DO I=1,sNx
C     set up anti symmettric drag force and add in current force
C     ( remember to average to correct velocity points )
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * uVel(I,J,kSrf,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &          (vVel(I  ,J  ,kSrf,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &          +vVel(I  ,J+1,kSrf,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &         + DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &          (vVel(I-1,J  ,kSrf,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &          +vVel(I-1,J+1,kSrf,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &         )
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * vVel(I,J,kSrf,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &          (uVel(I  ,J  ,kSrf,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &          +uVel(I+1,J  ,kSrf,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &         + DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &          (uVel(I  ,J-1,kSrf,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &          +uVel(I+1,J-1,kSrf,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &         )
         ENDDO
        ENDDO
C      this is the rhs contribution of the time derivative
        DO j=1,sNy
         DO i=1,sNx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         +seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *uIceNm1(i,j,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *vIceNm1(i,j,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
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
          etaPlusZeta (I,J,bi,bj) = ETA (I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA (I,J,bi,bj)
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

C SOLVE FOR uIce
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
          UVRT1(I,J,bi,bj)= UYY(I,J  ,bi,bj) + UYM(I,J  ,bi,bj)
C     coefficients of uIce(I,J+1)
          UVRT2(I,J,bi,bj)= UYY(I,J+1,bi,bj) - UYM(I,J+1,bi,bj)
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
C     beware of sign convection: UVRT1/2 have the opposite sign convention
C     than BU, hence the minus sign
          BU(I,J,bi,bj)=BU(I,J,bi,bj) + seaiceMaskU(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM )
     &         * ( UYY(I  ,J  ,bi,bj) + UYM(I  ,J  ,bi,bj) )
     &         + ( 1. _d 0 - hFacP )
     &         * ( UYY(I  ,J+1,bi,bj) - UYM(I  ,J+1,bi,bj) ) )
C     reset coefficients of U(I,J-1) and U(I,J+1)
          UVRT1(I,J,bi,bj)=UVRT1(I,J,bi,bj) * hFacM
          UVRT2(I,J,bi,bj)=UVRT2(I,J,bi,bj) * hFacP
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
          UVRT1(I,J,bi,bj) = UVRT1(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
          UVRT2(I,J,bi,bj) = UVRT2(I,J,bi,bj) * recip_rAw(I,J,bi,bj)
         ENDDO
        ENDDO

        DO J=1,sNy
         AU(1,J,bi,bj)=ZERO
         CU(sNx,J,bi,bj)=ZERO
         CU(1,J,bi,bj)=CU(1,J,bi,bj)/BU(1,J,bi,bj)
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
C     coriolis and other forcing
          FXY(I,J,bi,bj)=0.5 _d 0 *
     &         ( seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &          *0.5 _d 0*( vIceC( i ,j,bi,bj)+vIceC( i ,j+1,bi,bj) )
     &         + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &          *0.5 _d 0*( vIceC(i-1,j,bi,bj)+vIceC(i-1,j+1,bi,bj) ) )
     &         +FORCEX(I,J,bi,bj)
C     contribution to the rhs part of grad(sigma)_x
     &         + recip_rAw(I,J,bi,bj) * seaiceMaskU(I,J,bi,bj) *
     &         ( _dyF(I  ,J  ,bi,bj)*sig11(I  ,J  )
     &         - _dyF(I-1,J  ,bi,bj)*sig11(I-1,J  )
     &         + _dxV(I  ,J+1,bi,bj)*sig12(I  ,J+1)
     &         - _dxV(I  ,J  ,bi,bj)*sig12(I  ,J  ) )
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
        CALL DEBUG_STATS_RL(1,UICE,'UICE before iter. (SEAICE_LSR)',
     &                      myThid)
      ENDIF
#endif /* ALLOW_DEBUG */
C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      phexit = -1

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration uice
#endif /* ALLOW_AUTODIFF_TAMC */
      DO M=1, solv_max_iters
      IF ( phexit .EQ. -1 ) THEN

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW SET U(3)=U(1)
C     save uIce prior to iteration
        DO J=jMin,jMax
         DO I=1,sNx
          uTmp(I,J,bi,bj)=uIce(I,J,bi,bj)
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
          IF(I.EQ.1) THEN
           AA3=( UXX(I-1,J,bi,bj) - UXM(I-1,J,bi,bj) )
     &          * uIce(I-1,J,bi,bj) * seaiceMaskU(I,J,bi,bj)
          ELSEIF(I.EQ.sNx) THEN
           AA3=( UXX(I  ,J,bi,bj) + UXM(I  ,J,bi,bj) )
     &          * uIce(I+1,J,bi,bj) * seaiceMaskU(I,J,bi,bj)
          ELSE
           AA3=ZERO
          ENDIF
          URT(I,J)=FXY(I,J,bi,bj)
     &         + AA3 * recip_rAw(I,J,bi,bj)
#ifdef SEAICE_VECTORIZE_LSR
     &         + UVRT1(I,J,bi,bj)*uTmp(I,J-1,bi,bj)
     &         + UVRT2(I,J,bi,bj)*uTmp(I,J+1,bi,bj)
#else
     &         + UVRT1(I,J,bi,bj)*uIce(I,J-1,bi,bj)
     &         + UVRT2(I,J,bi,bj)*uIce(I,J+1,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
          URT(I,J)=URT(I,J)* seaiceMaskU(I,J,bi,bj)
         ENDDO

         DO I=1,sNx
          CUU(I,J)=CU(I,J,bi,bj)
         ENDDO
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
     &        /(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J))
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
          uIce(I,J,bi,bj)=uTmp(I,J,bi,bj)
     &        +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
         ENDDO
        ENDDO
C     end bi,bj-loops
       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       S1=ZERO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           UERR=(uIce(I,J,bi,bj)-uTmp(I,J,bi,bj))
     &             * seaiceMaskU(I,J,bi,bj)
           S1=MAX(ABS(UERR),S1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_MAX_RL( S1, myThid )
c       WRITE(standardMessageUnit,'(A,2I6,1P4E16.9)')
c    &   ' U iters,error,WF = ',ilcall,M,S1,S1A,WFAU
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S1.GT.S1A) WFAU=WFAU2
       S1A=S1
       IF(S1.LT.LSR_ERROR) THEN
        ICOUNT1=M
        phexit = 1
       ENDIF
      ENDIF
      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

      ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(A,I6,1P2E22.14)')
     &      ' U lsr iters, error = ',ICOUNT1,S1
       _END_MASTER( myThid )
        CALL DEBUG_STATS_RL(1,UICE,'UICE after  iter. (SEAICE_LSR)',
     &                      myThid)
        IF (S1.EQ.0.D0.AND.ICOUNT1.GT.SOLV_NCHECK)
     &   STOP 'ABNORMAL END: S/R SEAICE_LSR did not converge (uIce)'
      ENDIF
#endif /* ALLOW_DEBUG */

C NOW FOR vIce
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
          UVRT1(I,J,bi,bj) = VXX(I  ,J,bi,bj) + VXM(I  ,J,bi,bj)
C     coefficients for V(I+1,J)
          UVRT2(I,J,bi,bj) = VXX(I+1,J,bi,bj) - VXM(I+1,J,bi,bj)
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
C     beware of sign convection: UVRT1/2 have the opposite sign convention
C     than BV, hence the minus sign
          BV(I,J,bi,bj)=BV(I,J,bi,bj) + seaiceMaskV(I,J,bi,bj) *
     &         ( ( 1. _d 0 - hFacM )
     &         * ( VXX(I  ,J,bi,bj) + VXM(I  ,J,bi,bj) )
     &         + ( 1. _d 0 - hFacP )
     &         * ( VXX(I+1,J,bi,bj) - VXM(I+1,J,bi,bj) ) )
C     reset coefficients of V(I-1,J) and V(I+1,J)
          UVRT1(I,J,bi,bj)=UVRT1(I,J,bi,bj) * hFacM
          UVRT2(I,J,bi,bj)=UVRT2(I,J,bi,bj) * hFacP
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
          UVRT1(I,J,bi,bj) = UVRT1(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
          UVRT2(I,J,bi,bj) = UVRT2(I,J,bi,bj) * recip_rAs(I,J,bi,bj)
         ENDDO
        ENDDO

        DO I=1,sNx
         AV(I,1,bi,bj)=ZERO
         CV(I,sNy,bi,bj)=ZERO
         CV(I,1,bi,bj)=CV(I,1,bi,bj)/BV(I,1,bi,bj)
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
C     coriols and other foring
          FXY(I,J,bi,bj)= - 0.5 _d 0 *
     &         ( seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &         *0.5 _d 0*( uIceC(i  ,j  ,bi,bj)+uIceC(i+1,  j,bi,bj) )
     &         + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &         *0.5 _d 0*( uIceC(i  ,j-1,bi,bj)+uIceC(i+1,j-1,bi,bj) ) )
     &         + FORCEY(I,J,bi,bj)
C     contribution to the rhs part of grad(sigma)_y
     &         + recip_rAs(I,J,bi,bj) * seaiceMaskV(I,J,bi,bj) *
     &         ( _dyU(I+1,J  ,bi,bj) * sig12(I+1,J  )
     &         - _dyU(I  ,J  ,bi,bj) * sig12(I  ,J  )
     &         + _dxF(I  ,J  ,bi,bj) * sig22(I  ,J  )
     &         - _dxF(I  ,J-1,bi,bj) * sig22(I  ,J-1) )
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
        CALL DEBUG_STATS_RL(1,VICE,'VICE before iter. (SEAICE_LSR)',
     &                      myThid)
      ENDIF
#endif /* ALLOW_DEBUG */

C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      phexit = -1

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration vice
#endif /* ALLOW_AUTODIFF_TAMC */
      DO M=1, solv_max_iters
      IF ( phexit .EQ. -1 ) THEN

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW SET V(3)=V(1)
C     save vIce prior to iteration
        DO J=1,sNy
         DO I=iMin,iMax
          vTmp(I,J,bi,bj)=vIce(I,J,bi,bj)
         ENDDO
        ENDDO

        DO I=1,sNx
         DO J=1,sNy
          IF(J.EQ.1) THEN
           AA3=( VYY(I,J-1,bi,bj) - VYM(I,J-1,bi,bj)
     &          ) * vIce(I,J-1,bi,bj) * seaiceMaskV(I,J,bi,bj)
          ELSEIF(J.EQ.sNy) THEN
           AA3=( VYY(I,J  ,bi,bj) + VYM(I,J  ,bi,bj)
     &          ) * vIce(I,J+1,bi,bj) * seaiceMaskV(I,J,bi,bj)
          ELSE
           AA3=ZERO
          ENDIF

          VRT(I,J)=FXY(I,J,bi,bj)
     &         + AA3 * recip_rAs(I,J,bi,bj)
#ifdef SEAICE_VECTORIZE_LSR
     &         + UVRT1(I,J,bi,bj)*vTmp(I-1,J,bi,bj)
     &         + UVRT2(I,J,bi,bj)*vTmp(I+1,J,bi,bj)
#else
     &         + UVRT1(I,J,bi,bj)*vIce(I-1,J,bi,bj)
     &         + UVRT2(I,J,bi,bj)*vIce(I+1,J,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
          VRT(I,J)=VRT(I,J)* seaiceMaskV(I,J,bi,bj)
         ENDDO

         DO J=1,sNy
          CVV(I,J)=CV(I,J,bi,bj)
         ENDDO
         VRT(I,1)=VRT(I,1)/BV(I,1,bi,bj)
         DO J=2,sNy
          JM=J-1
          CVV(I,J)=CVV(I,J)/(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
          VRT(I,J)=(VRT(I,J)-AV(I,J,bi,bj)*VRT(I,JM))
     &         /(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
         ENDDO
         DO J=1,sNy-1
          J1=sNy-J
          J2=J1+1
          VRT(I,J1)=VRT(I,J1)-CVV(I,J1)*VRT(I,J2)
         ENDDO
         DO J=1,sNy
          vIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &        +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
         ENDDO
        ENDDO
C     end bi,bj-loops
       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       S2=ZERO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           UERR=(vIce(I,J,bi,bj)-vTmp(I,J,bi,bj))
     &             * seaiceMaskV(I,J,bi,bj)
           S2=MAX(ABS(UERR),S2)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_MAX_RL( S2, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S2.GT.S2A) WFAV=WFAV2
       S2A=S2
       IF(S2.LT.LSR_ERROR) THEN
        ICOUNT2=M
        phexit = 1
       ENDIF
      ENDIF
      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

      ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(A,I6,1P2E22.14)')
     &      ' V lsr iters, error = ',ICOUNT2,S2
       _END_MASTER( myThid )
        CALL DEBUG_STATS_RL(1,VICE,'VICE after  iter. (SEAICE_LSR)',
     &                      myThid)
        IF (S2.EQ.0.D0.AND.ICOUNT2.GT.SOLV_NCHECK)
     &   STOP 'ABNORMAL END: S/R SEAICE_LSR did not converge (vIce)'
      ENDIF
#endif /* ALLOW_DEBUG */

C     APPLY MASKS
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          uIce(I,J,bi,bj)=uIce(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          vIce(I,J,bi,bj)=vIce(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
CML      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

#else /* ifdef SEAICE_OLD_AND_BAD_DISCRETIZATION */
C     /==========================================================\
C     | C-grid version by Martin Losch                           |
C     | in this version, only metric terms for spherical polar   |
C     | grids are taken into account, therefore we can use       |
C     | tanPhiAtC = tanPhiAtU and tanPhiAtZ = tanPhiAtV          |
C     | in future versions this hack will be removed             |
C     \==========================================================/

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, k, m, bi, bj, j1, j2, im, jm
      INTEGER ICOUNT1, ICOUNT2
      INTEGER phexit

      _RL WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL AA1, AA2, AA3, AA4, AA5, AA6, AA7, AA8, S1, S2, S1A, S2A
      _RL seaiceSlipFactor, hFacM, hFacP
      _RL eplus, eminus
      _RL sig11       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sig22       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sig12       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      _RL AU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL BU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL CU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL AV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL BV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL CV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL FXY  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL URT(1-Olx:sNx+Olx, 1-Oly:sNy+Oly)
      _RL CUU(1-Olx:sNx+Olx, 1-Oly:sNy+Oly)
      _RL VRT(1-Olx:sNx+Olx, 1-Oly:sNy+Oly)
      _RL CVV(1-Olx:sNx+Olx, 1-Oly:sNy+Oly)
C
      _RL etaPlusZeta (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zetaMinusEta(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanZ    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanU    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanV    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL UVRT1    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UVRT2    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL uTmp     (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL vTmp     (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL dUdx     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dUdy     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dVdx     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dVdy     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL uMeanZ   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      _RL COSWAT
      _RS SINWAT
      _RL TEMPVAR
      _RL UERR
      INTEGER iMin, iMax, jMin, jMax
#ifdef SEAICE_VECTORIZE_LSR
C     in this case, the copy of u(3)=u(1)/v(3)=v(1) needs to include
C     part of the overlap, because the overlap of u/vTmp is used
      PARAMETER ( iMin = 0, iMax = sNx+1, jMin = 0, jMax = sNy+1 )
#else
      PARAMETER ( iMin = 1, iMax = sNx, jMin = 1, jMax = sNy )
#endif
#ifdef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE
      _RL resnorm, EKnorm, counter
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */

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

      ICOUNT1=SOLV_MAX_ITERS
      ICOUNT2=SOLV_MAX_ITERS

      k = 1

      IF ( ilcall .EQ. 1 ) THEN
C NOW DO PREDICTOR TIME STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(i,j,bi,bj)=uIce(I,J,bi,bj)
           vIceNm1(i,j,bi,bj)=vIce(I,J,bi,bj)
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uicec = comlev1_dynsol, kind=isbyte,
CADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
CADJ STORE vicec = comlev1_dynsol, kind=isbyte,
CADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif
      ELSE
C NOW DO MODIFIED EULER STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(I,J,bi,bj)=HALF*(uIce(I,J,bi,bj)+uIceNm1(i,j,bi,bj))
           vIce(I,J,bi,bj)=HALF*(vIce(I,J,bi,bj)+vIceNm1(i,j,bi,bj))
           uIceC(I,J,bi,bj)=uIce(I,J,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C     for additional (pseudo-time)steps update u/vIceNm1
      IF ( ilcall .GT. 2 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIceNm1(i,j,bi,bj)=uIce(I,J,bi,bj)
           vIceNm1(i,j,bi,bj)=vIce(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
cph That's an important one! Note, that
cph * lsr is called twice, thus the icall index
cph * this storing is still outside the iteration loop
CADJ STORE uice = comlev1_dynsol, kind=isbyte,
CADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
CADJ STORE vice = comlev1_dynsol, kind=isbyte,
CADJ &     key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif /* ALLOW_AUTODIFF_TAMC */

      IF ( SEAICE_no_Slip ) THEN
C     no slip
       seaiceSlipFactor = - 1. _d 0
      ELSE
C     free slip
       seaiceSlipFactor =   1. _d 0
      ENDIF

      CALL SEAICE_CALC_STRAINRATES(
     I     uIceC, vIceC,
     O     e11, e22, e12,
     I     ilcall, myTime, myIter, myThid )

      CALL SEAICE_CALC_VISCOSITIES(
     I     e11, e22, e12, zMin, zMax, hEffM, press0,
     O     eta, zeta, press,
     I     ilcall, myTime, myIter, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy+1
         DO i=0,sNx+1
C     set up non-linear water drag
          TEMPVAR = QUART*(
     &          (uIceC(I  ,J,bi,bj)-uVel(I  ,J,1,bi,bj)
     &          +uIceC(I+1,J,bi,bj)-uVel(I+1,J,1,bi,bj))**2
     &         +(vIceC(I,J  ,bi,bj)-vVel(I,J  ,1,bi,bj)
     &          +vIceC(I,J+1,bi,bj)-vVel(I,J+1,1,bi,bj))**2)
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag_south)**2 ) THEN
            DWATN(I,J,bi,bj)=QUART
           ELSE
            DWATN(I,J,bi,bj)=SEAICE_waterDrag_south*SQRT(TEMPVAR)
           ENDIF
          ELSE
           IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag)**2 ) THEN
            DWATN(I,J,bi,bj)=QUART
           ELSE
            DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
           ENDIF
          ENDIF
          DWATN(I,J,bi,bj) = DWATN(I,J,bi,bj) * HEFFM(I,J,bi,bj)
C     set up symmettric drag
          DRAGS(I,J,bi,bj) = DWATN(I,J,bi,bj)*COSWAT
         ENDDO
        ENDDO
C
        DO J=1,sNy
         DO I=1,sNx
C     set up anti symmettric drag force and add in current force
C     ( remember to average to correct velocity points )
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * uVel(I,J,1,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) *
     &         0.5 _d 0 * (vVel(I  ,J  ,1,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &                    +vVel(I  ,J+1,1,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &         + DWATN(I-1,J,bi,bj) *
     &         0.5 _d 0 * (vVel(I-1,J  ,1,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &                    +vVel(I-1,J+1,1,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &         )
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * vVel(I,J,1,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) *
     &         0.5 _d 0 * (uVel(I  ,J  ,1,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &                    +uVel(I+1,J  ,1,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &         + DWATN(I,J-1,bi,bj) *
     &         0.5 _d 0 * (uVel(I  ,J-1,1,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &                    +uVel(I+1,J-1,1,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &         )
C     calculate pressure force and add to external force
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         - 0.5 _d 0 *  _recip_dxC(I,J,bi,bj)
     &         *(PRESS(I,J,bi,bj) - PRESS(I-1,J  ,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         - 0.5 _d 0 *  _recip_dyC(I,J,bi,bj)
     &         *(PRESS(I,J,bi,bj) - PRESS(I  ,J-1,bi,bj))
         ENDDO
        ENDDO
C      this is the rhs contribution of the time derivative
        DO j=1,sNy
         DO i=1,sNx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         +seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *uIceNm1(i,j,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *vIceNm1(i,j,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
C     some abbreviations
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy
         DO i=0,sNx
          etaPlusZeta (I,J,bi,bj) = ETA (I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA (I,J,bi,bj)
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
          etaMeanU (I,J,bi,bj) =
     &         HALF*(ETA (I,J,bi,bj) + ETA (I-1,J  ,bi,bj))
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
          etaMeanV (I,J,bi,bj) =
     &         HALF*(ETA (I,J,bi,bj) + ETA (I  ,J-1,bi,bj))
         ENDDO
        ENDDO
        DO j=1,sNy+1
         DO i=1,sNx+1
          etaMeanZ (I,J,bi,bj) =
     &         (           ETA (I,J  ,bi,bj)  + ETA (I-1,J  ,bi,bj)
     &         +           ETA (I,J-1,bi,bj)  + ETA (I-1,J-1,bi,bj) )
     &         / MAX(1.D0,maskC(I,J,  k,bi,bj)+maskC(I-1,J,  k,bi,bj)
     &         +          maskC(I,J-1,k,bi,bj)+maskC(I-1,J-1,k,bi,bj) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C SOLVE FOR uIce
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
C     coefficients of uIce(I,J)
C     (d/dx)[(eta+zeta)*d/dx)] U
          AA1 = etaPlusZeta(I  ,J,bi,bj)
     &         * _recip_dxF(I  ,J,bi,bj)
     &         * _recip_dxC(I  ,J,bi,bj) * seaiceMaskU(I,J,bi,bj)
          AA2 = etaPlusZeta(I-1,J,bi,bj)
     &         * _recip_dxF(I-1,J,bi,bj)
     &         * _recip_dxC(I  ,J,bi,bj) * seaiceMaskU(I,J,bi,bj)
C     (d/dy)[eta*(d/dy)] U (also on UVRT1/2)
          AA3 = etaMeanZ(I,J  ,bi,bj) * _recip_dyU(I,J  ,bi,bj)
     &         * _recip_dyG(I,J,bi,bj)
          AA4 = etaMeanZ(I,J+1,bi,bj) * _recip_dyU(I,J+1,bi,bj)
     &         * _recip_dyG(I,J,bi,bj)
C     (d/dy)[eta*tanphi/a)] U (also on UVRT1/2)
          AA5 = etaMeanZ(I,J  ,bi,bj) * _recip_dyG(I,J,bi,bj)
     &         * 0.5 _d 0 * _tanPhiAtV(I,J,  bi,bj) * recip_rSphere
          AA6 = etaMeanZ(I,J+1,bi,bj) * _recip_dyG(I,J,bi,bj)
     &         * 0.5 _d 0 * _tanPhiAtV(I,J+1,bi,bj) * recip_rSphere
C     2*eta*(tanphi/a) * ( tanphi/a ) U
          AA7 = TWO*etaMeanU(I,J,bi,bj)*recip_rSphere*recip_rSphere
     &         * _tanPhiAtU(I,J,bi,bj)  * _tanPhiAtU(I,J,bi,bj)
C     - 2*eta*(tanphi/a) * (d/dy) U
          AA8 = - TWO*etaMeanU(I,J,bi,bj) * _tanPhiAtU(I,J,bi,bj)
     &         *recip_rSphere
     &         * 1.0 _d 0 / ( _dyU(I,J,bi,bj) + _dyU(I,J+1,bi,bj) )
CMLCML  now we discretize U * (d/dy) eta*(tanphi/a) - eta*(tanphi/a) * (d/dy) U
CMLCML  instead of (d/dy)[eta*tanphi/a)] U - 2*eta*(tanphi/a) * (d/dy) U
CMLC     (d/dy)[eta*tanphi/a)] U (also on UVRT1/2)
CML          AA5 = 0. _d 0
CML          AA6 = 0. _d 0
CMLC     U * (d/dy) eta*(tanphi/a)
CML          AA7 = AA7 +
CML     &         ( etaMeanZ(I,J+1,bi,bj) * _tanPhiAtV(I,J+1,bi,bj)
CML     &         - etaMeanZ(I,J  ,bi,bj) * _tanPhiAtV(I,J  ,bi,bj) )
CML     &         * _recip_dyG(I,J,bi,bj) * recip_rSphere
CMLC     - eta*(tanphi/a) * (d/dy) U
CML          AA8 = - etaMeanU(I,J,bi,bj) * _tanPhiAtU(I,J,bi,bj)
CML     &         *recip_rSphere
CML     &         * 1.0 _d 0 / ( _dyU(I,J,bi,bj) + _dyU(I,J+1,bi,bj) )
C
          AU(I,J,bi,bj)=-AA2
          CU(I,J,bi,bj)=-AA1
          BU(I,J,bi,bj)=(ONE - seaiceMaskU(I,J,bi,bj))
     &         - AU(I,J,bi,bj) - CU(I,J,bi,bj)
     &         + ( (AA4+AA3) - (AA6-AA5) + AA7
     &         + seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I-1,J,bi,bj) )
     &         )* seaiceMaskU(I,J,bi,bj)
C     coefficients of uIce(I,J-1)
          UVRT1(I,J,bi,bj)= AA3 - AA5 - AA8
C     coefficients of uIce(I,J+1)
          UVRT2(I,J,bi,bj)= AA4 + AA6 + AA8
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
        DO J=1,sNy
         DO I=1,sNx
          hFacM = seaiceMaskU(i,j-1,bi,bj)
          hFacP = seaiceMaskU(i,j+1,bi,bj)
C     copy contributions to coefficient of U(I,J)
C     beware of sign convection: UVRT1/2 have the opposite sign convention
C     than BU, hence the minus sign
          BU(I,J,bi,bj)=BU(I,J,bi,bj)
     &         - seaiceMaskU(i,j,bi,bj) *
     &         ( UVRT1(I,J,bi,bj)*( 1. _d 0 - hFacM )*seaiceSlipFactor
     &         + UVRT2(I,J,bi,bj)*( 1. _d 0 - hFacP )*seaiceSlipFactor
     &         )
C     reset coefficients of U(I,J-1) and U(I,J+1)
          UVRT1(I,J,bi,bj)=UVRT1(I,J,bi,bj) * hFacM
          UVRT2(I,J,bi,bj)=UVRT2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

        DO J=1,sNy
         AU(1,J,bi,bj)=ZERO
         CU(sNx,J,bi,bj)=ZERO
         CU(1,J,bi,bj)=CU(1,J,bi,bj)/BU(1,J,bi,bj)
        ENDDO

C     now set up right-hand side
        DO J=1,sNy
         DO I=0,sNx
          dVdy(I,J) = ( vIceC(I,J+1,bi,bj) - vIceC(I,J,bi,bj) )
     &         * _recip_dyF(I,J,bi,bj)
         ENDDO
        ENDDO
        DO J=1,sNy+1
         DO I=1,sNx
          dVdx(I,J) = ( vIceC(I,J,bi,bj) - vIceC(I-1,J,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj)
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
         ENDDO
        ENDDO
        IF ( SEAICE_no_slip ) THEN
C     no slip boundary conditions
C     free slip conditions are taken care of by masking dVdx
         DO J=1,sNy+1
          DO I=1,sNx
           hFacM = seaiceMaskV(I,J,bi,bj) - seaiceMaskV(I-1,J,bi,bj)
           dVdx(I,J) = dVdx(I,J)
     &          + ( vIceC(I,J,bi,bj) + vIceC(I-1,J,bi,bj) )
     &          * _recip_dxV(I,J,bi,bj) * hFacM * 2. _d 0
          ENDDO
         ENDDO
        ENDIF

        DO J=1,sNy
         DO I=1,sNx
C     coriolis and other forcing
          FXY(I,J,bi,bj)=0.5 _d 0 *
     &         ( seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &          *0.5 _d 0*( vIceC( i ,j,bi,bj)+vIceC( i ,j+1,bi,bj) )
     &         + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &          *0.5 _d 0*( vIceC(i-1,j,bi,bj)+vIceC(i-1,j+1,bi,bj) ) )
     &         +FORCEX(I,J,bi,bj)
C     + d/dx[ (zeta-eta) dV/dy]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &         ( zetaMinusEta(I  ,J  ,bi,bj) * dVdy(I  ,J  )
     &         - zetaMinusEta(I-1,J  ,bi,bj) * dVdy(I-1,J  )
     &         ) * _recip_dxC(I,J,bi,bj)
C     + d/dy[ eta dV/x ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &         ( etaMeanZ(I,J+1,bi,bj) * dVdx(I,J+1)
     &         - etaMeanZ(I,J  ,bi,bj) * dVdx(I,J  )
     &         ) * _recip_dyG(I,J,bi,bj)
C     - d/dx[ (eta+zeta) * v * (tanphi/a) ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) - (
     &           etaPlusZeta(I  ,J,bi,bj) * _tanPhiAtU(I  ,J,bi,bj)
     &         * 0.5 _d 0 * (vIceC(I  ,J,bi,bj)+vIceC(I  ,J+1,bi,bj))
     &         - etaPlusZeta(I-1,J,bi,bj) * _tanPhiAtU(I-1,J,bi,bj)
     &         * 0.5 _d 0 * (vIceC(I-1,J,bi,bj)+vIceC(I-1,J+1,bi,bj))
     &         )* _recip_dxC(I,J,bi,bj)*recip_rSphere
C     - 2*eta*(tanphi/a) * dV/dx
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) -
     &         TWO * etaMeanU(I,J,bi,bj)
     &         * _tanPhiAtU(I,J,bi,bj) * recip_rSphere
     &         * 0.5 _d 0 * ( dVdx(I,J+1) + dVdx(I,J) )
         ENDDO
        ENDDO
C     end bi/bj-loops
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       CALL DEBUG_STATS_RL(1,UICE,'UICE before iteration (SEAICE_LSR)',
     &      myThid)
      ENDIF
#endif /* ALLOW_DEBUG */
C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      phexit = -1

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration uice
#endif /* ALLOW_AUTODIFF_TAMC */
      DO M=1, solv_max_iters
      IF ( phexit .EQ. -1 ) THEN

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW SET U(3)=U(1)
        DO J=jMin,jMax
         DO I=1,sNx
          uTmp(I,J,bi,bj)=uIce(I,J,bi,bj)
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
          IF(I.EQ.1) THEN
           AA2 = etaPlusZeta(I-1,J,bi,bj)
     &          * _recip_dxF(I-1,J,bi,bj)
     &          * _recip_dxC(I  ,J,bi,bj)
           AA3=AA2*uIce(I-1,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          ELSEIF(I.EQ.sNx) THEN
           AA1 = etaPlusZeta(I  ,J,bi,bj)
     &          * _recip_dxF(I  ,J,bi,bj)
     &          * _recip_dxC(I  ,J,bi,bj)
           AA3=AA1*uIce(I+1,J,bi,bj) * seaiceMaskU(I,J,bi,bj)
          ELSE
           AA3=ZERO
          ENDIF
          URT(I,J)=FXY(I,J,bi,bj)
     &         + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &         + UVRT1(I,J,bi,bj)*uTmp(I,J-1,bi,bj)
     &         + UVRT2(I,J,bi,bj)*uTmp(I,J+1,bi,bj)
#else
     &         + UVRT1(I,J,bi,bj)*uIce(I,J-1,bi,bj)
     &         + UVRT2(I,J,bi,bj)*uIce(I,J+1,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
          URT(I,J)=URT(I,J)* seaiceMaskU(I,J,bi,bj)
         ENDDO

         DO I=1,sNx
          CUU(I,J)=CU(I,J,bi,bj)
         ENDDO
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
     &        /(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM,J))
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
          uIce(I,J,bi,bj)=uTmp(I,J,bi,bj)
     &        +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
         ENDDO
        ENDDO
C     end bi,bj-loops
       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       S1=ZERO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           UERR=(uIce(I,J,bi,bj)-uTmp(I,J,bi,bj))
     &             * seaiceMaskU(I,J,bi,bj)
           S1=MAX(ABS(UERR),S1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_MAX_RL( S1, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S1.GT.S1A) WFAU=WFAU2
       S1A=S1
       IF(S1.LT.LSR_ERROR) THEN
        ICOUNT1=M
        phexit = 1
       ENDIF
      ENDIF
      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

      ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       _BEGIN_MASTER( myThid )
        write(*,'(A,I6,1P2E22.14)')' U lsr iters, error = ',ICOUNT1,S1
       _END_MASTER( myThid )
       CALL DEBUG_STATS_RL(1,UICE,'UICE after iteration (SEAICE_LSR)',
     &      myThid)
       IF (S1.EQ.0.D0)STOP'u-iteration did not converge'
      ENDIF
#endif /* ALLOW_DEBUG */

C NOW FOR vIce
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
C     coefficients for VICE(I,J)
C     d/dy [(eta+zeta) d/dy] V
          AA1= etaPlusZeta(I,J  ,bi,bj)
     &         *_recip_dyF(I,J  ,bi,bj) * _recip_dyC(I,J,bi,bj)
          AA2= etaPlusZeta(I,J-1,bi,bj)
     &         * _recip_dyF(I,J-1,bi,bj) * _recip_dyC(I,J,bi,bj)
C     d/dx [eta d/dx] V
          AA3= etaMeanZ(I+1,J,bi,bj)
     &         * _recip_dxG(I,J,bi,bj) * _recip_dxV(I+1,J,bi,bj)
          AA4= etaMeanZ(I  ,J,bi,bj)
     &          *_recip_dxG(I,J,bi,bj) * _recip_dxV(I  ,J,bi,bj)
C     d/dy [(zeta-eta) tanphi/a] V
          AA5= zetaMinusEta(I,J  ,bi,bj) * _tanPhiAtU(I,J,  bi,bj)
     &         * _recip_dyC(I,J,bi,bj)*recip_rSphere * 0.5 _d 0
          AA6= zetaMinusEta(I,J-1,bi,bj) * _tanPhiAtU(I,J-1,bi,bj)
     &         * _recip_dyC(I,J,bi,bj)*recip_rSphere * 0.5 _d 0
C     2*eta tanphi/a ( - tanphi/a - d/dy) V
          AA7=TWO*etaMeanV(I,J,bi,bj) * recip_rSphere
     &         * _tanPhiAtV(I,J,bi,bj)
C
          AV(I,J,bi,bj)=(
     &         - AA2
     &         - AA6
     &         - AA7*1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &         )* seaiceMaskV(I,J,bi,bj)
          CV(I,J,bi,bj)=(
     &         -AA1
     &         + AA5
     &         + AA7*1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &         )* seaiceMaskV(I,J,bi,bj)
          BV(I,J,bi,bj)= (ONE - seaiceMaskV(I,J,bi,bj))
     &         +( (AA1+AA2) + (AA3+AA4)  + (AA5-AA6)
     &         + AA7 * _tanPhiAtV(I,J,bi,bj)*recip_rSphere
     &         + seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0 * ( DRAGS(I,J,bi,bj) + DRAGS(I,J-1,bi,bj) )
     &         )* seaiceMaskV(I,J,bi,bj)
C     coefficients for V(I-1,J)
          UVRT1(I,J,bi,bj)= AA4
C     coefficients for V(I+1,J)
          UVRT2(I,J,bi,bj)= AA3
         ENDDO
        ENDDO

C     apply boundary conditions according to slip factor
        DO J=1,sNy
         DO I=1,sNx
          hFacM = seaiceMaskV(i-1,j,bi,bj)
          hFacP = seaiceMaskV(i+1,j,bi,bj)
C     copy contributions to coefficient of V(I,J)
C     beware of sign convection: UVRT1/2 have the opposite sign convention
C     than BV, hence the minus sign
          BV(I,J,bi,bj)=BV(I,J,bi,bj)
     &         - seaiceMaskV(I,J,bi,bj) *
     &         ( UVRT1(I,J,bi,bj)*( 1. _d 0 - hFacM )*seaiceSlipFactor
     &         + UVRT2(I,J,bi,bj)*( 1. _d 0 - hFacP )*seaiceSlipFactor
     &         )
C     reset coefficients of V(I-1,J) and V(I+1,J)
          UVRT1(I,J,bi,bj)=UVRT1(I,J,bi,bj) * hFacM
          UVRT2(I,J,bi,bj)=UVRT2(I,J,bi,bj) * hFacP
         ENDDO
        ENDDO

        DO I=1,sNx
         AV(I,1,bi,bj)=ZERO
         CV(I,sNy,bi,bj)=ZERO
         CV(I,1,bi,bj)=CV(I,1,bi,bj)/BV(I,1,bi,bj)
        ENDDO

C     now set up right-hand-side
        DO J=0,sNy
         DO I=1,sNx
          dUdx(I,J) = ( uIceC(I+1,J,bi,bj) - uIceC(I,J,bi,bj) )
     &         * _recip_dxF(I,J,bi,bj)
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx+1
          dUdy(I,J) = ( uIceC(I,J,bi,bj) - uIceC(I,J-1,bi,bj) )
     &         * _recip_dyU(I,J,bi,bj)
     &         *maskC(I  ,J  ,k,bi,bj)*maskC(I-1,J  ,k,bi,bj)
     &         *maskC(I  ,J-1,k,bi,bj)*maskC(I-1,J-1,k,bi,bj)
         ENDDO
        ENDDO
        IF ( SEAICE_no_slip ) THEN
C     no slip boundary conditions
C     free slip conditions are taken care of by masking dUdy
         DO J=1,sNy
          DO I=1,sNx+1
           hFacM = seaiceMaskU(I,J,bi,bj) - seaiceMaskU(I,J-1,bi,bj)
           dUdy(I,J) = dUdy(I,J)
     &          + ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &          * _recip_dyU(I,J,bi,bj) * hFacM * 2. _d 0
          ENDDO
         ENDDO
        ENDIF
C     average of uIce on vorticity (Z) points, no slip boundary
C     conditions (u(j)+u(j-1)=0) are take care of by masking
        DO J=1,sNy
         DO I=1,sNx+1
          uMeanZ(I,J) =
     &         0.5 _d 0 * ( uIceC(I,J,bi,bj) + uIceC(I,J-1,bi,bj) )
     &         *seaiceMaskU(I,J,bi,bj)*seaiceMaskU(I,J-1,bi,bj)
         ENDDO
        ENDDO
        IF ( .NOT. SEAICE_no_slip ) THEN
C     for free slip (u(j)-u(j-1)=0) 0.5*(u(j)+u(j-1)) is either u(j) or u(j-1)
         DO J=1,sNy
          DO I=1,sNx+1
           uMeanZ(I,J) = uMeanZ(I,J)
     &          + uIceC(I,J  ,bi,bj) * seaiceMaskU(I,J  ,bi,bj)
     &          * ( 1. _d 0 - seaiceMaskU(I,J-1,bi,bj) )
     &          + uIceC(I,J-1,bi,bj) * seaiceMaskU(I,J-1,bi,bj)
     &          * ( 1. _d 0 - seaiceMaskU(I,J  ,bi,bj))
          ENDDO
         ENDDO
        ENDIF

        DO J=1,sNy
         DO I=1,sNx
C     coriols and other foring
          FXY(I,J,bi,bj)= - 0.5 _d 0 *
     &         ( seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &         *0.5 _d 0*( uIceC(i  ,j  ,bi,bj)+uIceC(i+1,  j,bi,bj) )
     &         + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &         *0.5 _d 0*( uIceC(i  ,j-1,bi,bj)+uIceC(i+1,j-1,bi,bj) ) )
     &         + FORCEY(I,J,bi,bj)
C     + d/dy[ (zeta-eta) dU/dx ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &         ( zetaMinusEta(I,J  ,bi,bj)*dUdx(I,J  )
     &         - zetaMinusEta(I,J-1,bi,bj)*dUdx(I,J-1) )
     &         * _recip_dyC(I,J,bi,bj)
C     + d/dx[ eta dU/dy ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &         ( etaMeanZ(I+1,J  ,bi,bj) * dUdy(I+1,J)
     &         - etaMeanZ(I  ,J  ,bi,bj) * dUdy(I  ,J))
     &         * _recip_dxG(I,J,bi,bj)
C     + d/dx[ eta * (tanphi/a) * U ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) + (
     &           etaMeanZ(I+1,J,bi,bj) * _tanPhiAtV(I+1,J,bi,bj)
     &         * uMeanZ(I+1,J)
     &         - etaMeanZ(I  ,J,bi,bj) * _tanPhiAtV(I  ,J,bi,bj)
     &         * uMeanZ(I  ,J)
     &         ) *  _recip_dxG(I,J,bi,bj)*recip_rSphere
C     + 2*eta*(tanphi/a) dU/dx
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &        TWO * etaMeanV(I,J,bi,bj)
     &         * _tanPhiAtV(I,J,bi,bj)*recip_rSphere
     &         * 0.5 _d 0 * ( dUdx(I,J) + dUdx(I,J-1) )
         ENDDO
        ENDDO
C     end bi/bj-loops
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       CALL DEBUG_STATS_RL(1,VICE,'VICE before iteration (SEAICE_LSR)',
     &      myThid)
      ENDIF
#endif /* ALLOW_DEBUG */

C NOW DO ITERATION

cph--- iteration starts here
cph--- need to kick out goto
      phexit = -1

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration vice
#endif /* ALLOW_AUTODIFF_TAMC */
      DO M=1, solv_max_iters
      IF ( phexit .EQ. -1 ) THEN

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW SET V(3)=V(1)
        DO J=1,sNy
         DO I=iMin,iMax
          vTmp(I,J,bi,bj)=vIce(I,J,bi,bj)
         ENDDO
        ENDDO

        DO I=1,sNx
         DO J=1,sNy
          IF(J.EQ.1) THEN
           AA2= etaPlusZeta(I,J-1,bi,bj)
     &          * _recip_dyF(I,J-1,bi,bj) * _recip_dyC(I,J,bi,bj)
           AA3=( AA2
     &          + zetaMinusEta(I,J-1,bi,bj) * _tanPhiAtU(I,J-1,bi,bj)
     &          * _recip_dyC(I,J,bi,bj)*recip_rSphere
     &          + TWO*etaMeanV(I,J,bi,bj) * recip_rSphere
     &          * _tanPhiAtV(I,J,bi,bj)
     &          *1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &          ) * vIce(I,J-1,bi,bj) * seaiceMaskV(I,J,bi,bj)
          ELSEIF(J.EQ.sNy) THEN
           AA1= etaPlusZeta(I,J  ,bi,bj)
     &          *_recip_dyF(I,J  ,bi,bj) * _recip_dyC(I,J,bi,bj)
           AA3=( AA1
     &          - zetaMinusEta(I,J  ,bi,bj) * _tanPhiAtU(I,J,  bi,bj)
     &          * _recip_dyC(I,J,bi,bj)*recip_rSphere
     &          - TWO*etaMeanV(I,J,bi,bj) * recip_rSphere
     &          * _tanPhiAtV(I,J,bi,bj)
     &          *1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &          ) * vIce(I,J+1,bi,bj) * seaiceMaskV(I,J,bi,bj)
          ELSE
           AA3=ZERO
          ENDIF

          VRT(I,J)=FXY(I,J,bi,bj)
     &         + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &         + UVRT1(I,J,bi,bj)*vTmp(I-1,J,bi,bj)
     &         + UVRT2(I,J,bi,bj)*vTmp(I+1,J,bi,bj)
#else
     &         + UVRT1(I,J,bi,bj)*vIce(I-1,J,bi,bj)
     &         + UVRT2(I,J,bi,bj)*vIce(I+1,J,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
          VRT(I,J)=VRT(I,J)* seaiceMaskV(I,J,bi,bj)
         ENDDO

         DO J=1,sNy
          CVV(I,J)=CV(I,J,bi,bj)
         ENDDO
         VRT(I,1)=VRT(I,1)/BV(I,1,bi,bj)
         DO J=2,sNy
          JM=J-1
          CVV(I,J)=CVV(I,J)/(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
          VRT(I,J)=(VRT(I,J)-AV(I,J,bi,bj)*VRT(I,JM))
     &         /(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(I,JM))
         ENDDO
         DO J=1,sNy-1
          J1=sNy-J
          J2=J1+1
          VRT(I,J1)=VRT(I,J1)-CVV(I,J1)*VRT(I,J2)
         ENDDO
         DO J=1,sNy
          vIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &        +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
         ENDDO
        ENDDO
C     end bi,bj-loops
       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       S2=ZERO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           UERR=(vIce(I,J,bi,bj)-vTmp(I,J,bi,bj))
     &             * seaiceMaskV(I,J,bi,bj)
           S2=MAX(ABS(UERR),S2)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _GLOBAL_MAX_RL( S2, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S2.GT.S2A) WFAV=WFAV2
       S2A=S2
       IF(S2.LT.LSR_ERROR) THEN
        ICOUNT2=M
        phexit = 1
       ENDIF
      ENDIF
      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

      ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) THEN
       _BEGIN_MASTER( myThid )
        write(*,'(A,I6,1P2E22.14)')' V lsr iters, error = ',ICOUNT2,S2
       _END_MASTER( myThid )
       CALL DEBUG_STATS_RL(1,VICE,'VICE after iteration (SEAICE_LSR)',
     &      myThid)
       IF (S2.EQ.0.D0)STOP'v-iteration did not converge'
      ENDIF
#endif /* ALLOW_DEBUG */

C     APPLY MASKS
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          uIce(I,J,bi,bj)=uIce(I,J,bi,bj)* seaiceMaskU(I,J,bi,bj)
          vIce(I,J,bi,bj)=vIce(I,J,bi,bj)* seaiceMaskV(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
CML      CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

#endif /* SEAICE_OLD_AND_BAD_DISCRETIZATION */

      IF ( useHB87StressCoupling .AND. ilcall.EQ.NPSEUDOTIMESTEPS ) THEN
C     compute the divergence of stress here to be used later
C
C     compute strain rate from latest velocities
       CALL SEAICE_CALC_STRAINRATES(
     I       uIce, vIce,
     O       e11, e22, e12,
     I       3, myTime, myIter, myThid )
C     compute internal stresses with updated ice velocities
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           sig11(I,J) = 0. _d 0
           sig22(I,J) = 0. _d 0
           sig12(I,J) = 0. _d 0
          ENDDO
         ENDDO

         DO j=0,sNy
          DO i=0,sNx
           eplus = e11(I,J,bi,bj) + e22(I,J,bi,bj)
           eminus= e11(I,J,bi,bj) - e22(I,J,bi,bj)
           sig11(I,J) = zeta(I,J,bi,bj)*eplus + eta(I,J,bi,bj)*eminus
     &          - 0.5 _d 0 * PRESS(I,J,bi,bj)
           sig22(I,J) = zeta(I,J,bi,bj)*eplus - eta(I,J,bi,bj)*eminus
     &          - 0.5 _d 0 * PRESS(I,J,bi,bj)
          ENDDO
         ENDDO

         DO j=1,sNy+1
          DO i=1,sNx+1
           sig12(I,J) = 2. _d 0 * e12(I,J,bi,bj) * etaMeanZ(I,J,bi,bj)
          ENDDO
         ENDDO
C     evaluate divergence of stress and apply to forcing
         DO J=1,sNy
          DO I=1,sNx
           stressDivergenceX(I,J,bi,bj) =
     &          ( sig11(I  ,J  ) * _dyF(I  ,J  ,bi,bj)
     &          - sig11(I-1,J  ) * _dyF(I-1,J  ,bi,bj)
     &          + sig12(I  ,J+1) * _dxV(I  ,J+1,bi,bj)
     &          - sig12(I  ,J  ) * _dxV(I  ,J  ,bi,bj)
     &          ) * recip_rAw(I,J,bi,bj)
            stressDivergenceY(I,J,bi,bj) =
     &          ( sig22(I  ,J  ) * _dxF(I  ,J  ,bi,bj)
     &          - sig22(I  ,J-1) * _dxF(I  ,J-1,bi,bj)
     &          + sig12(I+1,J  ) * _dyU(I+1,J  ,bi,bj)
     &          - sig12(I  ,J  ) * _dyU(I  ,J  ,bi,bj)
     &          ) * recip_rAs(I,J,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C     endif  useHB87StressCoupling
      ENDIF

#ifdef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE
      IF ( debugLevel .GE. debLevB ) THEN
       resnorm = 0. _d 0
       EKnorm  = 0. _d 0
       counter = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C--   Compute residual norm and print
         DO j=1,sNy
          DO i=1,sNx
           resnorm = resnorm + 0.5 _d 0 *
     &          ( ( (uIceNm1(i,j,bi,bj)+uIceNm1(i+1,j,bi,bj))
     &            - (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)) )**2
     &          + ( (vIceNm1(i,j,bi,bj)+vIceNm1(i,j+1,bi,bj))
     &            - (vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj)) )**2 )
           IF ( area(i,j,bi,bj) .gt. 0.5 _d 0 ) THEN
            EKnorm = EKnorm + 0.5 _d 0 * heff(i,j,bi,bj) *
     &           ( ( (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)) )**2
     &           + ( (vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj)) )**2 )
            counter = counter + 1. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_R8( resnorm, myThid )
       CALL GLOBAL_SUM_R8( EKnorm, myThid )
       CALL GLOBAL_SUM_R8( counter, myThid )
       IF ( counter .gt. 0. _d 0 ) EKnorm = EKnorm/counter
       WRITE(*,'(A,I7,1X,2E22.14)')
     &      'S/R seaice_lsr: IPSEUDO, RESNORM, EKNORM = ', 
     &      ilcall, sqrt(resnorm), EKnorm
      ENDIF
#endif /* SEAICE_ALLOW_CHECK_LSR_CONVERGENCE */

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END
