C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_lsr.F,v 1.12 2006/06/06 05:05:18 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_LSR( ilcall, myThid )
C     /==========================================================\
C     | SUBROUTINE  SEAICE_LSR                                   |
C     | o Solve ice momentum equation with an LSR dynamics solver|
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997     |
C     |    and Zhang and Rothrock, MWR, 131,  845- 861, 2003)    |
C     |   Written by Jinlun Zhang, PSC/UW, Feb-2001              |
C     |                     zhang@apl.washington.edu             |
C     |==========================================================|
C     | C-grid version by Martin Losch                           |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER ilcall
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, m, bi, bj, j1, j2, im, jm
      INTEGER ICOUNT1, ICOUNT2, SOLV_MAX_ITERS, SOLV_NCHECK
      INTEGER phexit

      _RL  WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL  AA1, AA2, AA3, AA4, AA5, AA6, AA7, S1, S2, S1A, S2A

      _RL AU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL BU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL CU   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL AV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL BV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL CV   (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UERR (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL FXY  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL URT(1-Olx:sNx+Olx), CUU(1-Olx:sNx+Olx)
      _RL VRT(1-Oly:sNy+Oly), CVV(1-Oly:sNy+Oly)

      _RL etaPlusZeta (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zetaMinusEta(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanZ    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanU    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaMeanV    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL UVRT1    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UVRT2    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL dVdy     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dUdx     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dUdy     (1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      _RL SINWAT, COSWAT
      _RL TEMPVAR

      _RL PRESS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

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

      SOLV_MAX_ITERS=1500
      SOLV_NCHECK=2

      ICOUNT1=SOLV_MAX_ITERS
      ICOUNT2=SOLV_MAX_ITERS

#ifdef ALLOW_AUTODIFF_TAMC
cph That's an important one! Note, that
cph * lsr is called twice, thus the icall index
cph * this storing is still outside the iteration loop
CADJ STORE uice = comlev1_lsr, 
CADJ &            key = ikey_dynamics + (ilcall-1)*nchklev_1
CADJ STORE vice = comlev1_lsr, 
CADJ &            key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif /* ALLOW_AUTODIFF_TAMC */

      CALL SEAICE_CALC_VISCOSITIES( 
     I     uIceC, vIceC, zMin, zMax, hEffM, press0,
     O     eta, zeta, press, 
#ifdef SEAICE_ALLOW_EVP
     O     seaice_div, seaice_tension, seaice_shear,
#endif /* SEAICE_ALLOW_EVP */
     I     myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
C NOW SET UP NON-LINEAR WATER DRAG, FORCEX, FORCEY
          TEMPVAR = QUART*(
     &          (uIceC(I  ,J,bi,bj)-GWATX(I  ,J,bi,bj)
     &          +uIceC(I+1,J,bi,bj)-GWATX(I+1,J,bi,bj))**2
     &         +(vIceC(I,J  ,bi,bj)-GWATY(I,J  ,bi,bj)
     &          +vIceC(I,J+1,bi,bj)-GWATY(I,J+1,bi,bj))**2)
          IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag)**2 ) THEN
             DWATN(I,J,bi,bj)=QUART
          ELSE
             DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
          ENDIF
          DWATN(I,J,bi,bj) = DWATN(I,J,bi,bj) * HEFFM(I,J,bi,bj)
C NOW SET UP SYMMETTRIC DRAG
          DRAGS(I,J,bi,bj) = DWATN(I,J,bi,bj)*COSWAT
C NOW SET UP ANTI SYMMETTRIC DRAG FORCE AND ADD IN CURRENT FORCE 
C     ( remember to average to correct velocity points )
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &         0.5*( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * GWATX(I,J,bi,bj) 
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 * 
     &         ( DWATN(I  ,J,bi,bj) *
     &         0.5 _d 0 * (GWATY(I  ,J  ,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &                    +GWATY(I  ,J+1,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &         + DWATN(I-1,J,bi,bj) *
     &         0.5 _d 0 * (GWATY(I-1,J  ,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &                    +GWATY(I-1,J+1,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &         )
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &         0.5*( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * GWATY(I,J,bi,bj) 
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 * 
     &         ( DWATN(I,J  ,bi,bj) *
     &         0.5 _d 0 * (GWATX(I  ,J  ,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &                    +GWATX(I+1,J  ,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &         + DWATN(I,J-1,bi,bj) *
     &         0.5 _d 0 * (GWATX(I  ,J-1,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &                    +GWATX(I+1,J-1,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &         )
C NOW CALCULATE PRESSURE FORCE AND ADD TO EXTERNAL FORCE
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         - 0.5 _d 0 *  _recip_dxC(I,J,bi,bj)
     &         *(PRESS(I,J,bi,bj) - PRESS(I-1,J  ,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj) 
     &         - 0.5 _d 0 *  _recip_dyC(I,J,bi,bj)
     &          *(PRESS(I,J,bi,bj) - PRESS(I  ,J-1,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         +seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *uIce(I,J,2,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         *vIce(I,J,2,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)* _maskW(I,J,1,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)* _maskS(I,J,1,bi,bj)
          etaPlusZeta (I,J,bi,bj) = ETA (I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA (I,J,bi,bj)
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          etaMeanU (I,J,bi,bj) =
     &         HALF*(ETA (I,J,bi,bj) + ETA (I-1,J  ,bi,bj))
          etaMeanV (I,J,bi,bj) =
     &         HALF*(ETA (I,J,bi,bj) + ETA (I  ,J-1,bi,bj))
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          etaMeanZ (I,J,bi,bj) = 
     &         HALF * ( etaMeanU(I,J,bi,bj) + etaMeanU(I,J-1,bi,bj) )
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
     &         * _recip_dxC(I  ,J,bi,bj) * _maskW(I,J,1,bi,bj)
          AA2 = etaPlusZeta(I-1,J,bi,bj)
     &         * _recip_dxF(I-1,J,bi,bj)
     &         * _recip_dxC(I  ,J,bi,bj) * _maskW(I,J,1,bi,bj)
C     (d/dy)[eta*(d/dy + tanphi/a)] U (also on UVRT1/2)
          AA3= ( etaMeanZ(I,J+1,bi,bj) * _recip_dyU(I,J+1,bi,bj)
     &         + etaMeanZ(I,J  ,bi,bj) * _recip_dyU(I,J  ,bi,bj) 
     &         ) * _recip_dyG(I,J,bi,bj)
     &         - (etaMeanZ(I,J+1,bi,bj) - etaMeanZ(I,J,bi,bj)) 
     &         * 0.5 _d 0 * _tanPhiAtU(I,J,bi,bj)
     &         * recip_rSphere * _recip_dyG(I,J,bi,bj)
C     2*eta*(tanphi/a) * ( tanphi/a ) U
          AA6=TWO*etaMeanU(I,J,bi,bj)*recip_rSphere*recip_rSphere
     &         * _tanPhiAtU(I,J,bi,bj)  * _tanPhiAtU(I,J,bi,bj)
          AU(I,J,bi,bj)=-AA2
          CU(I,J,bi,bj)=-AA1
          BU(I,J,bi,bj)=(ONE - _maskW(I,J,1,bi,bj))
     &         - AU(I,J,bi,bj) - CU(I,J,bi,bj)
     &         + ( AA3 + AA6
     &         + seaiceMassU(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I-1,J,bi,bj) )
     &         )* _maskW(I,J,1,bi,bj)
C     coefficients of uIce(I,J-1)
          UVRT1(I,J,bi,bj)=
     &         etaMeanZ(I,J  ,bi,bj) * _recip_dyG(I,J  ,bi,bj) * (
     &                                 _recip_dyU(I,J  ,bi,bj) 
     &         - _tanPhiAtU(I,J  ,bi,bj) * 0.5 _d 0 * recip_rSphere ) 
     &         + TWO*etaMeanU(I,J,bi,bj) * _tanPhiAtU(I,J,bi,bj)
     &          * 1.0 _d 0 / ( _dyU(I,J,bi,bj) + _dyU(I,J+1,bi,bj) )
     &         *recip_rSphere
C     coefficients of uIce(I,J+1)
          UVRT2(I,J,bi,bj)=
     &         etaMeanZ(I,J+1,bi,bj) * _recip_dyG(I,J  ,bi,bj) * (
     &                                 _recip_dyU(I,J+1,bi,bj) 
     &         + _tanPhiAtU(I,J+1,bi,bj) * 0.5 _d 0 * recip_rSphere )
     &         - TWO*etaMeanU(I,J,bi,bj) * _tanPhiAtU(I,J,bi,bj)
     &          * 1.0 _d 0 / ( _dyU(I,J,bi,bj) + _dyU(I,J+1,bi,bj) )
     &         *recip_rSphere
         END DO
        END DO

        DO J=1,sNy
         AU(1,J,bi,bj)=ZERO
         CU(sNx,J,bi,bj)=ZERO
         CU(1,J,bi,bj)=CU(1,J,bi,bj)/BU(1,J,bi,bj)
        END DO

C     now set up right-hand side
        DO J=1-Oly,sNy+Oly-1
         DO I=1-Olx,sNx+Olx
          dVdy(I,J) = ( vIceC(I,J+1,bi,bj) - vIceC(I,J,bi,bj) )
     &         * _recip_dyF(I,J,bi,bj)
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
C     coriolis and other forcing
          FXY(I,J,bi,bj)=
     &     0.5*( seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &          *0.5*( vIceC( i ,j,bi,bj)+vIceC( i ,j+1,bi,bj) )
     &         + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &          *0.5*( vIceC(i-1,j,bi,bj)+vIceC(i-1,j+1,bi,bj) ) )
     &         +FORCEX(I,J,bi,bj)
C     + d/dx[ (zeta-eta) dV/dy]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &         ( zetaMinusEta(I  ,J  ,bi,bj) * dVdy(I  ,J  )
     &         - zetaMinusEta(I-1,J  ,bi,bj) * dVdy(I-1,J  )
     &         ) * _recip_dxC(I,J,bi,bj) 
C     + d/dy[ eta dV/x ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) + (
     &         etaMeanZ(I,J+1,bi,bj)
     &         * ( vIceC(I  ,J+1,bi,bj) - vIceC(I-1,J+1,bi,bj) )
     &         * _recip_dxV(I,J+1,bi,bj)
     &         - etaMeanZ(I,J,bi,bj)
     &         * ( vIceC(I  ,J,bi,bj) - vIceC(I-1,J,bi,bj) )
     &         * _recip_dxV(I,J,bi,bj)
     &         ) * _recip_dyG(I,J,bi,bj)
C     - d/dx[ (eta+zeta) * v * (tanphi/a) ]
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) - (
     &           etaPlusZeta(I  ,J  ,bi,bj) 
     &         * 0.5 _d 0 * (vIceC(I  ,J,bi,bj)+vIceC(I  ,J+1,bi,bj))
     &         * 0.5 _d 0 * ( _tanPhiAtU(I  ,J,bi,bj) 
     &                      + _tanPhiAtU(I+1,J,bi,bj) )
     &         - etaPlusZeta(I-1,J,bi,bj)
     &         * 0.5 _d 0 * (vIceC(I-1,J,bi,bj)+vIceC(I-1,J+1,bi,bj))
     &         * 0.5 _d 0 * ( _tanPhiAtU(I-1,J,bi,bj) 
     &                      + _tanPhiAtU(I  ,J,bi,bj) )
     &         )* _recip_dxC(I,J,bi,bj)*recip_rSphere
C     - 2*eta*(tanphi/a) * dV/dx 
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) - 
     &         TWO * etaMeanU(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &         *recip_rSphere
     &         *(vIceC(I  ,J,bi,bj) + vIceC(I  ,J+1,bi,bj)
     &          -vIceC(I-1,J,bi,bj) - vIceC(I-1,J+1,bi,bj))
     &         * _recip_dxC(I,J,bi,bj)
         END DO
        END DO

       ENDDO
      ENDDO

C NOW DO ITERATION
100   CONTINUE

cph--- iteration starts here
cph--- need to kick out goto
      phexit = -1

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration uice
#endif /* ALLOW_AUTODIFF_TAMC */

      DO 8000 M=1, solv_max_iters
cph(
      IF ( phexit .EQ. -1 ) THEN
cph)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW SET U(3)=U(1)
        DO J=1,sNy
         DO I=1,sNx
          uIce(I,J,3,bi,bj)=uIce(I,J,1,bi,bj)
         END DO
        END DO

        DO 1200 J=1,sNy
         DO I=1,sNx
          IF(I.EQ.1) THEN
           AA2 = etaPlusZeta(I-1,J,bi,bj)
     &          * _recip_dxF(I-1,J,bi,bj)
     &          * _recip_dxC(I  ,J,bi,bj)
           AA3=AA2*uIce(I-1,J,1,bi,bj)* _maskW(I,J,1,bi,bj)
          ELSE IF(I.EQ.sNx) THEN
           AA1 = etaPlusZeta(I  ,J,bi,bj)
     &          * _recip_dxF(I  ,J,bi,bj) 
     &          * _recip_dxC(I  ,J,bi,bj)
           AA3=AA1*uIce(I+1,J,1,bi,bj) * _maskW(I,J,1,bi,bj)
          ELSE
           AA3=ZERO
          END IF
          URT(I)=FXY(I,J,bi,bj)+AA3
     &          +UVRT1(I,J,bi,bj)*uIce(I,J-1,1,bi,bj)
     &          +UVRT2(I,J,bi,bj)*uIce(I,J+1,1,bi,bj)
          URT(I)=URT(I)* _maskW(I,J,1,bi,bj) * seaiceMaskU(I,J,bi,bj)
         END DO

         DO I=1,sNx
          CUU(I)=CU(I,J,bi,bj)
         END DO
         URT(1)=URT(1)/BU(1,J,bi,bj)
         DO I=2,sNx
          IM=I-1
          CUU(I)=CUU(I)/(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM))
          URT(I)=(URT(I)-AU(I,J,bi,bj)*URT(IM))
     &          /(BU(I,J,bi,bj)-AU(I,J,bi,bj)*CUU(IM))
         END DO
         DO I=1,sNx-1
          J1=sNx-I
          J2=J1+1
          URT(J1)=URT(J1)-CUU(J1)*URT(J2)
         END DO
         DO I=1,sNx
          uIce(I,J,1,bi,bj)=uIce(I,J,3,bi,bj)
     &        +WFAU*(URT(I)-uIce(I,J,3,bi,bj))
         END DO

1200    CONTINUE

       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         S1=ZERO
         DO J=1,sNy
          DO I=1,sNx
           UERR(I,J,bi,bj)=(uIce(I,J,1,bi,bj)-uIce(I,J,3,bi,bj))
     &             * _maskW(I,J,1,bi,bj)
           S1=MAX(ABS(UERR(I,J,bi,bj)),S1)
          END DO
         END DO
         _GLOBAL_MAX_R8( S1, myThid )
        ENDDO
       ENDDO
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S1.GT.S1A) WFAU=WFAU2
       S1A=S1
       IF(S1.LT.LSR_ERROR) THEN
        ICOUNT1=M
cph(
cph        GO TO 8001
        phexit = 1
cph)
       END IF
      END IF
      CALL SEAICE_EXCH_UV ( uIce, vIce, myThid )

cph(
      END IF
cph)

8000  CONTINUE
cph 8001  CONTINUE
C ITERATION END -----------------------------------------------------

      IF ( debugLevel .GE. debLevB ) THEN
       _BEGIN_MASTER( myThid )
        write(*,'(A,I6,1P2E22.14)')' U lsr iters, error = ',ICOUNT1,S1
       _END_MASTER( myThid )
      ENDIF

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
          AA5= zetaMinusEta(I,J  ,bi,bj) * tanPhiAtU(I,J  ,bi,bj)
     &         * _recip_dyC(I,J,bi,bj)*recip_rSphere * 0.5 _d 0
          AA6= zetaMinusEta(I,J-1,bi,bj) * tanPhiAtU(I,J-1,bi,bj)
     &         * _recip_dyC(I,J,bi,bj)*recip_rSphere * 0.5 _d 0
C     2*eta tanphi/a ( - tanphi/a - d/dy) V
          AA7=TWO*etaMeanV(I,J,bi,bj) * recip_rSphere
     &         * _tanPhiAtV(I,J,bi,bj) 
C
          AV(I,J,bi,bj)=(
     &         - AA2
     &         - AA6
     &         - AA7*1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &         )* _maskS(I,J,1,bi,bj)
          CV(I,J,bi,bj)=(
     &         -AA1 
     &         + AA5
     &         + AA7*1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &         )* _maskS(I,J,1,bi,bj)
          BV(I,J,bi,bj)= (ONE- _maskS(I,J,1,bi,bj))
     &         +( (AA1+AA2) + (AA3+AA4)  + (AA5-AA6) 
     &         + AA7 * _tanPhiAtV(I,J,bi,bj)*recip_rSphere
     &         + seaiceMassV(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + 0.5 _d 0 * ( DRAGS(I,J,bi,bj) + DRAGS(I,J-1,bi,bj) )
     &         )* _maskS(I,J,1,bi,bj)
C     coefficients for V(I-1,J)
          UVRT1(I,J,bi,bj)= AA4
C     coefficients for V(I+1,J)
          UVRT2(I,J,bi,bj)= AA3
         END DO
        END DO

        DO I=1,sNx
         AV(I,1,bi,bj)=ZERO
         CV(I,sNy,bi,bj)=ZERO
         CV(I,1,bi,bj)=CV(I,1,bi,bj)/BV(I,1,bi,bj)
        END DO

C     now set up right-hand-side
        DO J=1-Oly,sNy+Oly-1
         DO I=1-Olx,sNx+Olx-1
          dUdx(I,J) = ( uIceC(I+1,J,bi,bj) - uIceC(I,J,bi,bj) )
     &         * _recip_dxF(I,J,bi,bj)
          dUdy(I,J) = ( uIceC(I,J+1,bi,bj) - uIceC(I,J,bi,bj) )
     &         * _recip_dyU(I,J+1,bi,bj)
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
C     coriols and other foring
          FXY(I,J,bi,bj)=
     &         -0.5*( seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &              *0.5*( uIceC(i  ,j  ,bi,bj)+uIceC(i+1,  j,bi,bj) )
     &              + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &              *0.5*( uIceC(i  ,j-1,bi,bj)+uIceC(i+1,j-1,bi,bj) ) )
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
     &         etaMeanZ(I+1,J,bi,bj) * 0.5 * 
     &         ( uIceC(I+1,J  ,bi,bj) * _tanPhiAtU(I+1,J  ,bi,bj)
     &         + uIceC(I+1,J-1,bi,bj) * _tanPhiAtU(I+1,J-1,bi,bj) ) 
     &         - etaMeanZ(I  ,J,bi,bj) * 0.5 * 
     &         ( uIceC(I  ,J  ,bi,bj) * _tanPhiAtU(I  ,J  ,bi,bj) 
     &         + uIceC(I  ,J-1,bi,bj) * _tanPhiAtU(I  ,J  ,bi,bj) ) 
     &         ) *  _recip_dxG(I,J,bi,bj)*recip_rSphere
C     + 2*eta*(tanphi/a) dU/dx
          FXY(I,J,bi,bj)=FXY(I,J,bi,bj) +
     &        TWO * etaMeanV(I,J,bi,bj)*TWO  * _tanPhiAtV(I,J,bi,bj)
     &        * ( uIceC(I+1,J,bi,bj)+uIceC(I+1,J-1,bi,bj)
     &          - uIceC(I  ,J,bi,bj)-uIceC(I  ,J-1,bi,bj) )
     &         * _recip_dxG(I,J,bi,bj)
     &         *recip_rSphere
         END DO
        END DO

       ENDDO
      ENDDO

C NOW DO ITERATION
300   CONTINUE

cph--- iteration starts here
cph--- need to kick out goto
      phexit = -1

C ITERATION START -----------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CADJ LOOP = iteration vice
#endif /* ALLOW_AUTODIFF_TAMC */

      DO 9000 M=1, solv_max_iters
cph(
      IF ( phexit .EQ. -1 ) THEN
cph)
C NOW SET U(3)=U(1)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          vIce(I,J,3,bi,bj)=vIce(I,J,1,bi,bj) 
         END DO
        END DO

        DO I=1,sNx
         DO J=1,sNy
           IF(J.EQ.1) THEN
            AA2= etaPlusZeta(I,J-1,bi,bj)
     &           * _recip_dyF(I,J-1,bi,bj) * _recip_dyC(I,J,bi,bj)
            AA3=( AA2
     &           + zetaMinusEta(I,J-1,bi,bj) * tanPhiAtU(I,J-1,bi,bj)
     &           * _recip_dyC(I,J,bi,bj)*recip_rSphere
     &           + TWO*etaMeanV(I,J,bi,bj) * recip_rSphere
     &           * _tanPhiAtV(I,J,bi,bj) 
     &           *1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &           ) * vIce(I,J-1,1,bi,bj) * _maskS(I,J,1,bi,bj)
           ELSE IF(J.EQ.sNy) THEN
            AA1= etaPlusZeta(I,J  ,bi,bj)
     &           *_recip_dyF(I,J  ,bi,bj) * _recip_dyC(I,J,bi,bj)
            AA3=( AA1
     &           - zetaMinusEta(I,J  ,bi,bj) * tanPhiAtU(I,J  ,bi,bj)
     &           * _recip_dyC(I,J,bi,bj)*recip_rSphere
     &           - TWO*etaMeanV(I,J,bi,bj) * recip_rSphere
     &           * _tanPhiAtV(I,J,bi,bj) 
     &           *1.0 _d 0 / ( _dyF(I,J,bi,bj) + _dyF(I,J-1,bi,bj) )
     &           ) * vIce(I,J+1,1,bi,bj) * _maskS(I,J,1,bi,bj)
           ELSE
            AA3=ZERO
           END IF

          VRT(J)=FXY(I,J,bi,bj)+AA3+UVRT1(I,J,bi,bj)*vIce(I-1,J,1,bi,bj)
     &                       +UVRT2(I,J,bi,bj)*vIce(I+1,J,1,bi,bj)
          VRT(J)=VRT(J)* _maskS(I,J,1,bi,bj) * seaiceMaskV(I,J,bi,bj)
         END DO

         DO J=1,sNy
          CVV(J)=CV(I,J,bi,bj)
         END DO
         VRT(1)=VRT(1)/BV(I,1,bi,bj)
         DO J=2,sNy
          JM=J-1
          CVV(J)=CVV(J)/(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(JM))
          VRT(J)=(VRT(J)-AV(I,J,bi,bj)*VRT(JM))
     &          /(BV(I,J,bi,bj)-AV(I,J,bi,bj)*CVV(JM))
         END DO
         DO J=1,sNy-1
          J1=sNy-J
          J2=J1+1
          VRT(J1)=VRT(J1)-CVV(J1)*VRT(J2)
         END DO
         DO J=1,sNy
          vIce(I,J,1,bi,bj)=vIce(I,J,3,bi,bj)
     &        +WFAV*(VRT(J)-vIce(I,J,3,bi,bj))
         END DO
        ENDDO

       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         S2=ZERO
         DO J=1,sNy
          DO I=1,sNx
           UERR(I,J,bi,bj)=(vIce(I,J,1,bi,bj)-vIce(I,J,3,bi,bj))
     &             * _maskS(I,J,1,bi,bj)
           S2=MAX(ABS(UERR(I,J,bi,bj)),S2)
          END DO
         END DO
         _GLOBAL_MAX_R8( S2, myThid )
        ENDDO
       ENDDO
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S2.GT.S2A) WFAV=WFAV2
       S2A=S2
       IF(S2.LT.LSR_ERROR) THEN
        ICOUNT2=M
cph(
cph        GO TO 9001
        phexit = 1
cph)
       END IF
      END IF

      CALL SEAICE_EXCH_UV ( uIce, vIce, myThid )

cph(
      END IF
cph)

9000  CONTINUE
cph 9001  CONTINUE
C ITERATION END -----------------------------------------------------

      IF ( debugLevel .GE. debLevB ) THEN
       _BEGIN_MASTER( myThid )
        write(*,'(A,I6,1P2E22.14)')' V lsr iters, error = ',ICOUNT2,S2
       _END_MASTER( myThid )
      ENDIF

C     APPLY MASKS
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          uIce(I,J,1,bi,bj)=uIce(I,J,1,bi,bj)* _maskW(I,J,1,bi,bj)
          vIce(I,J,1,bi,bj)=vIce(I,J,1,bi,bj)* _maskS(I,J,1,bi,bj)
         END DO
        END DO
       ENDDO
      ENDDO
CML      CALL SEAICE_EXCH_UV ( uIce, vIce, myThid )

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END
