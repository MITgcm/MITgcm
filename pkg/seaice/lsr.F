C $Header: /u/gcmpack/MITgcm/pkg/seaice/lsr.F,v 1.16 2006/02/15 09:04:38 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE lsr( ilcall, myThid )
C     /==========================================================\
C     | SUBROUTINE  lsr                                          |
C     | o Solve ice momentum equation with an LSR dynamics solver|
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997     |
C     |    and Zhang and Rothrock, MWR, 131,  845- 861, 2003)    |
C     |   Written by Jinlun Zhang, PSC/UW, Feb-2001              |
C     |                     zhang@apl.washington.edu             |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
C#include "SEAICE_GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER ilcall
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_ALLOW_DYNAMICS

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, m, bi, bj, j1, j2, im, jm
      INTEGER ICOUNT1, ICOUNT2, SOLV_MAX_ITERS, SOLV_NCHECK
      INTEGER phexit

      _RL  RADIUS, RADIUS2
      _RL  WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL  AA1, AA2, AA3, AA4, AA5, AA6, S1, S2, S1A, S2A

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
      _RL ETAMEAN  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL ZETAMEAN (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CML      _RL DELXY    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CML      _RL DELXR    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CML      _RL DELYR    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CML      _RL DELX2    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CML      _RL DELY2    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL UVRT1    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL UVRT2    (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

      _RL vDiffX   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL vDiffY   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL uDiffY   (1-Olx:sNx+Olx,1-Oly:sNy+Oly)

C SET SOME VALUES
CML      RADIUS=rSphere
      RADIUS=6370. _d 3
CML      RADIUS2=recip_rSphere*recip_rSphere
      RADIUS2=ONE/(RADIUS*RADIUS)
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

C SOLVE FOR UICE

#ifdef ALLOW_AUTODIFF_TAMC
cph That's an important one! Note, that
cph * lsr is called twice, thus the icall index
cph * this storing is still outside the iteration loop
CADJ STORE uice = comlev1_lsr, 
CADJ &            key = ikey_dynamics + (ilcall-1)*nchklev_1
CADJ STORE vice = comlev1_lsr, 
CADJ &            key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &           +AMASS(I,J,bi,bj)/SEAICE_deltaTdyn*UICE(I,J,2,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &           +AMASS(I,J,bi,bj)/SEAICE_deltaTdyn*VICE(I,J,2,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)*UVM(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)*UVM(I,J,bi,bj)
CML          DELX2(I,J,bi,bj)=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
CML          DELY2(I,J,bi,bj)=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
CML          DELXY(I,J,bi,bj)=HALF/(DXUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
CML          DELXR(I,J,bi,bj)=HALF/(DXUICE(I,J,bi,bj)*RADIUS)
CML          DELYR(I,J,bi,bj)=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          etaPlusZeta(I,J,bi,bj) = ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA(I,J,bi,bj)
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          ETAMEAN(I,J,bi,bj) =QUART*(
     &          ETA(I,J-1,bi,bj) + ETA(I-1,J-1,bi,bj)
     &         +ETA(I,J  ,bi,bj) + ETA(I-1,J  ,bi,bj))
          ZETAMEAN(I,J,bi,bj)=QUART*(
     &          ZETA(I,J-1,bi,bj) + ZETA(I-1,J-1,bi,bj)
     &         +ZETA(I,J  ,bi,bj) + ZETA(I-1,J  ,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          AA1=( etaPlusZeta(I  ,J-1,bi,bj) * _recip_dxF(I  ,J-1,bi,bj)
     &         +etaPlusZeta(I  ,J  ,bi,bj) * _recip_dxF(I  ,J  ,bi,bj)
     &         )*0.5 _d 0 * _recip_dxV(I,J,bi,bj) * UVM(I,J,bi,bj)
          AA2=( etaPlusZeta(I-1,J-1,bi,bj) * _recip_dxF(I-1,J-1,bi,bj)
     &         +etaPlusZeta(I-1,J  ,bi,bj) * _recip_dxF(I-1,J  ,bi,bj)
     &         )*0.5 _d 0 * _recip_dxV(I,J,bi,bj) * UVM(I,J,bi,bj)
          AA3=  0.5 _d 0 *(ETA(I-1,J  ,bi,bj)+ETA(I,J  ,bi,bj))
          AA4=  0.5 _d 0 *(ETA(I-1,J-1,bi,bj)+ETA(I,J-1,bi,bj))
          AA5= -(AA3-AA4) * _tanPhiAtV(I,J,bi,bj) 
     &         * _recip_dyU(I,J,bi,bj)/RADIUS
          AA6=TWO*ETAMEAN(I,J,bi,bj) *RADIUS2
     &          * _tanPhiAtV(I,J,bi,bj)  * _tanPhiAtV(I,J,bi,bj)
          AU(I,J,bi,bj)=-AA2
          CU(I,J,bi,bj)=-AA1
          BU(I,J,bi,bj)=(ONE-UVM(I,J,bi,bj))
     &         - AU(I,J,bi,bj) - CU(I,J,bi,bj)
     &         + ((AA3+AA4)*_recip_dyU(I,J,bi,bj)*_recip_dyU(I,J,bi,bj)
     &           + AA5 + AA6
     &           + AMASS(I,J,bi,bj)/SEAICE_deltaTdyn
     &           + DRAGS(I,J,bi,bj)
     &           )*UVM(I,J,bi,bj)
         END DO
        END DO

        DO J=1,sNy
         AU(1,J,bi,bj)=ZERO
         CU(sNx,J,bi,bj)=ZERO
         CU(1,J,bi,bj)=CU(1,J,bi,bj)/BU(1,J,bi,bj)
        END DO

C     now set up right-hand side
        DO J=1-Oly,sNy+Oly-1
         DO I=1-Olx,sNx+Olx-1
          vDiffY(I,J) = 0.5 _d 0 * (
     &          VICEC(I+1,J+1,bi,bj) + VICEC(I  ,J+1,bi,bj)
     &         -VICEC(I+1,J  ,bi,bj) - VICEC(I  ,J  ,bi,bj) )
          vDiffX(I,J) = 0.5 _d 0 * (
     &          VICEC(I+1,J+1,bi,bj) + VICEC(I+1,J  ,bi,bj)
     &         -VICEC(I  ,J+1,bi,bj) - VICEC(I  ,J  ,bi,bj) )
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          FXY(I,J,bi,bj)=DRAGA(I,J,bi,bj)*VICEC(I,J,bi,bj)
     &         +FORCEX(I,J,bi,bj)
     &         + ( zetaMinusEta(I  ,J  ,bi,bj) * vDiffY(I  ,J  )
     &           + zetaMinusEta(I  ,J-1,bi,bj) * vDiffY(I  ,J-1)
     &           - zetaMinusEta(I-1,J  ,bi,bj) * vDiffY(I-1,J  )
     &           - zetaMinusEta(I-1,J-1,bi,bj) * vDiffY(I-1,J-1)
     &         )* 0.5 _d 0 * 
     &         _recip_dxV(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &
     &         + ( ETA         (I  ,J  ,bi,bj) * vDiffX(I  ,J  )
     &         * _recip_dxF(I  ,J,bi,bj)
     &           + ETA         (I-1,J  ,bi,bj) * vDiffX(I-1,J  )
     &         * _recip_dxF(I-1,J,bi,bj)
     &           - ETA         (I  ,J-1,bi,bj) * vDiffX(I  ,J-1)
     &         * _recip_dxF(I  ,J-1,bi,bj)
     &           - ETA         (I-1,J-1,bi,bj) * vDiffX(I-1,J-1)
     &         * _recip_dxF(I-1,J-1,bi,bj)
     &         ) * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)
     &
     &        -(etaPlusZeta(I  ,J  ,bi,bj)+etaPlusZeta(I  ,J-1,bi,bj)
     &         -etaPlusZeta(I-1,J-1,bi,bj)-etaPlusZeta(I-1,J  ,bi,bj))
     &         * VICEC(I,J,bi,bj)
     &            * _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dxV(I,J,bi,bj)/RADIUS
     &
     &         -(ETAMEAN(I,J,bi,bj)+ZETAMEAN(I,J,bi,bj))
     &         *(VICEC(I+1,J,bi,bj) - VICEC(I-1,J,bi,bj))
     &            * _tanPhiAtV(I,J,bi,bj)
     &         * 1.0 _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj) )
     &         /RADIUS
     &
     &         -ETAMEAN(I,J,bi,bj)
     &         *(VICEC(I+1,J,bi,bj) - VICEC(I-1,J,bi,bj))
     &            *TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 1.0 _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj) )
     &         /RADIUS

          UVRT1(I,J,bi,bj)=
     &         0.5 _d 0 * (ETA(I-1,J-1,bi,bj)+ETA(I,J-1,bi,bj))
     &         * _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         -     ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J-1,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
     &         + TWO*ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
          UVRT2(I,J,bi,bj)=
     &         0.5 _d 0 * (ETA(I-1,J,bi,bj)+ETA(I,J,bi,bj))
     &         * _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         +     ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J+1,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
     &         - TWO*ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
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
          UICE(I,J,3,bi,bj)=UICE(I,J,1,bi,bj)
         END DO
        END DO

        DO 1200 J=1,sNy
         DO I=1,sNx
          IF(I.EQ.1) THEN
           AA2=(etaPlusZeta(I-1,J-1,bi,bj) * _recip_dxF(I-1,J-1,bi,bj)
     &         +etaPlusZeta(I-1,J  ,bi,bj) * _recip_dxF(I-1,J  ,bi,bj)
     &          )*0.5 _d 0 * _recip_dxV(I,J,bi,bj)
           AA3=AA2*UICE(I-1,J,1,bi,bj)*UVM(I,J,bi,bj)
          ELSE IF(I.EQ.sNx) THEN
           AA1=(etaPlusZeta(I  ,J-1,bi,bj) * _recip_dxF(I  ,J-1,bi,bj)
     &         +etaPlusZeta(I  ,J  ,bi,bj) * _recip_dxF(I  ,J  ,bi,bj)
     &          )*0.5 _d 0 * _recip_dxV(I,J,bi,bj)
           AA3=AA1*UICE(I+1,J,1,bi,bj)*UVM(I,J,bi,bj)
          ELSE
           AA3=ZERO
          END IF
          URT(I)=FXY(I,J,bi,bj)+AA3
     &          +UVRT1(I,J,bi,bj)*UICE(I,J-1,1,bi,bj)
     &          +UVRT2(I,J,bi,bj)*UICE(I,J+1,1,bi,bj)
          URT(I)=URT(I)*UVM(I,J,bi,bj)
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
          UICE(I,J,1,bi,bj)=UICE(I,J,3,bi,bj)
     &        +WFAU*(URT(I)-UICE(I,J,3,bi,bj))
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
           UERR(I,J,bi,bj)=(UICE(I,J,1,bi,bj)-UICE(I,J,3,bi,bj))
     &             *UVM(I,J,bi,bj)
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
      CALL SEAICE_EXCH ( UICE, myThid )

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

C NOW FOR VICE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          AA1=0.5 _d 0 * _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         * (etaPlusZeta(I-1,J  ,bi,bj) + etaPlusZeta(I,J  ,bi,bj))
          AA2=0.5 _d 0 * _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         * (etaPlusZeta(I-1,J-1,bi,bj) + etaPlusZeta(I,J-1,bi,bj))
          AA3= (ETA(I  ,J-1,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         +ETA(I  ,J  ,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         )* 0.5 _d 0 * _recip_dxV(I,J,bi,bj)
          AA4= (ETA(I-1,J-1,bi,bj)+ETA(I-1,J,bi,bj))*0.5 _d 0
     &          *_recip_dxV(I,J,bi,bj) * _recip_dxV(I,J,bi,bj)
          AA5=(zetaMinusEta(I-1,J  ,bi,bj) + zetaMinusEta(I,J  ,bi,bj)
     &        -zetaMinusEta(I-1,J-1,bi,bj) - zetaMinusEta(I,J-1,bi,bj)
     &          )* _tanPhiAtV(I,J,bi,bj) 
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS

          AA6=TWO*ETAMEAN(I,J,bi,bj) * RADIUS2
     &         * _tanPhiAtV(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)

         AV(I,J,bi,bj)=(
     &         - AA2
     &         - (ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj))
     &          * _tanPhiAtV(I,J-1,bi,bj) 
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
     &         -ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
     &         )*UVM(I,J,bi,bj)
         CV(I,J,bi,bj)=(
     &         -AA1 
     &        +(ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj))
     &         * _tanPhiAtV(I,J+1,bi,bj) 
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
     &        +ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS
     &        )*UVM(I,J,bi,bj)
          BV(I,J,bi,bj)= (ONE-UVM(I,J,bi,bj))
     &        +( (AA1+AA2) + (AA3+AA4)  + AA5 + AA6
     &        +AMASS(I,J,bi,bj)/SEAICE_deltaTdyn+DRAGS(I,J,bi,bj))
     &        *UVM(I,J,bi,bj)
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
          uDiffY(I,J) = 0.5 _d 0 * (
     &          UICEC(I+1,J+1,bi,bj) + UICEC(I ,J+1,bi,bj)
     5         -UICEC(I+1,J  ,bi,bj) - UICEC(I ,J  ,bi,bj) )
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
           FXY(I,J,bi,bj)=-DRAGA(I,J,bi,bj)*UICEC(I,J,bi,bj)
     &        +FORCEY(I,J,bi,bj)
     &     
     &        + HALF*(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     &        *(zetaMinusEta(I-1,J  ,bi,bj)+zetaMinusEta(I,J  ,bi,bj)
     &         -zetaMinusEta(I-1,J-1,bi,bj)-zetaMinusEta(I,J-1,bi,bj))
     &         * 1. _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj))
     &         * _recip_dyU(I,J,bi,bj)
     &         
     &         +HALF*(ZETAMEAN(I,J,bi,bj) - ETAMEAN(I,J,bi,bj))
     &         *((UICEC(I+1,J+1,bi,bj) - UICEC(I-1,J+1,bi,bj))
     &         * 2. _d 0 /( _dxG(I,J+1,bi,bj) + _dxG(I-1,J+1,bi,bj))
     &          -(UICEC(I+1,J-1,bi,bj)-UICEC(I-1,J-1,bi,bj))
     &         * 2. _d 0 /( _dxG(I,J-1,bi,bj) + _dxG(I-1,J-1,bi,bj)))
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)
     &
     &        +(ETA(I  ,J  ,bi,bj) * uDiffY(I  ,J  )
     &         +ETA(I  ,J-1,bi,bj) * uDiffY(I  ,J-1)
     &         -ETA(I-1,J  ,bi,bj) * uDiffY(I-1,J  )
     &         -ETA(I-1,J-1,bi,bj) * uDiffY(I-1,J-1)
     &         )*0.5 _d 0* _recip_dxV(I,J,bi,bj)* _recip_dyU(I,J,bi,bj)
     &
     &        +(ETA(I  ,J  ,bi,bj) + ETA(I  ,J-1,bi,bj)
     &         -ETA(I-1,J-1,bi,bj) - ETA(I-1,J  ,bi,bj))
     &         * UICEC(I,J,bi,bj)
     &         * _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dxV(I,J,bi,bj)/RADIUS
     &        +ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &         *(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     &         * 0.5 _d 0 * _recip_dxV(I,J,bi,bj)/RADIUS
     &
     &        +ETAMEAN(I,J,bi,bj)*TWO  * _tanPhiAtV(I,J,bi,bj)
     &        *(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     &         * 1. _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj))
     &         /RADIUS
          UVRT1(I,J,bi,bj)= 0.5 _d 0 * (
     &           ETA(I-1,J-1,bi,bj) * _recip_dxV(I,J,bi,bj)
     &          +ETA(I-1,J  ,bi,bj) * _recip_dxV(I,J,bi,bj)
     &          ) * _recip_dxV(I,J,bi,bj)
          UVRT2(I,J,bi,bj)= 0.5 _d 0 * (
     &          ETA(I  ,J-1,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         +ETA(I  ,J  ,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         ) * _recip_dxV(I,J,bi,bj)

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
          VICE(I,J,3,bi,bj)=VICE(I,J,1,bi,bj) 
         END DO
        END DO

        DO I=1,sNx
         DO J=1,sNy
           IF(J.EQ.1) THEN
            AA2= _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &           * 0.5 _d 0 *( 
     &           etaPlusZeta(I-1,J-1,bi,bj) + etaPlusZeta(I,J-1,bi,bj)
     &           )
              AA3=( AA2
     &         +( ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj) )
     &         * _tanPhiAtV(I,J-1,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS 
     &         + ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS )
     &         *VICE(I,J-1,1,bi,bj)*UVM(I,J,bi,bj)
           ELSE IF(J.EQ.sNy) THEN
            AA1= _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &           * 0.5 _d 0 * (
     &           etaPlusZeta(I-1,J,bi,bj) + etaPlusZeta(I,J,bi,bj)
     &            )
            AA3=( AA1
     &         -( ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj))
     &         * _tanPhiAtV(I,J+1,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS 
     &         - ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)/RADIUS )
     &         *VICE(I,J+1,1,bi,bj)*UVM(I,J,bi,bj)
           ELSE
            AA3=ZERO
           END IF

          VRT(J)=FXY(I,J,bi,bj)+AA3+UVRT1(I,J,bi,bj)*VICE(I-1,J,1,bi,bj)
     &                       +UVRT2(I,J,bi,bj)*VICE(I+1,J,1,bi,bj)
          VRT(J)=VRT(J)*UVM(I,J,bi,bj)
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
          VICE(I,J,1,bi,bj)=VICE(I,J,3,bi,bj)
     &        +WFAV*(VRT(J)-VICE(I,J,3,bi,bj))
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
           UERR(I,J,bi,bj)=(VICE(I,J,1,bi,bj)-VICE(I,J,3,bi,bj))
     &             *UVM(I,J,bi,bj)
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

      CALL SEAICE_EXCH ( VICE, myThid )

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

C NOW END
C NOW MAKE COROLIS TERM IMPLICIT
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          UICE(I,J,1,bi,bj)=UICE(I,J,1,bi,bj)*UVM(I,J,bi,bj)
          VICE(I,J,1,bi,bj)=VICE(I,J,1,bi,bj)*UVM(I,J,bi,bj)
         END DO
        END DO
       ENDDO
      ENDDO
      CALL SEAICE_EXCH_UV ( UICE, VICE, myThid )

#endif /* SEAICE_ALLOW_DYNAMICS */

      RETURN
      END
