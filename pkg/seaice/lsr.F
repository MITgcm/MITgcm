C $Header: /u/gcmpack/MITgcm/pkg/seaice/lsr.F,v 1.35 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#ifndef SEAICE_LSRBNEW
C     for an alternative discretization of d/dx[ (zeta-eta) dV/dy]
C     and d/dy[ (zeta-eta) dU/dx] uncomment this option
C#define SEAICE_TEST
#endif

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CStartOfInterface
      SUBROUTINE lsr( ilcall, myThid )
C     *==========================================================*
C     | SUBROUTINE  lsr                                          |
C     | o Solve ice momentum equation with an LSR dynamics solver|
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997     |
C     |    and Zhang and Rothrock, MWR, 131,  845- 861, 2003)    |
C     |   Written by Jinlun Zhang, PSC/UW, Feb-2001              |
C     |                     zhang@apl.washington.edu             |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
C#include "SEAICE_GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER ilcall
      INTEGER myThid
CEndOfInterface

#ifndef SEAICE_CGRID
#ifdef SEAICE_ALLOW_DYNAMICS

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, m, bi, bj, j1, j2, im, jm
      INTEGER ICOUNT1, ICOUNT2

      _RL WFAU, WFAV, WFAU1, WFAV1, WFAU2, WFAV2
      _RL AA3, S1, S2, S1A, S2A

      _RL e11loc, e22loc, e12loc

      _RL COSWAT
      _RS SINWAT
      _RL ECM2, DELT1, DELT2
      _RL TEMPVAR

C     diagonals of coefficient matrices
      _RL AU   (1:sNx,1:sNy,nSx,nSy)
      _RL BU   (1:sNx,1:sNy,nSx,nSy)
      _RL CU   (1:sNx,1:sNy,nSx,nSy)
      _RL AV   (1:sNx,1:sNy,nSx,nSy)
      _RL BV   (1:sNx,1:sNy,nSx,nSy)
      _RL CV   (1:sNx,1:sNy,nSx,nSy)

C     coefficients for lateral points, u(j+/-1)
      _RL uRt1(1:sNx,1:sNy,nSx,nSy)
      _RL uRt2(1:sNx,1:sNy,nSx,nSy)
C     coefficients for lateral points, v(i+/-1)
      _RL vRt1(1:sNx,1:sNy,nSx,nSy)
      _RL vRt2(1:sNx,1:sNy,nSx,nSy)
C     RHS
      _RL rhsU (1:sNx,1:sNy,nSx,nSy)
      _RL rhsV (1:sNx,1:sNy,nSx,nSy)
C     symmetric and anti-symmetric drag coefficients
      _RL DRAGS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL DRAGA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SEAICE_LSRBNEW
      LOGICAL doIterate4u, doIterate4v
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     coefficients of ice velocities in coefficient matrix
C     for both U and V-equation
C     XX: double derivative in X
C     YY: double derivative in Y
C     XM: metric term with derivative in X
C     YM: metric term with derivative in Y
      _RL UXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL UYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL UXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL UYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VXX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VYY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VXM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VYM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     abbreviations
      _RL etaU (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zetaU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL etaV (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zetaV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     contribution of sigma on righ hand side
      _RL sig11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig21(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     auxillary fields
      _RL CUU  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL CVV  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL URT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL VRT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#else
      _RL URT(1-OLx:sNx+OLx), CUU(1-OLx:sNx+OLx)
      _RL VRT(1-OLy:sNy+OLy), CVV(1-OLy:sNy+OLy)

      _RL etaPlusZeta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL zetaMinusEta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ETAMEAN  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ZETAMEAN (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL dVdx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dVdy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dUdx     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dUdy     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef SEAICE_TEST
      _RL uz     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vz     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

      INTEGER phexit

      _RL  AA1, AA2, AA4, AA5, AA6

#endif /* SEAICE_LSRBNEW */

      _RL UERR

C     abbreviations
      _RL ucLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vcLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

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

      ECM2= 1. _d 0/(SEAICE_eccen**2)

      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          ucLoc(I,J,bi,bj) = 0.25 _d 0 * (
     &         +  uIceC(I+1,J,bi,bj) +  uIceC(I+1,J+1,bi,bj)
     &         +  uIceC(I  ,J,bi,bj) +  uIceC(I,  J+1,bi,bj) )
          vcLoc(I,J,bi,bj) = 0.25 _d 0 * (
     &         +  vIceC(I+1,J,bi,bj) +  vIceC(I+1,J+1,bi,bj)
     &         +  vIceC(I  ,J,bi,bj) +  vIceC(I,  J+1,bi,bj) )
         ENDDO
        ENDDO
        DO J=1-OLy,sNy+OLy-1
         DO I=1-OLy,sNx+OLy-1
          e11loc = 0.5 _d 0 * _recip_dxF(I,J,bi,bj) *
     &                (uIceC(I+1,J+1,bi,bj)+uIceC(I+1,J,bi,bj)
     &                -uIceC(I,  J+1,bi,bj)-uIceC(I,  J,bi,bj))
     &                + vcLoc(I,J,bi,bj) * k2AtC(I,J,bi,bj)
          e22loc = 0.5 _d 0 * _recip_dyF(I,J,bi,bj) *
     &                (vIceC(I+1,J+1,bi,bj)+vIceC(I,J+1,bi,bj)
     &                -vIceC(I+1,J,  bi,bj)-vIceC(I,J,  bi,bj))
     &                + ucLoc(I,J,bi,bj) * k1AtC(I,J,bi,bj)
          e12loc = 0.5 _d 0*(
     &           0.5 _d 0 * _recip_dyF(I,J,bi,bj) *
     &         (uIceC(I+1,J+1,bi,bj)+uIceC(I,J+1,bi,bj)
     &         -uIceC(I+1,J,  bi,bj)-uIceC(I,J,  bi,bj))
     &         + 0.5 _d 0 * _recip_dxF(I,J,bi,bj) *
     &         (vIceC(I+1,J+1,bi,bj)+vIceC(I+1,J,bi,bj)
     &         -vIceC(I,  J+1,bi,bj)-vIceC(I,  J,bi,bj))
     &         - vcLoc(I,J,bi,bj)*k1AtC(I,J,bi,bj)
     &         - ucLoc(I,J,bi,bj)*k2AtC(I,J,bi,bj) )
C  NOW EVALUATE VISCOSITIES
          DELT1=(e11loc**2+e22loc**2)*(1. _d 0+ECM2)
     &         +4.0 _d 0*ECM2*e12loc**2
     &         +2.0 _d 0*e11loc*e22loc*(1. _d 0-ECM2)
          IF ( DELT1 .LE. SEAICE_EPS_SQ ) THEN
             DELT2=SEAICE_EPS
          ELSE
             DELT2=SQRT(DELT1)
          ENDIF
          ZETA(I,J,bi,bj)  = 0.5 _d 0*PRESS0(I,J,bi,bj)/DELT2
C NOW PUT MIN AND MAX VISCOSITIES IN
          ZETA(I,J,bi,bj)  = MIN(ZMAX(I,J,bi,bj),ZETA(I,J,bi,bj))
          ZETA(I,J,bi,bj)  = MAX(ZMIN(I,J,bi,bj),ZETA(I,J,bi,bj))
C NOW SET VISCOSITIES TO ZERO AT HEFFMFLOW PTS
          ZETA(I,J,bi,bj)  = ZETA(I,J,bi,bj)*HEFFM(I,J,bi,bj)
          ETA(I,J,bi,bj)   = ECM2*ZETA(I,J,bi,bj)
          PRESS(I,J,bi,bj) = 2.0 _d 0*ZETA(I,J,bi,bj)*DELT2
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
C NOW SET UP NON-LINEAR WATER DRAG, FORCEX, FORCEY
          TEMPVAR=(uIce(I,J,bi,bj)-GWATX(I,J,bi,bj))**2
     &           +(vIce(I,J,bi,bj)-GWATY(I,J,bi,bj))**2
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
C NOW SET UP SYMMETTRIC DRAG
          DRAGS(I,J,bi,bj)=DWATN(I,J,bi,bj)*COSWAT
C NOW SET UP ANTI SYMMETTRIC DRAG PLUS CORIOLIS
          DRAGA(I,J,bi,bj)=DWATN(I,J,bi,bj)
     &         *SIGN(SINWAT, _fCori(I,J,bi,bj))
     &         + AMASS(I,J,bi,bj) * _fCoriG(I,J,bi,bj)
C NOW ADD IN CURRENT FORCE
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+DWATN(I,J,bi,bj)
     &         *(COSWAT*GWATX(I,J,bi,bj)
     &         -SIGN(SINWAT, _fCori(I,J,bi,bj))*GWATY(I,J,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+DWATN(I,J,bi,bj)
     &         *(SIGN(SINWAT, _fCori(I,J,bi,bj))*GWATX(I,J,bi,bj)
     &         +COSWAT*GWATY(I,J,bi,bj))
#ifndef SEAICE_LSRBNEW
C NOW CALCULATE PRESSURE FORCE AND ADD TO EXTERNAL FORCE
C     only for old code, in the new code the pressure force
C     is added to the rhs stress tensor terms
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &          -QUART * _recip_dxV(I,J,bi,bj)
     &          *(PRESS(I,  J,bi,bj) + PRESS(I,  J-1,bi,bj)
     &          - PRESS(I-1,J,bi,bj) - PRESS(I-1,J-1,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         -QUART * _recip_dyU(I,J,bi,bj)
     &          *(PRESS(I,J,  bi,bj) + PRESS(I-1,J,  bi,bj)
     &          - PRESS(I,J-1,bi,bj) - PRESS(I-1,J-1,bi,bj))
#endif /* not SEAICE_LSRBNEW */
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         +AMASS(I,J,bi,bj)/SEAICE_deltaTdyn*uIceNm1(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +AMASS(I,J,bi,bj)/SEAICE_deltaTdyn*vIceNm1(I,J,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)*UVM(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)*UVM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef SEAICE_LSRBNEW
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     coefficients for matrices
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          etaU(I,J) = 0.5 _d 0 * (
     &         +  eta(I,J,bi,bj) +  eta(I-1,J,bi,bj) )
          zetaU(I,J)= 0.5 _d 0 *(
     &         + zeta(I,J,bi,bj) + zeta(I-1,J,bi,bj) )
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          etaV(I,J) = 0.5 _d 0 * (
     &         +  eta(I,J,bi,bj) +  eta(I,J-1,bi,bj) )
          zetaV(I,J)= 0.5 _d 0 *(
     &         + zeta(I,J,bi,bj) + zeta(I,J-1,bi,bj) )
         ENDDO
        ENDDO
C     coefficients of uIce(I,J) and vIce(I,J) belonging to ...
        DO J=1,sNy
         DO I=0,sNx
C     ... d/dx (eta+zeta) d/dx u
          UXX(I,J) = _dyC(I,J,bi,bj) * (zetaV(I,J)+etaV(I,J))
     &         * _recip_dxG(I,J,bi,bj)
C     ... d/dx (zeta-eta) k1 u
          UXM(I,J) = _dyC(I,J,bi,bj) * (zetaV(I,J)-etaV(I,J))
     &         * k1AtV(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=0,sNy
         DO I=1,sNx
C     ... d/dy eta d/dy u
          UYY(I,J) = _dxC(I,J,bi,bj) * etaU(I,J)
     &         * _recip_dyG(I,J,bi,bj)
C     ... d/dy eta k2 u
          UYM(I,J) = _dxC(I,J,bi,bj) * etaU(I,J)
     &         * k2AtU(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=0,sNx
C     ... d/dx eta dv/dx
          VXX(I,J) = _dyC(I,J,bi,bj) * etaV(I,J)
     &         * _recip_dxG(I,J,bi,bj)
C     ... d/dx eta k1 v
          VXM(I,J) = _dyC(I,J,bi,bj) * etaV(I,J)
     &         * k1AtV(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO
        DO J=0,sNy
         DO I=1,sNx
C     ... d/dy eta+zeta dv/dy
          VYY(I,J) = _dxC(I,J,bi,bj) * (zetaU(I,J)+etaU(I,J))
     &         * _recip_dyG(I,J,bi,bj)
C     ... d/dy (zeta-eta) k2 v
          VYM(I,J) = _dxC(I,J,bi,bj) * (zetaU(I,J)-etaU(I,J))
     &         * k2AtU(I,J,bi,bj) * 0.5 _d 0
         ENDDO
        ENDDO

C     assemble coefficient matrix of uIce
C     beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of U(I,J+/-1) are counted on the right hand side
        DO J=1,sNy
         DO I=1,sNx
C     coefficients for uIce(I-1,J)
          AU(I,J,bi,bj)= ( - UXX(I-1,J) + UXM(I-1,J) )
     &         * UVM(I,J,bi,bj)
C     coefficients for uIce(I+1,J)
          CU(I,J,bi,bj)= ( - UXX(I  ,J) - UXM(I  ,J) )
     &         * UVM(I,J,bi,bj)
C     coefficients for uIce(I,J)
          BU(I,J,bi,bj)=(ONE - UVM(I,J,bi,bj)) +
     &         ( UXX(I-1,J) + UXX(I,J) + UYY(I,J-1) + UYY(I,J)
     &         + UXM(I-1,J) - UXM(I,J) - UYM(I,J-1) + UYM(I,J)
     &         ) * UVM(I,J,bi,bj)
C     coefficients of uIce(I,J-1)
          uRt1(I,J,bi,bj)= UYY(I,J-1) + UYM(I,J-1)
C     coefficients of uIce(I,J+1)
          uRt2(I,J,bi,bj)= UYY(I,J  ) - UYM(I,J  )
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=1,sNy
         DO I=1,sNx
          AU(I,J,bi,bj)    = AU(I,J,bi,bj)    * recip_rAz(I,J,bi,bj)
          CU(I,J,bi,bj)    = CU(I,J,bi,bj)    * recip_rAz(I,J,bi,bj)
C     here we need add in the contribution from the time derivative
C     and the symmetric drag term; must be done after normalizing
          BU(I,J,bi,bj)    = BU(I,J,bi,bj)    * recip_rAz(I,J,bi,bj)
     &         + UVM(I,J,bi,bj) *
     &         ( AMASS(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + DRAGS(I,J,bi,bj) )
          uRt1(I,J,bi,bj) = uRt1(I,J,bi,bj) * recip_rAz(I,J,bi,bj)
          uRt2(I,J,bi,bj) = uRt2(I,J,bi,bj) * recip_rAz(I,J,bi,bj)
         ENDDO
        ENDDO

C     assemble coefficient matrix of vIce
C     beware of sign convention: because this
C     is the left hand side we calculate -grad(sigma), but the coefficients
C     of V(I,J+/-1) are counted on the right hand side
        DO J=1,sNy
         DO I=1,sNx
C     coefficients for vIce(I,J-1)
          AV(I,J,bi,bj)=( - VYY(I,J-1) + VYM(I,J-1)
     &         ) * UVM(I,J,bi,bj)
C     coefficients for vIce(I,J+1)
          CV(I,J,bi,bj)=( - VYY(I,J  ) - VYM(I,J  )
     &         ) * UVM(I,J,bi,bj)
C     coefficients for vIce(I,J)
          BV(I,J,bi,bj)= (ONE - UVM(I,J,bi,bj)) +
     &         ( VXX(I,J) + VXX(I-1,J) + VYY(I,J) + VYY(I,J-1)
     &         + VXM(I,J) - VXM(I-1,J) - VYM(I,J) + VYM(I,J-1)
     &         ) * UVM(I,J,bi,bj)
C     coefficients for V(I-1,J)
          vRt1(I,J,bi,bj) = VXX(I-1,J) + VXM(I-1,J)
C     coefficients for V(I+1,J)
          vRt2(I,J,bi,bj) = VXX(I  ,J) - VXM(I  ,J)
         ENDDO
        ENDDO

C     now we need to normalize everything by the grid cell area
        DO J=1,sNy
         DO I=1,sNx
          AV(I,J,bi,bj)    = AV(I,J,bi,bj)    * recip_rAz(I,J,bi,bj)
          CV(I,J,bi,bj)    = CV(I,J,bi,bj)    * recip_rAz(I,J,bi,bj)
C     here we need add in the contribution from the time derivative
C     and the symmetric drag term; must be done after normalizing
          BV(I,J,bi,bj)    = BV(I,J,bi,bj)    * recip_rAz(I,J,bi,bj)
     &         + UVM(I,J,bi,bj) *
     &         ( AMASS(I,J,bi,bj)/SEAICE_deltaTdyn
     &         + DRAGS(I,J,bi,bj) )
          vRt1(I,J,bi,bj) = vRt1(I,J,bi,bj) * recip_rAz(I,J,bi,bj)
          vRt2(I,J,bi,bj) = vRt2(I,J,bi,bj) * recip_rAz(I,J,bi,bj)
         ENDDO
        ENDDO

C     set up right-hand sides
        DO j=1,sNy
         DO i=0,sNx
C     at V-point
          sig11(I,J) =
     &           (zetaV(I,J)-etaV(I,J))
     &         * (vcLoc(I,J,bi,bj)-vcLoc(I,J-1,bi,bj))
     &         * _recip_dyC(I,J,bi,bj)
     &         + (zetaV(I,J)+etaV(I,J))*k2atV(I,J,bi,bj)
     &         * 0.5 _d 0 * (vIceC(I,J,bi,bj)+vIceC(I+1,J,bi,bj))
     &         - 0.5 _d 0 * (PRESS(I,J,bi,bj)+PRESS(I,J-1,bi,bj))
          sig12(I,J) = etaV(I,J)
     &         * (ucLoc(I,J,bi,bj)-ucLoc(I,J-1,bi,bj))
     &         * _recip_dyC(I,J,bi,bj)
     &         - etaV(I,J) * k2AtV(I,J,bi,bj)
     &         * 0.5 _d 0 * (uIceC(I,J,bi,bj)+uIceC(I+1,J,bi,bj))
         ENDDO
        ENDDO
        DO j=0,sNy
         DO i=1,sNx
C     at U-point
          sig22(I,J) =
     &           (zetaU(I,J)-etaU(I,J))
     &         * (ucLoc(I,J,bi,bj)-ucLoc(I-1,J,bi,bj))
     &         * _recip_dxC(I,J,bi,bj)
     &         + (zetaU(I,J)+etaU(I,J))*k2atU(I,J,bi,bj)
     &         * 0.5 _d 0 * (uIceC(I,J,bi,bj)+uIceC(I,J+1,bi,bj))
     &         - 0.5 _d 0 * (PRESS(I,J,bi,bj)+PRESS(I-1,J,bi,bj))
          sig21(I,J) = etaU(I,J)
     &         * (vcLoc(I,J,bi,bj)-vcLoc(I-1,J,bi,bj))
     &         * _recip_dxC(I,J,bi,bj)
     &         - etaU(I,J) * k1AtU(I,J,bi,bj)
     &         * 0.5 _d 0 * (vIceC(I,J,bi,bj)+vIceC(I,J+1,bi,bj))
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
          rhsU(I,J,bi,bj) =   DRAGA(I,J,bi,bj)*vIceC(I,J,bi,bj)
     &         +FORCEX(I,J,bi,bj)
     &         + ( _dyC(I,  J,  bi,bj) * sig11(I,  J  )
     &           - _dyC(I-1,J,  bi,bj) * sig11(I-1,J  )
     &           + _dxC(I,  J,  bi,bj) * sig21(I,  J  )
     &           - _dxC(I,  J-1,bi,bj) * sig21(I,  J-1) )
     &         * recip_rAz(I,J,bi,bj) * UVM(I,J,bi,bj)
          rhsV(I,J,bi,bj) = - DRAGA(I,J,bi,bj)*uIceC(I,J,bi,bj)
     &         +FORCEY(I,J,bi,bj)
     &         + ( _dyC(I,  J,  bi,bj) * sig12(I,  J  )
     &           - _dyC(I-1,J,  bi,bj) * sig12(I-1,J  )
     &           + _dxC(I,  J,  bi,bj) * sig22(I,  J  )
     &           - _dxC(I,  J-1,bi,bj) * sig22(I,  J-1) )
     &         * recip_rAz(I,J,bi,bj) * UVM(I,J,bi,bj)
         ENDDO
        ENDDO
C     bi/bj-loops
       ENDDO
      ENDDO

C NOW DO ITERATION

      doIterate4u = .TRUE.
      doIterate4v = .TRUE.

C ITERATION START -----------------------------------------------------

      DO m = 1, SOLV_MAX_ITERS
       IF ( doIterate4u .OR. doIterate4v ) THEN

        IF ( useCubedSphereExchange ) THEN
          doIterate4u = .TRUE.
          doIterate4v = .TRUE.
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C-jmc: get less TAF warnings when always (no if doIterate) saving uIce,vIce:
C     save uIce prior to iteration, NOW SET U(3)=U(1)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uTmp(I,J,bi,bj)=uIce(I,J,bi,bj)
           ENDDO
          ENDDO
C     save vIce prior to iteration, NOW SET V(3)=V(1)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vTmp(I,J,bi,bj)=vIce(I,J,bi,bj)
           ENDDO
          ENDDO

          IF ( doIterate4u ) THEN
C Solve for uIce :
           DO J=1,sNy
            DO I=1,sNx
             AA3 = ZERO
             IF (I.EQ.1)   AA3 = AA3 - AU(I,J,bi,bj)*uIce(I-1,J,bi,bj)
             IF (I.EQ.sNx) AA3 = AA3 - CU(I,J,bi,bj)*uIce(I+1,J,bi,bj)

             URT(I,J)=rhsU(I,J,bi,bj)
     &            + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &            + uRt1(I,J,bi,bj)*uTmp(I,J-1,bi,bj)
     &            + uRt2(I,J,bi,bj)*uTmp(I,J+1,bi,bj)
#else
     &            + uRt1(I,J,bi,bj)*uIce(I,J-1,bi,bj)
     &            + uRt2(I,J,bi,bj)*uIce(I,J+1,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
             URT(I,J)=URT(I,J)* UVM(I,J,bi,bj)
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
             uIce(I,J,bi,bj)=uTmp(I,J,bi,bj)
     &           +WFAU*(URT(I,J)-uTmp(I,J,bi,bj))
            ENDDO
           ENDDO
C--   end doIterate4u
          ENDIF

          IF ( doIterate4v ) THEN
C Solve for vIce
           DO I=1,sNx
            DO J=1,sNy
             AA3 = ZERO
             IF (J.EQ.1)   AA3 = AA3 - AV(I,J,bi,bj)*vIce(I,J-1,bi,bj)
             IF (J.EQ.sNy) AA3 = AA3 - CV(I,J,bi,bj)*vIce(I,J+1,bi,bj)

             VRT(I,J)=rhsV(I,J,bi,bj)
     &            + AA3
#ifdef SEAICE_VECTORIZE_LSR
     &            + vRt1(I,J,bi,bj)*vTmp(I-1,J,bi,bj)
     &            + vRt2(I,J,bi,bj)*vTmp(I+1,J,bi,bj)
#else
     &            + vRt1(I,J,bi,bj)*vIce(I-1,J,bi,bj)
     &            + vRt2(I,J,bi,bj)*vIce(I+1,J,bi,bj)
#endif /* SEAICE_VECTORIZE_LSR */
             VRT(I,J)=VRT(I,J)* UVM(I,J,bi,bj)
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
             vIce(I,J,bi,bj)=vTmp(I,J,bi,bj)
     &           +WFAV*(VRT(I,J)-vTmp(I,J,bi,bj))
            ENDDO
           ENDDO
C--   end doIterate4v
          ENDIF

C     end bi,bj-loops
         ENDDO
        ENDDO

        IF ( doIterate4u.AND.MOD(m,SOLV_NCHECK).EQ.0) THEN
         S1=ZERO
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=1,sNy
            DO I=1,sNx
             UERR=(uIce(I,J,bi,bj)-uTmp(I,J,bi,bj))
     &               * UVM(I,J,bi,bj)
             S1=MAX(ABS(UERR),S1)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _GLOBAL_MAX_RL( S1, myThid )
c        WRITE(standardMessageUnit,'(A,2I6,1P4E16.9)')
c    &   ' U iters,error,WF = ',ilcall,M,S1,S1A,WFAU
C SAFEGUARD AGAINST BAD FORCING ETC
         IF(m.GT.1.AND.S1.GT.S1A) WFAU=WFAU2
         S1A=S1
         IF(S1.LT.LSR_ERROR) THEN
          ICOUNT1=m
          doIterate4u = .FALSE.
         ENDIF
        ENDIF

        IF ( doIterate4v.AND.MOD(m,SOLV_NCHECK).EQ.0) THEN
         S2=ZERO
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=1,sNy
            DO I=1,sNx
             UERR=(vIce(I,J,bi,bj)-vTmp(I,J,bi,bj))
     &               * UVM(I,J,bi,bj)
             S2=MAX(ABS(UERR),S2)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _GLOBAL_MAX_RL( S2, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
         IF(m.GT.1.AND.S2.GT.S2A) WFAV=WFAV2
         S2A=S2
         IF(S2.LT.LSR_ERROR) THEN
          ICOUNT2=m
          doIterate4v = .FALSE.
         ENDIF
        ENDIF

        CALL EXCH_UV_XY_RL( uIce, vIce,.TRUE.,myThid)

C--    end doIterate4u or doIterate4v
       ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &        'Uice post iter (SEAICE_LSR', MOD(ilcall,1000), ')'
        CALL DEBUG_STATS_RL( 1, uIce, msgBuf, myThid )
        WRITE(msgBuf,'(A,I3,A)')
     &        'Vice post iter (SEAICE_LSR', MOD(ilcall,1000), ')'
        CALL DEBUG_STATS_RL( 1, vIce, msgBuf, myThid )
      ENDIF
#endif /* ALLOW_DEBUG */
      IF ( doIterate4u .OR. doIterate4v ) THEN
        WRITE(msgBuf,'(2A,I10,A,I4,A)') '** WARNING ** SEAICE_LSR ',
     &    '(it=', -9999, ',', ilcall,') did not converge :'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2(A,I6,0PF6.3,1PE14.6))')
     &      ' nIt,wFU,dU=', ICOUNT1, WFAU, S1,
     &    ' ; nIt,wFV,dV=', ICOUNT2, WFAV, S2
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

#else /* SEAICE_LSRBNEW */
C SOLVE FOR UICE

#ifdef ALLOW_AUTODIFF_TAMC
cph That is an important one! Note, that
cph * lsr is called twice, thus the icall index
cph * this storing is still outside the iteration loop
CADJ STORE uice = comlev1_dynsol,
CADJ &            key = ikey_dynamics + (ilcall-1)*nchklev_1
CADJ STORE vice = comlev1_dynsol,
CADJ &            key = ikey_dynamics + (ilcall-1)*nchklev_1
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          etaPlusZeta(I,J,bi,bj) = ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)
          zetaMinusEta(I,J,bi,bj) = ZETA(I,J,bi,bj)-ETA(I,J,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
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
     &         * _recip_dyU(I,J,bi,bj)*recip_rSphere
          AA6=TWO*ETAMEAN(I,J,bi,bj) *recip_rSphere*recip_rSphere
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
        DO J=1-OLy,sNy+OLy-1
         DO I=1-OLx,sNx+OLx-1
          dVdy(I,J) = 0.5 _d 0 * (
     &         ( VICEC(I+1,J+1,bi,bj) - VICEC(I+1,J  ,bi,bj) )
     &         * _recip_dyG(I+1,J,bi,bj)
     &         +(VICEC(I  ,J+1,bi,bj) - VICEC(I  ,J  ,bi,bj) )
     &         * _recip_dyG(I,  J,bi,bj) )
          dVdx(I,J) = 0.5 _d 0 * (
     &         ( VICEC(I+1,J+1,bi,bj) - VICEC(I  ,J+1,bi,bj) )
     &         * _recip_dxG(I,J+1,bi,bj)
     &         +(VICEC(I+1,J  ,bi,bj) - VICEC(I  ,J  ,bi,bj) )
     &         * _recip_dxG(I,J,  bi,bj) )
         ENDDO
        ENDDO
#ifdef SEAICE_TEST
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          vz(i,j) = quart * (
     &         vicec(i,j,bi,bj) + vicec(i+1,j,bi,bj) )
          vz(i,j)= vz(i,j) + quart * (
     &         vicec(i,j+1,bi,bj) + vicec(i+1,j+1,bi,bj) )
         ENDDO
        ENDDO
#endif
        DO J=1,sNy
         DO I=1,sNx
          rhsU(I,J,bi,bj)=DRAGA(I,J,bi,bj)*VICEC(I,J,bi,bj)
     &         +FORCEX(I,J,bi,bj)
#ifdef SEAICE_TEST
     &        + ( 0.5 _d 0 *
     &         (zetaMinusEta(i,j,bi,bj)+zetaMinusEta(i,j-1,bi,bj))
     &         *(vz(i,j)-vz(i,j-1)) * _recip_dyC(i,j,bi,bj)
     &          - 0.5 _d 0 *
     &         (zetaMinusEta(i-1,j,bi,bj)+zetaMinusEta(i-1,j-1,bi,bj))
     &         *(vz(i-1,j)-vz(i-1,j-1)) * _recip_dyC(i-1,j,bi,bj)
     &         ) * _recip_dxV(i,j,bi,bj)
#else
     &         + ( zetaMinusEta(I  ,J  ,bi,bj) * dVdy(I  ,J  )
     &           + zetaMinusEta(I  ,J-1,bi,bj) * dVdy(I  ,J-1)
     &           - zetaMinusEta(I-1,J  ,bi,bj) * dVdy(I-1,J  )
     &           - zetaMinusEta(I-1,J-1,bi,bj) * dVdy(I-1,J-1)
     &         )* 0.5 _d 0 * _recip_dxV(I,J,bi,bj)
#endif
     &
     &         + ( ETA         (I  ,J  ,bi,bj) * dVdx(I  ,J  )
     &           + ETA         (I-1,J  ,bi,bj) * dVdx(I-1,J  )
     &           - ETA         (I  ,J-1,bi,bj) * dVdx(I  ,J-1)
     &           - ETA         (I-1,J-1,bi,bj) * dVdx(I-1,J-1)
     &         ) * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)
     &
     &        -(etaPlusZeta(I  ,J  ,bi,bj)+etaPlusZeta(I  ,J-1,bi,bj)
     &         -etaPlusZeta(I-1,J-1,bi,bj)-etaPlusZeta(I-1,J  ,bi,bj))
     &         * VICEC(I,J,bi,bj)
     &            * _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dxV(I,J,bi,bj)*recip_rSphere
     &
     &         -(ETAMEAN(I,J,bi,bj)+ZETAMEAN(I,J,bi,bj))
     &         *(VICEC(I+1,J,bi,bj) - VICEC(I-1,J,bi,bj))
     &            * _tanPhiAtV(I,J,bi,bj)
     &         * 1.0 _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj) )
     &         *recip_rSphere
     &
     &         -ETAMEAN(I,J,bi,bj)
     &         *(VICEC(I+1,J,bi,bj) - VICEC(I-1,J,bi,bj))
     &            *TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 1.0 _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj) )
     &         *recip_rSphere

          URT1(I,J,bi,bj)=
     &         0.5 _d 0 * (ETA(I-1,J-1,bi,bj)+ETA(I,J-1,bi,bj))
     &         * _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         -     ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J-1,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &         + TWO*ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
          URT2(I,J,bi,bj)=
     &         0.5 _d 0 * (ETA(I-1,J,bi,bj)+ETA(I,J,bi,bj))
     &         * _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &         +     ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J+1,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &         - TWO*ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
         END DO
        END DO

       ENDDO
      ENDDO

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
        DO J=1,sNy
         DO I=1,sNx
          UTMP(I,J,bi,bj)=UICE(I,J,bi,bj)
         END DO
        END DO

        DO J=1,sNy
         DO I=1,sNx
          IF(I.EQ.1) THEN
           AA2=(etaPlusZeta(I-1,J-1,bi,bj) * _recip_dxF(I-1,J-1,bi,bj)
     &         +etaPlusZeta(I-1,J  ,bi,bj) * _recip_dxF(I-1,J  ,bi,bj)
     &          )*0.5 _d 0 * _recip_dxV(I,J,bi,bj)
           AA3=AA2*UICE(I-1,J,bi,bj)*UVM(I,J,bi,bj)
          ELSE IF(I.EQ.sNx) THEN
           AA1=(etaPlusZeta(I  ,J-1,bi,bj) * _recip_dxF(I  ,J-1,bi,bj)
     &         +etaPlusZeta(I  ,J  ,bi,bj) * _recip_dxF(I  ,J  ,bi,bj)
     &          )*0.5 _d 0 * _recip_dxV(I,J,bi,bj)
           AA3=AA1*UICE(I+1,J,bi,bj)*UVM(I,J,bi,bj)
          ELSE
           AA3=ZERO
          END IF
          URT(I)=rhsU(I,J,bi,bj)+AA3
     &          +URT1(I,J,bi,bj)*UICE(I,J-1,bi,bj)
     &          +URT2(I,J,bi,bj)*UICE(I,J+1,bi,bj)
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
          UICE(I,J,bi,bj)=UTMP(I,J,bi,bj)
     &        +WFAU*(URT(I)-UTMP(I,J,bi,bj))
         END DO

        END DO

       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       S1=ZERO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           UERR=(UICE(I,J,bi,bj)-UTMP(I,J,bi,bj))
     &             *UVM(I,J,bi,bj)
           S1=MAX(ABS(UERR),S1)
          END DO
         END DO
        ENDDO
       ENDDO
       _GLOBAL_MAX_RL( S1, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S1.GT.S1A) WFAU=WFAU2
       S1A=S1
       IF(S1.LT.LSR_ERROR) THEN
        ICOUNT1=M
        phexit = 1
       END IF
      END IF
      _EXCH_XY_RL( UICE, myThid )

      ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

      IF ( debugLevel .GE. debLevC ) THEN
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
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere

          AA6=TWO*ETAMEAN(I,J,bi,bj) * recip_rSphere*recip_rSphere
     &         * _tanPhiAtV(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)

         AV(I,J,bi,bj)=(
     &         - AA2
     &         - (ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj))
     &          * _tanPhiAtV(I,J-1,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &         -ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &          * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &         )*UVM(I,J,bi,bj)
         CV(I,J,bi,bj)=(
     &         -AA1
     &        +(ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj))
     &         * _tanPhiAtV(I,J+1,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &        +ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
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
        DO J=1-OLy,sNy+OLy-1
         DO I=1-OLx,sNx+OLx-1
          dUdx(I,J) = 0.5 _d 0 * (
     &         ( UICEC(I+1,J+1,bi,bj) - UICEC(I ,J+1,bi,bj) )
     &         * _recip_dxG(I,J+1,bi,bj)
     &         +(UICEC(I+1,J  ,bi,bj) - UICEC(I ,J  ,bi,bj) )
     &         * _recip_dxG(I,J  ,bi,bj) )
          dUdy(I,J) = 0.5 _d 0 * (
     &          ( UICEC(I+1,J+1,bi,bj) - UICEC(I+1,J  ,bi,bj) )
     &         * _recip_dyG(I+1,J,bi,bj)
     &          +(UICEC(I  ,J+1,bi,bj) - UICEC(I  ,J  ,bi,bj) )
     &         * _recip_dyG(I,  J,bi,bj) )
         ENDDO
        ENDDO
#ifdef SEAICE_TEST
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          uz(i,j) = quart * (
     &         uicec(i,j,bi,bj) + uicec(i+1,j,bi,bj) )
          uz(i,j)= uz(i,j) + quart * (
     &         uicec(i,j+1,bi,bj) + uicec(i+1,j+1,bi,bj) )
         ENDDO
        ENDDO
#endif
        DO J=1,sNy
         DO I=1,sNx
           rhsV(I,J,bi,bj)=-DRAGA(I,J,bi,bj)*UICEC(I,J,bi,bj)
     &        +FORCEY(I,J,bi,bj)
     &
#ifdef SEAICE_TEST
     &        + ( 0.5 _d 0 *
     &         (zetaMinusEta(i,j,bi,bj)+zetaMinusEta(i-1,j,bi,bj))
     &         *(uz(i,j)-uz(i-1,j)) * _recip_dxC(i,j,bi,bj)
     &          - 0.5 _d 0 *
     &         (zetaMinusEta(i,j-1,bi,bj)+zetaMinusEta(i-1,j-1,bi,bj))
     &         *(uz(i,j-1)-uz(i-1,j-1)) * _recip_dxC(i,j-1,bi,bj)
     &         ) * _recip_dyU(i,j,bi,bj)
#else
     &         + ( zetaMinusEta(I  ,J  ,bi,bj) * dUdx(I  ,J  )
     &           + zetaMinusEta(I-1,J  ,bi,bj) * dUdx(I-1,J  )
     &           - zetaMinusEta(I  ,J-1,bi,bj) * dUdx(I  ,J-1)
     &           - zetaMinusEta(I-1,J-1,bi,bj) * dUdx(I-1,J-1)
     &         )* 0.5 _d 0 * _recip_dyU(I,J,bi,bj)
#endif
     &
     &        + ( ETA          (I  ,J  ,bi,bj) * dUdy(I  ,J  )
     &         +  ETA          (I  ,J-1,bi,bj) * dUdy(I  ,J-1)
     &         -  ETA          (I-1,J  ,bi,bj) * dUdy(I-1,J  )
     &         -  ETA          (I-1,J-1,bi,bj) * dUdy(I-1,J-1)
     &         )*0.5 _d 0* _recip_dxV(I,J,bi,bj)
     &
     &        +(ETA(I  ,J  ,bi,bj) + ETA(I  ,J-1,bi,bj)
     &         -ETA(I-1,J-1,bi,bj) - ETA(I-1,J  ,bi,bj))
     &         * UICEC(I,J,bi,bj)
     &         * _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dxV(I,J,bi,bj)*recip_rSphere
     &        +ETAMEAN(I,J,bi,bj) * _tanPhiAtV(I,J,bi,bj)
     &         *(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     &         * 0.5 _d 0 * _recip_dxV(I,J,bi,bj)*recip_rSphere
     &
     &        +ETAMEAN(I,J,bi,bj)*TWO  * _tanPhiAtV(I,J,bi,bj)
     &        *(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     &         * 1. _d 0 /( _dxG(I,J,bi,bj) + _dxG(I-1,J,bi,bj))
     &         *recip_rSphere
          VRT1(I,J,bi,bj)= 0.5 _d 0 * (
     &           ETA(I-1,J-1,bi,bj) * _recip_dxV(I,J,bi,bj)
     &          +ETA(I-1,J  ,bi,bj) * _recip_dxV(I,J,bi,bj)
     &          ) * _recip_dxV(I,J,bi,bj)
          VRT2(I,J,bi,bj)= 0.5 _d 0 * (
     &          ETA(I  ,J-1,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         +ETA(I  ,J  ,bi,bj) * _recip_dxV(I,J,bi,bj)
     &         ) * _recip_dxV(I,J,bi,bj)

         END DO
        END DO

       ENDDO
      ENDDO

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

C NOW SET U(3)=U(1)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          VTMP(I,J,bi,bj)=VICE(I,J,bi,bj)
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
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &         + ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere )
     &         *VICE(I,J-1,bi,bj)*UVM(I,J,bi,bj)
           ELSE IF(J.EQ.sNy) THEN
            AA1= _recip_dyU(I,J,bi,bj) * _recip_dyU(I,J,bi,bj)
     &           * 0.5 _d 0 * (
     &           etaPlusZeta(I-1,J,bi,bj) + etaPlusZeta(I,J,bi,bj)
     &            )
            AA3=( AA1
     &         -( ZETAMEAN(I,J,bi,bj)-ETAMEAN(I,J,bi,bj))
     &         * _tanPhiAtV(I,J+1,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere
     &         - ETAMEAN(I,J,bi,bj)*TWO* _tanPhiAtV(I,J,bi,bj)
     &         * 0.5 _d 0 * _recip_dyU(I,J,bi,bj)*recip_rSphere )
     &         *VICE(I,J+1,bi,bj)*UVM(I,J,bi,bj)
           ELSE
            AA3=ZERO
           END IF

          VRT(J)=rhsV(I,J,bi,bj)+AA3+VRT1(I,J,bi,bj)*VICE(I-1,J,bi,bj)
     &                       +VRT2(I,J,bi,bj)*VICE(I+1,J,bi,bj)
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
          VICE(I,J,bi,bj)=VTMP(I,J,bi,bj)
     &        +WFAV*(VRT(J)-VTMP(I,J,bi,bj))
         END DO
        ENDDO

       ENDDO
      ENDDO

      IF(MOD(M,SOLV_NCHECK).EQ.0) THEN
       S2=ZERO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           UERR=(VICE(I,J,bi,bj)-VTMP(I,J,bi,bj))
     &             *UVM(I,J,bi,bj)
           S2=MAX(ABS(UERR),S2)
          END DO
         END DO
        ENDDO
       ENDDO
       _GLOBAL_MAX_RL( S2, myThid )
C SAFEGUARD AGAINST BAD FORCING ETC
       IF(M.GT.1.AND.S2.GT.S2A) WFAV=WFAV2
       S2A=S2
       IF(S2.LT.LSR_ERROR) THEN
        ICOUNT2=M
        phexit = 1
       END IF
      END IF

      _EXCH_XY_RL( VICE, myThid )

      ENDIF
      ENDDO
C ITERATION END -----------------------------------------------------

      IF ( debugLevel .GE. debLevC ) THEN
       _BEGIN_MASTER( myThid )
        write(*,'(A,I6,1P2E22.14)')' V lsr iters, error = ',ICOUNT2,S2
       _END_MASTER( myThid )
      ENDIF

#endif /* SEAICE_LSRBNEW */

C NOW END
C NOW MAKE COROLIS TERM IMPLICIT
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          UICE(I,J,bi,bj)=UICE(I,J,bi,bj)*UVM(I,J,bi,bj)
          VICE(I,J,bi,bj)=VICE(I,J,bi,bj)*UVM(I,J,bi,bj)
         END DO
        END DO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */

      RETURN
      END
