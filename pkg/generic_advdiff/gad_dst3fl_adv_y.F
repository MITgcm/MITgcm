C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_y.F,v 1.15 2014/04/04 20:29:08 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3FL_ADV_Y(
     I           bi,bj,k, calcCFL, deltaTloc,
     I           vTrans, vFld,
     I           maskLocS, tracer,
     O           vT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_DST3FL_ADV_Y                              |
C     | o Compute Meridional advective Flux of Tracer using      |
C     |   3rd Order DST Sceheme with flux limiting               |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      LOGICAL calcCFL
      _RL deltaTloc
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL Rjm,Rj,Rjp,vCFL,d0,d1,psiP,psiM,thetaP,thetaM
      _RL thetaMax
      PARAMETER( thetaMax = 1.D+20 )

      DO i=1-OLx,sNx+OLx
       vT(i,1-OLy)=0. _d 0
       vT(i,2-OLy)=0. _d 0
       vT(i,sNy+OLy)=0. _d 0
      ENDDO
      DO j=1-OLy+2,sNy+OLy-1
       DO i=1-OLx,sNx+OLx
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     These lines make TAF create vectorizable code
        thetaP = 0. _d 0
        thetaM = 0. _d 0
#endif
        Rjp=(tracer(i,j+1)-tracer(i, j ))*maskLocS(i,j+1)
        Rj =(tracer(i, j )-tracer(i,j-1))*maskLocS(i, j )
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskLocS(i,j-1)

        vCFL = vFld(i,j)
        IF ( calcCFL ) vCFL = ABS( vFld(i,j)*deltaTloc
     &                  *recip_dyC(i,j,bi,bj)*recip_deepFacC(k) )
        d0=(2. _d 0 -vCFL)*(1. _d 0 -vCFL)*oneSixth
        d1=(1. _d 0 -vCFL*vCFL)*oneSixth

C-      the old version: can produce overflow, division by zero,
c       and is wrong for tracer with low concentration:
c       thetaP=Rjm/(1.D-20+Rj)
c       thetaM=Rjp/(1.D-20+Rj)
C-      the right expression, but not bounded:
c       thetaP=0.D0
c       thetaM=0.D0
c       IF (Rj.NE.0.D0) thetaP=Rjm/Rj
c       IF (Rj.NE.0.D0) thetaM=Rjp/Rj
C-      prevent |thetaP,M| to reach too big value:
        IF ( ABS(Rj)*thetaMax .LE. ABS(Rjm) ) THEN
          thetaP=SIGN(thetaMax,Rjm*Rj)
        ELSE
          thetaP=Rjm/Rj
        ENDIF
        IF ( ABS(Rj)*thetaMax .LE. ABS(Rjp) ) THEN
          thetaM=SIGN(thetaMax,Rjp*Rj)
        ELSE
          thetaM=Rjp/Rj
        ENDIF

        psiP=d0+d1*thetaP
        psiP=MAX(0. _d 0,MIN(MIN(1. _d 0,psiP),
     &                       thetaP*(1. _d 0 -vCFL)/(vCFL+1. _d -20) ))
        psiM=d0+d1*thetaM
        psiM=MAX(0. _d 0,MIN(MIN(1. _d 0,psiM),
     &                       thetaM*(1. _d 0 -vCFL)/(vCFL+1. _d -20) ))

        vT(i,j)=
     &   0.5*(vTrans(i,j)+ABS(vTrans(i,j)))
     &      *( Tracer(i,j-1) + psiP*Rj )
     &  +0.5*(vTrans(i,j)-ABS(vTrans(i,j)))
     &      *( Tracer(i, j ) - psiM*Rj )

       ENDDO
      ENDDO

      RETURN
      END
