C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_y.F,v 1.4 2001/11/08 23:39:34 heimbach Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3FL_ADV_Y( 
     I           bi,bj,k,deltaT,
     I           vTrans, vVel,
     I           tracer,
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
      _RL deltaT
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL Rjm,Rj,Rjp,cfl,d0,d1,psiP,psiM,thetaP,thetaM

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.D0
       vT(i,2-Oly)=0.D0
       vT(i,sNy+Oly)=0.D0
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j+1)-tracer(i,j))*maskS(i,j+1,k,bi,bj)
        Rj =(tracer(i,j)-tracer(i,j-1))*maskS(i,j,k,bi,bj)
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskS(i,j-1,k,bi,bj)

        cfl=abs(vVel(i,j,k,bi,bj)*deltaT*recip_dyc(i,j,bi,bj))
        d0=(2.D0-cfl)*(1.D0-cfl)*oneSixth
        d1=(1.D0-cfl*cfl)*oneSixth
c       thetaP=0.D0
c       IF (Rj.NE.0.D0) thetaP=Rjm/Rj
        thetaP=Rjm/(1.D-20+Rj)
        psiP=d0+d1*thetaP
        psiP=max(0.D0, min(min(1.D0,psiP),
     &       (1.D0-cfl)/(1.D-20+cfl)*thetaP))
        thetaM=Rjp/(1.D-20+Rj)
c       thetaM=0.D0
c       IF (Rj.NE.0.D0) thetaM=Rjp/Rj
        psiM=d0+d1*thetaM
        psiM=max(0.D0, min(min(1.D0,psiM),
     &       (1.D0-cfl)/(1.D-20+cfl)*thetaM))
        vT(i,j)=
     &   0.5*(vTrans(i,j)+abs(vTrans(i,j)))
     &      *( Tracer(i,j-1) + psiP*Rj )
     &  +0.5*(vTrans(i,j)-abs(vTrans(i,j)))
     &      *( Tracer(i, j ) - psiM*Rj )

       ENDDO
      ENDDO

      RETURN
      END
