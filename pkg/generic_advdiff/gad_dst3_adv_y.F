C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3_adv_y.F,v 1.2 2001/09/10 00:14:05 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3_ADV_Y( 
     I           bi,bj,k,deltaT,
     I           vTrans, vVel,
     I           tracer,
     O           vT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_DST3_ADV_Y                                |
C     | o Compute Meridional advective Flux of Tracer using      |
C     |   3rd Order DST Sceheme                                  |
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
      _RL Rjm,Rj,Rjp,cfl,d0,d1
      _RL psiP,psiM,thetaP,thetaM

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
       vT(i,2-Oly)=0.
       vT(i,sNy+Oly)=0.
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j+1)-tracer(i,j))*maskS(i,j+1,k,bi,bj)
        Rj =(tracer(i,j)-tracer(i,j-1))*maskS(i,j,k,bi,bj)
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskS(i,j-1,k,bi,bj)

        cfl=abs(vVel(i,j,k,bi,bj)*deltaT*recip_dyc(i,j,bi,bj))
        d0=(2.-cfl)*(1.-cfl)*oneSixth
        d1=(1.-cfl*cfl)*oneSixth
c       thetaP=0.
c       IF (Rj.NE.0.) thetaP=Rjm/Rj
        thetaP=Rjm/(1.D-20+Rj)
        psiP=d0+d1*thetaP
c       psiP=max(0.,min(min(1.,psiP),(1.-cfl)/(1.D-20+cfl)*thetaP))
        thetaM=Rjp/(1.D-20+Rj)
c       thetaM=0.
c       IF (Rj.NE.0.) thetaM=Rjp/Rj
        psiM=d0+d1*thetaM
c       psiM=max(0.,min(min(1.,psiM),(1.-cfl)/(1.D-20+cfl)*thetaM))
        vT(i,j)=
     &   0.5*(vTrans(i,j)+abs(vTrans(i,j)))
     &      *( Tracer(i,j-1) + psiP*Rj )
     &  +0.5*(vTrans(i,j)-abs(vTrans(i,j)))
     &      *( Tracer(i, j ) - psiM*Rj )

       ENDDO
      ENDDO

      RETURN
      END
