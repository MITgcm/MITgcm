C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_y.F,v 1.7 2005/08/19 22:19:35 heimbach Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3FL_ADV_Y( 
     I           bi,bj,k,deltaTloc,
     I           vTrans, vVel,
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
      _RL deltaTloc
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
C     vFld    :: velocity [m/s], meridional component
      INTEGER i,j
      _RL Rjm,Rj,Rjp,cfl,d0,d1,psiP,psiM,thetaP,thetaM
      _RL vFld

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.D0
       vT(i,2-Oly)=0.D0
       vT(i,sNy+Oly)=0.D0
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j+1)-tracer(i, j ))*maskLocS(i,j+1)
        Rj =(tracer(i, j )-tracer(i,j-1))*maskLocS(i, j )
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskLocS(i,j-1)

c       vFld = vVel(i,j,k,bi,bj)
        vFld = vTrans(i,j)*recip_dxG(i,j,bi,bj)
     &       *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)
        cfl=abs(vFld*deltaTloc*recip_dyC(i,j,bi,bj))
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
