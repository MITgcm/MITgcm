C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3_adv_x.F,v 1.2 2001/09/10 00:14:05 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3_ADV_X( 
     I           bi,bj,k,deltaT,
     I           uTrans, uVel,
     I           tracer,
     O           uT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_DST3_ADV_X                                |
C     | o Compute Zonal advective Flux of Tracer using           |
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
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL Rjm,Rj,Rjp,cfl,d0,d1
      _RL psiP,psiM,thetaP,thetaM

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
       DO i=1-Olx+2,sNx+Olx-1
        Rjp=(tracer(i+1,j)-tracer(i,j))*maskW(i+1,j,k,bi,bj)
        Rj =(tracer(i,j)-tracer(i-1,j))*maskW(i,j,k,bi,bj)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskW(i-1,j,k,bi,bj)

        cfl=abs(uVel(i,j,k,bi,bj)*deltaT*recip_dxc(i,j,bi,bj))
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
        uT(i,j)=
     &   0.5*(uTrans(i,j)+abs(uTrans(i,j)))
     &      *( Tracer(i-1,j) + psiP*Rj )
     &  +0.5*(uTrans(i,j)-abs(uTrans(i,j)))
     &      *( Tracer( i ,j) - psiM*Rj )

       ENDDO
      ENDDO

      RETURN
      END
