C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_y.F,v 1.9 2006/06/07 01:55:14 heimbach Exp $
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
      _RL thetaMax
      PARAMETER( thetaMax = 1.D+20 )

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0. _d 0
       vT(i,2-Oly)=0. _d 0
       vT(i,sNy+Oly)=0. _d 0
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j+1)-tracer(i, j ))*maskLocS(i,j+1)
        Rj =(tracer(i, j )-tracer(i,j-1))*maskLocS(i, j )
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskLocS(i,j-1)

c       vFld = vVel(i,j,k,bi,bj)
        vFld = vTrans(i,j)*recip_dxG(i,j,bi,bj)
     &       *recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
        cfl=abs(vFld*deltaTloc*recip_dyC(i,j,bi,bj))
        d0=(2. _d 0 -cfl)*(1. _d 0 -cfl)*oneSixth
        d1=(1. _d 0 -cfl*cfl)*oneSixth

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
        psiP=MAX(0. _d 0, MIN(MIN(1. _d 0,psiP),
     &                        thetaP*(1. _d 0 -cfl)/(cfl+1. _d -20) ))
        psiM=d0+d1*thetaM
        psiM=MAX(0. _d 0, MIN(MIN(1. _d 0,psiM),
     &                        thetaM*(1. _d 0 -cfl)/(cfl+1. _d -20) ))

        vT(i,j)=
     &   0.5*(vTrans(i,j)+abs(vTrans(i,j)))
     &      *( Tracer(i,j-1) + psiP*Rj )
     &  +0.5*(vTrans(i,j)-abs(vTrans(i,j)))
     &      *( Tracer(i, j ) - psiM*Rj )

       ENDDO
      ENDDO

      RETURN
      END
