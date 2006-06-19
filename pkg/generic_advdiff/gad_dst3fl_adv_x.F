C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_x.F,v 1.11 2006/06/19 14:40:43 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3FL_ADV_X(
     I           bi,bj,k,deltaTloc,
     I           uTrans, uFld,
     I           maskLocW, tracer,
     O           uT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_DST3FL_ADV_X                              |
C     | o Compute Zonal advective Flux of Tracer using           |
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
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
C     uLoc   :: velocity [m/s], zonal component
      INTEGER i,j
      _RL Rjm,Rj,Rjp,cfl,d0,d1,psiP,psiM,thetaP,thetaM
      _RL uLoc
      _RL thetaMax
      PARAMETER( thetaMax = 1.D+20 )

C- jmc: an alternative would be to compute directly psiM*Rj & psiP*Rj
C       (if Rj*Rjm < 0 => psiP*Rj = 0 , elsef Rj > 0 ... , else  ... )
C       with no need to compute thetaM (might be easier to differentiate)

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0. _d 0
       uT(2-Olx,j)=0. _d 0
       uT(sNx+Olx,j)=0. _d 0
       DO i=1-Olx+2,sNx+Olx-1
        Rjp=(tracer(i+1,j)-tracer( i ,j))*maskLocW(i+1,j)
        Rj =(tracer( i ,j)-tracer(i-1,j))*maskLocW( i ,j)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskLocW(i-1,j)

        uLoc = uFld(i,j)
c       uLoc = uTrans(i,j)*recip_dyG(i,j,bi,bj)
c    &       *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
        cfl=abs(uLoc*deltaTloc*recip_dxC(i,j,bi,bj))
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

        uT(i,j)=
     &   0.5*(uTrans(i,j)+abs(uTrans(i,j)))
     &      *( Tracer(i-1,j) + psiP*Rj )
     &  +0.5*(uTrans(i,j)-abs(uTrans(i,j)))
     &      *( Tracer( i ,j) - psiM*Rj )

       ENDDO
      ENDDO

      RETURN
      END
