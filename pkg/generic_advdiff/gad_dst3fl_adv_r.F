C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_r.F,v 1.2 2001/11/08 23:39:34 heimbach Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3FL_ADV_R( 
     I           bi_arg,bj_arg,k,dTarg,
     I           rTrans, wVel,
     I           tracer,
     O           wT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_DST3_ADV_R                                |
C     | o Compute Vertical advective Flux of Tracer using        |
C     |   3rd Order DST Sceheme with flux limiting               |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi_arg,bj_arg,k
      _RL dTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,kp1,km1,km2,bi,bj
      _RL Rjm,Rj,Rjp,cfl,d0,d1
      _RL psiP,psiM,thetaP,thetaM

      IF (.NOT. multiDimAdvection) THEN
C      If using the standard time-stepping/advection schemes (ie. AB-II)
C      then the data-structures are all global arrays
       bi=bi_arg
       bj=bj_arg
      ELSE
C      otherwise if using the multi-dimensional advection schemes
C      then the data-structures are all local arrays except
C      for maskC(...) and wVel(...)
       bi=1
       bj=1
      ENDIF

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j,k,bi,bj)-tracer(i,j,kp1,bi,bj))
     &         *maskC(i,j,kp1,bi,bj)
        Rj =(tracer(i,j,km1,bi,bj)-tracer(i,j,k,bi,bj))
     &         *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
        Rjm=(tracer(i,j,km2,bi,bj)-tracer(i,j,km1,bi,bj))
     &         *maskC(i,j,km1,bi,bj)

        cfl=abs(wVel(i,j,k,bi,bj)*dTarg*recip_drc(k))
        d0=(2.D0-cfl)*(1.-cfl)*oneSixth
        d1=(1.D0-cfl*cfl)*oneSixth
c       thetaP=0.D0
c       IF (Rj.NE.0.D0) thetaP=Rjm/Rj
        thetaP=Rjm/(1.D-20+Rj)
        psiP=d0+d1*thetaP
        psiP=max(0.D0,min(min(1.D0,psiP),
     &       (1.D0-cfl)/(1.D-20+cfl)*thetaP))
        thetaM=Rjp/(1.D-20+Rj)
c       thetaM=0.D0
c       IF (Rj.NE.0.D0) thetaM=Rjp/Rj
        psiM=d0+d1*thetaM
        psiM=max(0.D0,min(min(1.D0,psiM),
     &       (1.D0-cfl)/(1.D-20+cfl)*thetaM))
        wT(i,j)=
     &   0.5*(rTrans(i,j)+abs(rTrans(i,j)))
     &      *( Tracer(i,j, k ,bi,bj) + psiM*Rj )
     &  +0.5*(rTrans(i,j)-abs(rTrans(i,j)))
     &      *( Tracer(i,j,km1,bi,bj) - psiP*Rj )

       ENDDO
      ENDDO

      RETURN
      END
