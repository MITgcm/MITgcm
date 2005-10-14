C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3_adv_r.F,v 1.5 2005/10/14 21:51:05 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3_ADV_R( 
     I           bi_arg,bj_arg,k,dTarg,
     I           rTrans, wVel,
     I           tracer,
     O           wT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_DST3_ADV_R                                |
C     | o Compute Vertical advective Flux of Tracer using        |
C     |   3rd Order DST Sceheme                                  |
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
C     wFld     :: velocity, vertical component
      INTEGER i,j,kp1,km1,km2,bi,bj
      _RL Rjm,Rj,Rjp,cfl,d0,d1
      _RL psiP,psiM,thetaP,thetaM
      _RL wFld
      _RL smallNo
c     _RL Rjjm,Rjjp

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

      IF (inAdMode) THEN
       smallNo = 1.0D-20
      ELSE
       smallNo = 1.0D-20
      ENDIF

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j,k,bi,bj)-tracer(i,j,kp1,bi,bj))
     &         *maskC(i,j,kp1,bi_arg,bj_arg)
        Rj =(tracer(i,j,km1,bi,bj)-tracer(i,j,k,bi,bj))
     &         *maskC(i,j,k,bi_arg,bj_arg)*maskC(i,j,km1,bi_arg,bj_arg)
        Rjm=(tracer(i,j,km2,bi,bj)-tracer(i,j,km1,bi,bj))
     &         *maskC(i,j,km1,bi_arg,bj_arg)

c       wFld = wVel(i,j,k,bi_arg,bj_arg)
        wFld = rTrans(i,j)*recip_rA(i,j,bi_arg,bj_arg)
        cfl=abs(wFld*dTarg*recip_drC(k))
        d0=(2.-cfl)*(1.-cfl)*oneSixth
        d1=(1.-cfl*cfl)*oneSixth
#ifdef ALLOW_MATRIX
        IF (.NOT.useMATRIX) THEN
#endif /* ALLOW_MATRIX */        
        IF ( ABS(Rj).LT.smallNo .OR.
     &       ABS(Rjm).LT.smallNo ) THEN
         thetaP=0.
         psiP=0.
        ELSE
         thetaP=(Rjm+smallNo)/(smallNo+Rj)
         psiP=d0+d1*thetaP
        ENDIF
        IF ( ABS(Rj).LT.smallNo .OR.
     &       ABS(Rjp).LT.smallNo ) THEN
         thetaM=0.
         psiM=0.
        ELSE
         thetaM=(Rjp+smallNo)/(smallNo+Rj)
         psiM=d0+d1*thetaM
        ENDIF
         wT(i,j)=
     &    0.5*(rTrans(i,j)+abs(rTrans(i,j)))
     &       *( Tracer(i,j, k ,bi,bj) + psiM*Rj )
     &   +0.5*(rTrans(i,j)-abs(rTrans(i,j)))
     &       *( Tracer(i,j,km1,bi,bj) - psiP*Rj )
#ifdef ALLOW_MATRIX
        ELSE
          wT(i,j)=
     &     0.5*(rTrans(i,j)+abs(rTrans(i,j)))
     &        *( Tracer(i,j, k ,bi,bj) + (d0*Rj+d1*Rjp) )
     &    +0.5*(rTrans(i,j)-abs(rTrans(i,j)))
     &        *( Tracer(i,j,km1,bi,bj) - (d0*Rj+d1*Rjm) )
        ENDIF
#endif /* ALLOW_MATRIX */

       ENDDO
      ENDDO

      RETURN
      END
