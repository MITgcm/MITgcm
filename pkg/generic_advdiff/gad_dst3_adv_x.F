C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3_adv_x.F,v 1.9 2006/06/19 14:40:43 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DST3_ADV_X(
     I           bi,bj,k,deltaTloc,
     I           uTrans, uFld,
     I           maskLocW, tracer,
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
#include "EEPARAMS.h"
#include "PARAMS.h"
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
      _RL Rjm,Rj,Rjp,cfl,d0,d1
      _RL psiP,psiM,thetaP,thetaM
      _RL uLoc
      _RL smallNo
c     _RL Rjjm,Rjjp

      IF (inAdMode) THEN
       smallNo = 1.0D-20
      ELSE
       smallNo = 1.0D-20
      ENDIF

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
       DO i=1-Olx+2,sNx+Olx-1
        Rjp=(tracer(i+1,j)-tracer( i ,j))*maskLocW(i+1,j)
        Rj =(tracer( i ,j)-tracer(i-1,j))*maskLocW( i ,j)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskLocW(i-1,j)

        uLoc = uFld(i,j)
c       uLoc = uTrans(i,j)*recip_dyG(i,j,bi,bj)
c    &       *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
        cfl=abs(uLoc*deltaTloc*recip_dxC(i,j,bi,bj))
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
        uT(i,j)=
     &   0.5*(uTrans(i,j)+abs(uTrans(i,j)))
     &      *( Tracer(i-1,j) + psiP*Rj )
     &  +0.5*(uTrans(i,j)-abs(uTrans(i,j)))
     &      *( Tracer( i ,j) - psiM*Rj )
#ifdef ALLOW_MATRIX
        ELSE
          uT(i,j)=
     &     0.5*(uTrans(i,j)+abs(uTrans(i,j)))
     &        *( Tracer(i-1,j) + (d0*Rj+d1*Rjm) )
     &    +0.5*(uTrans(i,j)-abs(uTrans(i,j)))
     &        *( Tracer( i ,j) - (d0*Rj+d1*Rjp) )
        ENDIF
#endif /* ALLOW_MATRIX */

       ENDDO
      ENDDO

      RETURN
      END
