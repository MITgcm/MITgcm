C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_fluxlimit_adv_x.F,v 1.2 2001/07/11 22:46:32 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_FLUXLIMIT_ADV_X( 
     I           bi,bj,k,deltaT,
     I           uTrans, uVel,
     I           tracer,
     O           uT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_FLUXLIMIT_ADV_X                           |
C     | o Compute Zonal advective Flux of Tracer using           |
C     |   Flux Limiter Scheme                                    |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL deltaT
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL Cr,Rjm,Rj,Rjp
#include "GAD_FLUX_LIMITER.h"

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
       DO i=1-Olx+2,sNx+Olx-1
        Rjp=(tracer(i+1,j)-tracer(i,j))*maskW(i+1,j,k,bi,bj)
        Rj=(tracer(i,j)-tracer(i-1,j))*maskW(i,j,k,bi,bj)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskW(i-1,j,k,bi,bj)
        IF (Rj.NE.0.) THEN
         IF (uTrans(i,j).GT.0) THEN
           Cr=Rjm/Rj
         ELSE
           Cr=Rjp/Rj
         ENDIF
        ELSE
         IF (uTrans(i,j).GT.0) THEN
           Cr=Rjm*1.E20
         ELSE
           Cr=Rjp*1.E20
         ENDIF
        ENDIF
        Cr=Limiter(Cr)
        uT(i,j) = 
     &   uTrans(i,j)*(Tracer(i,j)+Tracer(i-1,j))*0.5 _d 0
     &   -0.5*(
     &        (1-Cr)*ABS(uTrans(i,j))
     &        +uTrans(i,j)*uVel(i,j,k,bi,bj)*deltaT
     &         *recip_dxC(i,j,bi,bj)*Cr
     &        )*Rj
       ENDDO
      ENDDO

      RETURN
      END
