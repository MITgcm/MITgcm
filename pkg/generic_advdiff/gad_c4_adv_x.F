C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c4_adv_x.F,v 1.2 2001/07/11 22:46:32 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_C4_ADV_X( 
     I           bi,bj,k,
     I           uTrans,
     I           tracer,
     O           uT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_C4_ADV_X                                  |
C     | o Compute Zonal advective Flux of Tracer using           |
C     |   4th Order Centered Scheme                              |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
       DO i=1-Olx+2,sNx+Olx-1
        Rjp=(tracer(i+1,j)-tracer(i,j))*maskW(i+1,j,k,bi,bj)
        Rj=(tracer(i,j)-tracer(i-1,j))*maskW(i,j,k,bi,bj)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskW(i-1,j,k,bi,bj)
        Rjjp=Rjp-Rj
        Rjjm=Rj-Rjm
        uT(i,j) = 
     &   uTrans(i,j)*(
     &     Tracer(i,j)+Tracer(i-1,j)-oneSixth*( Rjjp+Rjjm )
     &               )*0.5 _d 0
       ENDDO
      ENDDO

      RETURN
      END
