C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c4_adv_y.F,v 1.2 2001/07/11 22:46:32 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_C4_ADV_Y( 
     I           bi,bj,k,
     I           vTrans,
     I           tracer,
     O           vT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_C4_ADV_Y                                  |
C     | o Compute Meridional advective Flux of Tracer using      |
C     |   4th Order Centered Scheme                              |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp


      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
       vT(i,2-Oly)=0.
       vT(i,sNy+Oly)=0.
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp=(tracer(i,j+1)-tracer(i,j))*maskS(i,j+1,k,bi,bj)
        Rj=(tracer(i,j)-tracer(i,j-1))*maskS(i,j,k,bi,bj)
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskS(i,j-1,k,bi,bj)
        Rjjp=Rjp-Rj
        Rjjm=Rj-Rjm
        vT(i,j) = 
     &   vTrans(i,j)*(
     &     Tracer(i,j)+Tracer(i,j-1)-oneSixth*( Rjjp+Rjjm )
     &               )*0.5 _d 0
       ENDDO
      ENDDO

      RETURN
      END
