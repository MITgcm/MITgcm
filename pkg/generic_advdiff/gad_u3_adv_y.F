C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_u3_adv_y.F,v 1.2 2001/08/20 20:44:01 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_U3_ADV_Y( 
     I           bi,bj,k,
     I           vTrans,
     I           tracer,
     O           vT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_U3_ADV_Y                                  |
C     | o Compute Meridional advective Flux of Tracer using      |
C     |   3rd Order Upwind Scheme                                |
C     | o reduced to 2nd or 3rd Order near a boundary            |
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
        Rjp=(tracer(i,j+1)-tracer(i,j))
c    &     *maskS(i,j+1,k,bi,bj)
        Rj =(tracer(i,j)-tracer(i,j-1))
c    &     *maskS(i,j,k,bi,bj)
        Rjm=(tracer(i,j-1)-tracer(i,j-2))
c    &     *maskS(i,j-1,k,bi,bj)
C- jmc: mask not needed here above if Rjjp & Rjjm & vTrans are masked
        Rjjp=(Rjp-Rj)*maskS(i,j+1,k,bi,bj)
        Rjjm=(Rj-Rjm)*maskS(i,j-1,k,bi,bj)
        vT(i,j) = 
     &   vTrans(i,j)*(
     &     Tracer(i,j)+Tracer(i,j-1)-oneSixth*( Rjjp+Rjjm )
     &               )*0.5 _d 0
     &  +ABS( vTrans(i,j) )*0.5 _d 0*oneSixth*( Rjjp-Rjjm )
       ENDDO
      ENDDO

      RETURN
      END
