C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_u3_adv_r.F,v 1.2 2001/08/20 20:44:01 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_U3_ADV_R( 
     I           bi,bj,k,
     I           rTrans,
     I           tracer,
     O           wT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_U3_ADV_R                                  |
C     | o Compute vertical advective Flux of Tracer using        |
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
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,kp1,km1,km2
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp
      _RL maskM2, maskP1

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)
      maskM2 = 1.
      maskP1 = 1.
      IF (k.LE.2)  maskM2 = 0.
      IF (k.GE.Nr) maskP1 = 0.

      IF ( k.EQ.1 .OR. k.GT.Nr ) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         Rjp=(tracer(i,j,kp1,bi,bj)-tracer(i,j,k,bi,bj))
c    &        *maskC(i,j,kp1,bi,bj)
         Rj=(tracer(i,j,k,bi,bj)-tracer(i,j,km1,bi,bj))
         Rjm=(tracer(i,j,km1,bi,bj)-tracer(i,j,km2,bi,bj))
c    &        *maskC(i,j,km2,bi,bj)
C- jmc: mask not needed here above if Rjjp & Rjjm & rTrans are masked  
         Rjjp=(Rjp-Rj)*maskC(i,j,kp1,bi,bj)*maskP1
         Rjjm=(Rj-Rjm)*maskC(i,j,km2,bi,bj)*maskM2
         wT(i,j) = maskC(i,j,km1,bi,bj)*(
     &     rTrans(i,j)*(
     &        (Tracer(i,j,k,bi,bj)+Tracer(i,j,km1,bi,bj))*0.5 _d 0
     &        -oneSixth*(Rjjm+Rjjp)*0.5 _d 0  )
     &    +ABS(rTrans(i,j))*
     &         oneSixth*(Rjjm-Rjjp)*0.5 _d 0
     &                                  )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
