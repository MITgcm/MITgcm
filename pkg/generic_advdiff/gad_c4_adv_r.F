C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c4_adv_r.F,v 1.3 2001/08/20 20:45:15 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_C4_ADV_R( 
     I           bi,bj,k,
     I           rTrans,
     I           tracer,
     O           wT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_C4_ADV_R                                  |
C     | o Compute vertical advective Flux of Tracer using        |
C     |   4th Order Centered Scheme                              |
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
      _RL maskP1

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)
      maskP1 = 1.
      IF (k.GE.Nr) maskP1 = 0.

      IF ( k.EQ.1 .OR. k.GT.Nr) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) = 0.
        ENDDO
       ENDDO
      ELSEIF ( k.EQ.2 ) THEN
         Rjjm= 0.
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         Rjp=(tracer(i,j,kp1,bi,bj)-tracer(i,j,k,bi,bj))
         Rj =(tracer(i,j,k,bi,bj)-tracer(i,j,km1,bi,bj))
         Rjjp=(Rjp-Rj)*maskC(i,j,kp1,bi,bj)*maskP1
         wT(i,j) = maskC(i,j,km1,bi,bj)*(
     &     rTrans(i,j)*(
     &        (Tracer(i,j,k,bi,bj)+Tracer(i,j,km1,bi,bj))*0.5 _d 0
     &        -oneSixth*(Rjjm+Rjjp)*0.5 _d 0 )
     &    +ABS(rTrans(i,j))*
     &         oneSixth*(Rjjm-Rjjp)*0.5 _d 0
     &                                  )                      
        ENDDO
       ENDDO
      ELSEIF ( k.EQ.Nr ) THEN
         Rjjp= 0.
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         Rj =(tracer(i,j,k,bi,bj)-tracer(i,j,km1,bi,bj))
         Rjm=(tracer(i,j,km1,bi,bj)-tracer(i,j,km2,bi,bj))
         Rjjm=(Rj-Rjm)*maskC(i,j,km2,bi,bj)
         wT(i,j) = maskC(i,j,km1,bi,bj)*(
     &     rTrans(i,j)*(
     &        (Tracer(i,j,k,bi,bj)+Tracer(i,j,km1,bi,bj))*0.5 _d 0
     &        -oneSixth*(Rjjm+Rjjp)*0.5 _d 0 )
     &    +ABS(rTrans(i,j))*
     &         oneSixth*(Rjjm-Rjjp)*0.5 _d 0
     &                                  )                      
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         Rjp=(tracer(i,j,kp1,bi,bj)-tracer(i,j,k,bi,bj))
         Rj =(tracer(i,j,k,bi,bj)-tracer(i,j,km1,bi,bj))
         Rjm=(tracer(i,j,km1,bi,bj)-tracer(i,j,km2,bi,bj))
         Rjjp=(Rjp-Rj)*maskC(i,j,kp1,bi,bj)
         Rjjm=(Rj-Rjm)*maskC(i,j,km2,bi,bj)
         wT(i,j) = maskC(i,j,km1,bi,bj)*(
     &     rTrans(i,j)*(
     &        (Tracer(i,j,k,bi,bj)+Tracer(i,j,km1,bi,bj))*0.5 _d 0
     &        -oneSixth*(Rjjm+Rjjp)*0.5 _d 0 )
     &    +ABS(rTrans(i,j))*
     &         oneSixth*(Rjjm-Rjjp)*0.5 _d 0
     &      *( 1. _d 0 - maskC(i,j,km2,bi,bj)*maskC(i,j,kp1,bi,bj) )
     &                                  )                      
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
