C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c4_adv_r.F,v 1.4 2001/09/21 13:11:43 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_C4_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_C4_ADV_R( 
     I           bi,bj,k,
     I           rTrans,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated vertical flux due to advection of a tracer
C using centered fourth-order interpolation:
C \begin{equation*}
C F^r_{adv} = W \overline{ \theta - \frac{1}{6} \delta_{kk} \theta }^k
C \end{equation*}
C Near boundaries, the scheme reduces to a second if the flow is away
C from the boundary and to third order if the flow is towards
C the boundary.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  rTrans               :: vertical volume transport
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                   :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  kp1                  :: =min( k+1 , Nr )
C  km1                  :: =max( k-1 , 1 )
C  km2                  :: =max( k-2 , 1 )
C  Rjm,Rj,Rjp           :: differences at i-1,i,i+1
C  Rjjm,Rjjp            :: second differences at i-1,i
C  maskP1               :: =1 for k<Nr, =0 for k>=Nr
      INTEGER i,j,kp1,km1,km2
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp
      _RL maskP1
CEOP

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
