C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_u3_adv_y.F,v 1.5 2011/03/29 15:50:30 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_U3_ADV_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_U3_ADV_Y(
     I           bi,bj,k,
     I           vTrans, maskLocS,
     I           tracer,
     O           vT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional flux due to advection of a tracer
C using upwind biased third-order interpolation (or the $\kappa=1/3$ scheme):
C \begin{equation*}
C F^y_{adv} = V \overline{ \theta  - \frac{1}{6} \delta_{jj} \theta }^j
C                 + \frac{1}{12} |V| \delta_{jjj} \theta
C \end{equation*}
C Near boundaries, mask all the gradients ==> still 3rd O.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
c#include "GRID.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj        :: tile indices
C  k            :: vertical level
C  vTrans       :: meridional volume transport
C  maskLocS     :: mask (either 0 or 1) at grid-cell southern edge
C  tracer       :: tracer field
C  myThid       :: my thread Id number
      INTEGER bi,bj,k
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vT                   :: meridional advective flux
      _RL vT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Rjm,Rj,Rjp           :: differences at j-1,j,j+1
C  Rjjm,Rjjp            :: second differences at j-1,j
      INTEGER i,j
      _RL Rjm,Rj,Rjp,Rjjm,Rjjp
CEOP

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
       vT(i,2-Oly)=0.
       vT(i,sNy+Oly)=0.
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        Rjp = (tracer(i,j+1)-tracer(i, j ))*maskLocS(i,j+1)
        Rj  = (tracer(i, j )-tracer(i,j-1))*maskLocS(i, j )
        Rjm = (tracer(i,j-1)-tracer(i,j-2))*maskLocS(i,j-1)
        Rjjp=Rjp-Rj
        Rjjm=Rj-Rjm
        vT(i,j) =
     &   vTrans(i,j)*(
     &     Tracer(i,j)+Tracer(i,j-1)-oneSixth*( Rjjp+Rjjm )
     &               )*0.5 _d 0
     &  +ABS( vTrans(i,j) )*0.5 _d 0*oneSixth*( Rjjp-Rjjm )
       ENDDO
      ENDDO

      RETURN
      END
