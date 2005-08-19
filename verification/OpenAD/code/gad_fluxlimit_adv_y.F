C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code/Attic/gad_fluxlimit_adv_y.F,v 1.1 2005/08/19 21:48:51 heimbach Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_FLUXLIMIT_ADV_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_FLUXLIMIT_ADV_Y( 
     I           bi,bj,k,deltaTloc,
     I           vTrans, vVel,
     I           maskLocS, tracer,
     O           vT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional flux due to advection of a tracer
C using second-order interpolation with a flux limiter:
C \begin{equation*}
C F^y_{adv} = V \overline{ \theta }^j
C - \frac{1}{2} \left(
C     [ 1 - \psi(C_r) ] |V|
C    + V \frac{v \Delta t}{\Delta y_c} \psi(C_r)
C              \right) \delta_j \theta
C \end{equation*}
C where the $\psi(C_r)$ is the limiter function and $C_r$ is
C the slope ratio.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj             :: tile indices
C  k                 :: vertical level
C  vTrans            :: meridional volume transport
C  vVel              :: meridional flow
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj,k
      _RL deltaTloc
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vT                :: meridional advective flux
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  Cr                :: slope ratio
C  Rjm,Rj,Rjp        :: differences at j-1,j,j+1
C  vFld              :: velocity [m/s], meridional component
      INTEGER i,j
      _RL Cr,Rjm,Rj,Rjp
      _RL vFld
C Statement function provides Limiter(Cr)
#include "GAD_FLUX_LIMITER.h"
CEOP

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
       vT(i,2-Oly)=0.
       vT(i,sNy+Oly)=0.
      ENDDO
      DO j=1-Oly+2,sNy+Oly-1
       DO i=1-Olx,sNx+Olx

c       vFld = vVel(i,j,k,bi,bj)
        vFld = vTrans(i,j)*recip_dxG(i,j,bi,bj)
     &       *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)
        Rjp=(tracer(i,j+1)-tracer(i, j ))*maskLocS(i,j+1)
        Rj =(tracer(i, j )-tracer(i,j-1))*maskLocS(i, j )
        Rjm=(tracer(i,j-1)-tracer(i,j-2))*maskLocS(i,j-1)

        IF (Rj.NE.0.) THEN
         IF (vTrans(i,j).GT.0) THEN
           Cr=Rjm/Rj
         ELSE
           Cr=Rjp/Rj
         ENDIF
        ELSE
         IF (vTrans(i,j).GT.0) THEN
           Cr=Rjm*1.E20
         ELSE
           Cr=Rjp*1.E20
         ENDIF
        ENDIF
        Cr=max(0.D0,max(min(1.D0,2.D0*Cr),min(2.D0,Cr))) 
cph        Cr=Limiter(Cr)
        vT(i,j) = 
     &   vTrans(i,j)*(Tracer(i,j)+Tracer(i,j-1))*0.5 _d 0
     &   -0.5*(
     &        (1-Cr)*ABS(vTrans(i,j))
     &        +vTrans(i,j)*vFld*deltaTloc
     &         *recip_dyC(i,j,bi,bj)*Cr
     &        )*Rj
       ENDDO
      ENDDO

      RETURN
      END
