C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_fluxlimit_adv_x.F,v 1.8 2006/06/18 23:31:35 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_FLUXLIMIT_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_FLUXLIMIT_ADV_X(
     I           bi,bj,k,deltaTloc,
     I           uTrans, uFld,
     I           maskLocW, tracer,
     O           uT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated zonal flux due to advection of a tracer
C using second-order interpolation with a flux limiter:
C \begin{equation*}
C F^x_{adv} = U \overline{ \theta }^i
C - \frac{1}{2} \left(
C     [ 1 - \psi(C_r) ] |U|
C    + U \frac{u \Delta t}{\Delta x_c} \psi(C_r)
C              \right) \delta_i \theta
C \end{equation*}
C where the $\psi(C_r)$ is the limiter function and $C_r$ is
C the slope ratio.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uTrans               :: zonal volume transport
C  uFld                 :: zonal flow
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL deltaTloc
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT                   :: zonal advective flux
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Cr                   :: slope ratio
C  Rjm,Rj,Rjp           :: differences at i-1,i,i+1
C  uLoc                 :: velocity [m/s], zonal component
      INTEGER i,j
      _RL Cr,Rjm,Rj,Rjp
      _RL uLoc
C Statement function provides Limiter(Cr)
#include "GAD_FLUX_LIMITER.h"
CEOP

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
       DO i=1-Olx+2,sNx+Olx-1

c       uLoc = uFld(i,j)
        uLoc = uTrans(i,j)*recip_dyG(i,j,bi,bj)
     &       *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
        Rjp=(tracer(i+1,j)-tracer( i ,j))*maskLocW(i+1,j)
        Rj =(tracer( i ,j)-tracer(i-1,j))*maskLocW( i ,j)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskLocW(i-1,j)

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
     &        +uTrans(i,j)*uLoc*deltaTloc
     &         *recip_dxC(i,j,bi,bj)*Cr
     &        )*Rj
       ENDDO
      ENDDO

      RETURN
      END
