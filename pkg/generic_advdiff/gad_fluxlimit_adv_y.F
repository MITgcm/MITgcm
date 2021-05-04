#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_FLUXLIMIT_ADV_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_FLUXLIMIT_ADV_Y(
     I           bi, bj, k, calcCFL, deltaTloc,
     I           vTrans, vFld,
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
#include "EEPARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj            :: tile indices
C  k                 :: vertical level
C  calcCFL           :: =T: calculate CFL number ; =F: take vFld as CFL
C  deltaTloc         :: local time-step (s)
C  vTrans            :: meridional volume transport
C  vFld              :: meridional flow / CFL number
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi, bj, k
      LOGICAL calcCFL
      _RL deltaTloc
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vT                :: meridional advective flux
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j              :: loop indices
C  Cr                :: slope ratio
C  Rjm, Rj, Rjp      :: differences at j-1,j,j+1
      INTEGER i,j
      _RL Cr, Rjm, Rj, Rjp
      _RL vCFL
      _RL CrMax
      PARAMETER( CrMax = 1.D+6 )

C Statement function provides Limiter(Cr)
#include "GAD_FLUX_LIMITER.h"
CEOP

      DO i=1-OLx,sNx+OLx
       vT(i,1-OLy) = 0. _d 0
       vT(i,2-OLy) = 0. _d 0
       vT(i,sNy+OLy) = 0. _d 0
      ENDDO
      DO j=1-OLy+2,sNy+OLy-1
       DO i=1-OLx,sNx+OLx

        vCFL = vFld(i,j)
        IF ( calcCFL ) vCFL = ABS( vFld(i,j)*deltaTloc
     &                  *recip_dyC(i,j,bi,bj)*recip_deepFacC(k) )
        Rjp = (tracer(i,j+1)-tracer(i, j ))*maskLocS(i,j+1)
        Rj  = (tracer(i, j )-tracer(i,j-1))*maskLocS(i, j )
        Rjm = (tracer(i,j-1)-tracer(i,j-2))*maskLocS(i,j-1)

        IF ( vTrans(i,j).GT.zeroRL ) THEN
          Cr = Rjm
        ELSE
          Cr = Rjp
        ENDIF
        IF ( ABS(Rj)*CrMax .LE. ABS(Cr) ) THEN
          Cr = SIGN( CrMax, Cr )*SIGN( oneRL, Rj )
        ELSE
          Cr = Cr/Rj
        ENDIF

C       calculate Limiter Function:
        Cr = Limiter(Cr)

        vT(i,j) =
     &     vTrans(i,j)*(Tracer(i,j)+Tracer(i,j-1))*0.5 _d 0
     &   - ABS(vTrans(i,j))*( (oneRL-Cr) + vCFL*Cr )
     &                     *Rj*0.5 _d 0
       ENDDO
      ENDDO

      RETURN
      END
