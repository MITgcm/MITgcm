#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_FLUXLIMIT_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_FLUXLIMIT_ADV_X(
     I           bi, bj, k, calcCFL, deltaTloc,
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
#include "EEPARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj            :: tile indices
C  k                 :: vertical level
C  calcCFL           :: =T: calculate CFL number ; =F: take uFld as CFL
C  deltaTloc         :: local time-step (s)
C  uTrans            :: zonal volume transport
C  uFld              :: zonal flow / CFL number
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi, bj, k
      LOGICAL calcCFL
      _RL deltaTloc
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT                :: zonal advective flux
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j              :: loop indices
C  Cr                :: slope ratio
C  Rjm, Rj, Rjp      :: differences at i-1,i,i+1
      INTEGER i,j
      _RL Cr, Rjm, Rj, Rjp
      _RL uCFL
      _RL CrMax
      PARAMETER( CrMax = 1.D+6 )

C Statement function provides Limiter(Cr)
#include "GAD_FLUX_LIMITER.h"
CEOP

      DO j=1-OLy,sNy+OLy
       uT(1-OLx,j) = 0. _d 0
       uT(2-OLx,j) = 0. _d 0
       uT(sNx+OLx,j) = 0. _d 0
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx+2,sNx+OLx-1

        uCFL = uFld(i,j)
        IF ( calcCFL ) uCFL = ABS( uFld(i,j)*deltaTloc
     &                  *recip_dxC(i,j,bi,bj)*recip_deepFacC(k) )
        Rjp = (tracer(i+1,j)-tracer( i ,j))*maskLocW(i+1,j)
        Rj  = (tracer( i ,j)-tracer(i-1,j))*maskLocW( i ,j)
        Rjm = (tracer(i-1,j)-tracer(i-2,j))*maskLocW(i-1,j)

        IF ( uTrans(i,j).GT.zeroRL ) THEN
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

        uT(i,j) =
     &     uTrans(i,j)*(Tracer(i,j)+Tracer(i-1,j))*0.5 _d 0
     &   - ABS(uTrans(i,j))*( (oneRL-Cr) + uCFL*Cr )
     &                     *Rj*0.5 _d 0
       ENDDO
      ENDDO

      RETURN
      END
