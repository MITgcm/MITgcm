#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_FLUXLIMIT_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_FLUXLIMIT_ADV_R(
     I           bi, bj, k, dTarg,
     I           rTrans, wFld,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated vertical flux due to advection of a tracer
C using second-order interpolation with a flux limiter:
C \begin{equation*}
C F^x_{adv} = W \overline{ \theta }^k
C - \frac{1}{2} \left(
C     [ 1 - \psi(C_r) ] |W|
C    + W \frac{w \Delta t}{\Delta r_c} \psi(C_r)
C              \right) \delta_k \theta
C \end{equation*}
C where the $\psi(C_r)$ is the limiter function and $C_r$ is
C the slope ratio.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj             :: tile indices
C  k                  :: vertical level
C  rTrans             :: vertical volume transport
C  wFld               :: vertical flow
C  tracer             :: tracer field
C  myThid             :: thread number
      INTEGER bi, bj, k
      _RL dTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                 :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j               :: loop indices
C  kp1                :: =min( k+1 , Nr )
C  km1                :: =max( k-1 , 1 )
C  km2                :: =max( k-2 , 1 )
C  bi, bj             :: tile indices or (1,1) depending on use
C  Cr                 :: slope ratio
C  Rjm, Rj, Rjp       :: differences at k-1,k,k+1
C  wLoc               :: velocity, vertical component
      INTEGER i, j, kp1, km1, km2
      _RL Cr, Rjm, Rj, Rjp
      _RL wLoc, wCFL
      _RL CrMax
      PARAMETER( CrMax = 1.D+6 )

C Statement function provides Limiter(Cr)
#include "GAD_FLUX_LIMITER.h"
CEOP

      km2 = MAX(1,k-2)
      km1 = MAX(1,k-1)
      kp1 = MIN(Nr,k+1)

      IF ( k.GT.Nr) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         wT(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx

         wLoc = wFld(i,j)
         wCFL = ABS( wLoc*dTarg*recip_drC(k) )
         Rjp = (tracer(i,j,kp1)-tracer(i,j,k))
     &          *maskC(i,j,kp1,bi,bj)
         Rj  = (tracer(i,j,k)  -tracer(i,j,km1))
         Rjm = (tracer(i,j,km1)-tracer(i,j,km2))
     &          *maskC(i,j,km2,bi,bj)

         IF ( rTrans(i,j).LT.zeroRL ) THEN
           Cr = Rjm
         ELSE
           Cr = Rjp
         ENDIF
         IF ( ABS(Rj)*CrMax .LE. ABS(Cr) ) THEN
           Cr = SIGN( CrMax, Cr )*SIGN( oneRL, Rj )
         ELSE
           Cr = Cr/Rj
         ENDIF

C        calculate Limiter Function:
         Cr = Limiter(Cr)

         wT(i,j) = maskC(i,j,km1,bi,bj)*(
     &      rTrans(i,j)*
     &        (tracer(i,j,k)+tracer(i,j,km1))*0.5 _d 0
     &    + ABS(rTrans(i,j))*( (oneRL-Cr) + wCFL*Cr )
     &                      *Rj*0.5 _d 0 )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
