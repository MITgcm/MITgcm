C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_metric_nh.F,v 1.3 2007/03/12 23:54:26 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_METRIC_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_METRIC_NH(
     I               bi,bj,k,
     I               uFld, wFld,
     O               uMetricTerms,
     I               myThid )

C !DESCRIPTION:
C Calculates the zonal metric term due to non-hydrostaticity on the sphere:
C \begin{equation}
C -\frac{u}{a} \overline{w}^{ik}
C \end{equation}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal flow
C  wFld                 :: vertical flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uMetricTerms         :: metric term
      _RL uMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j,kp1
      _RL wVelBottomOverride
CEOP

      kp1=min(k+1,Nr)
      wVelBottomOverride=1.
      IF (k.EQ.Nr) wVelBottomOverride=0.

      DO j=1-Olx,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
         uMetricTerms(i,j) = uFld(i,j)*recip_rSphere*recip_deepFacC(k)
     &    *0.25*( (wFld(i-1,j,kp1,bi,bj)+wFld(i,j,kp1,bi,bj))
     &            *rVel2wUnit(kp1)*wVelBottomOverride
     &          + (wFld(i-1,j, k ,bi,bj)+wFld(i,j, k ,bi,bj))
     &            *rVel2wUnit( k )
     &          )*gravitySign
       ENDDO
      ENDDO

      RETURN
      END
