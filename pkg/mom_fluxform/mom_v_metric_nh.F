C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_v_metric_nh.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_METRIC_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_METRIC_NH(
     I        bi,bj,k,
     I        vFld,wFld,
     O        vMetricTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the zonal metric term due to non-hydrostaticity on the sphere:
C \begin{equation}
C -\frac{v}{a} \overline{w}^{jk} 
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
C  vFld                 :: merdional flow 
C  wFld                 :: vertical flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vMetricTerms         :: metric term
      _RL vMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j,Kp1
      _RL wVelBottomOverride
CEOP

      Kp1=min(k+1,Nr)
      wVelBottomOverride=1.
      IF (k.EQ.Nr) wVelBottomOverride=0.

      DO j=1-Olx+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
         vMetricTerms(i,j) = -vFld(i,j)*recip_RSphere
     &    *0.25*(wVelBottomOverride*
     &           (wFld(i,j,kp1,bi,bj)+wFld(i,j-1,kp1,bi,bj))
     &           +wFld(i,j, k ,bi,bj)+wFld(i,j-1, k ,bi,bj)
     &          )*rkFac*recip_horiVertRatio
       ENDDO
      ENDDO

      RETURN
      END
