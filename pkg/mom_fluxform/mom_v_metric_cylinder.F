C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_metric_cylinder.F,v 1.4 2012/08/11 19:01:35 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_METRIC_CYLINDER

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_METRIC_CYLINDER(
     I        bi,bj,k,
     I        uFld, vFld,
     O        vMetricTerms,
     I        myThid )

C !DESCRIPTION:
C Calculates the meridional metric term due to cylinder curvature:
C \begin{equation}
C -\frac{u^2}{r}
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
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vMetricTerms         :: metric term
      _RL vMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      _RL radius
CEOP

      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx,sNx+OLx-1
          radius = (yG(i,j,bi,bj)+yG(i+1,j,bi,bj))*0.5 _d 0
C-   to get the same results as before:
c         radius = yC(i,j,bi,bj)
          IF ( radius.GT.0. ) THEN
             vMetricTerms(i,j) =
     &           -0.25*(uFld(i,j) + uFld(i+1,j) +
     &                  uFld(i,j-1)+uFld(i+1,j-1))
     &           *0.25*(uFld(i,j) + uFld(i+1,j) +
     &                  uFld(i,j-1)+uFld(i+1,j-1))/radius
          ELSE
             vMetricTerms(i,j) = 0.
          ENDIF
       ENDDO
      ENDDO

      RETURN
      END
