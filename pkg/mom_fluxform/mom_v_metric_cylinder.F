C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_metric_cylinder.F,v 1.2 2004/07/07 18:34:07 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_METRIC_SPHERE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_METRIC_CYLINDER(
     I        bi,bj,k,
     I        uFld, vFld,
     O        vMetricTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the zonal metric term due to sphericity:
C \begin{equation}
C -\frac{v^2}{r}     
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
CEOP

C jmc: using yG make more sense to me but I might be wrong ?
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
          if (yC(i,j,bi,bj).GT. 0) then 
             vMetricTerms(i,j) = 
     &            -1.*0.25*(uFld(i,j) + uFld(i+1,j) +
     &            uFld(i,j-1)+uFld(i+1,j-1))*
     &            0.25*(uFld(i,j) + uFld(i+1,j) +
     &            uFld(i,j-1)+uFld(i+1,j-1))/yC(i,j,bi,bj)
          else
             vMetricTerms(i,j) = 0.
          endif
       ENDDO
      ENDDO

      RETURN
      END
