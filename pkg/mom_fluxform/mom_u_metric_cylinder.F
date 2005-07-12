C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_metric_cylinder.F,v 1.3 2005/07/12 00:38:31 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_METRIC_SPHERE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_METRIC_CYLINDER(
     I        bi,bj,k,
     I        uFld, vFld,
     O        uMetricTerms,
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
C  uMetricTerms         :: metric term
      _RL uMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      _RL radius
CEOP

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx
          radius = (yG(i,j,bi,bj)+yG(i,j+1,bi,bj))*0.5 _d 0
C-   to get the same results as before:
c         radius = yC(i,j,bi,bj)
          IF ( radius.GT.0. ) THEN
             uMetricTerms(i,j) = 
     &            uFld(i,j)
     &            *0.25*(vFld(i,j) + vFld(i-1,j) +
     &                   vFld(i,j+1)+vFld(i-1,j+1))/radius
          ELSE
             uMetricTerms(i,j) = 0.
          ENDIF

       ENDDO
      ENDDO

      RETURN
      END
