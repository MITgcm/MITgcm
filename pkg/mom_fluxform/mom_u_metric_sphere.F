C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_metric_sphere.F,v 1.5 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_METRIC_SPHERE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_METRIC_SPHERE(
     I        bi,bj,k,
     I        uFld, vFld,
     O        uMetricTerms,
     I        myThid )

C !DESCRIPTION:
C Calculates the zonal metric term due to sphericity:
C \begin{equation}
C \frac{u}{a} \overline{v}^{ij} \tan{\phi}
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
CEOP

      DO j=1-Olx,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx
         uMetricTerms(i,j) =
     &    uFld(i,j)*recip_rSphere*recip_deepFacC(k)
     &    *0.25*( vFld(i,j  )+vFld(i-1,j  )
     &           +vFld(i,j+1)+vFld(i-1,j+1)
     &          )*_tanPhiAtU(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
