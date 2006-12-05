C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_metric_sphere.F,v 1.5 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_METRIC_SPHERE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_METRIC_SPHERE(
     I        bi,bj,k,
     I        uFld,
     O        vMetricTerms,
     I        myThid )

C !DESCRIPTION:
C Calculates the meridional metric term due to sphericity:
C \begin{equation}
C \frac{1}{a} \overline{u}^{ij} \overline{u}^{ij} \tan{\phi}
C \end{equation}

C !USES: ===============================================================`
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vMetricTerms         :: metric term
      _RL vMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO j=1-Olx+1,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
         vMetricTerms(i,j) = -recip_rSphere*recip_deepFacC(k)
     &    *0.25*( uFld(i,j  )+uFld(i+1,j  )
     &           +uFld(i,j-1)+uFld(i+1,j-1)
     &          )
     &    *0.25*( uFld(i,j  )+uFld(i+1,j  )
     &           +uFld(i,j-1)+uFld(i+1,j-1)
     &          )
     &    *_tanPhiAtV(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
