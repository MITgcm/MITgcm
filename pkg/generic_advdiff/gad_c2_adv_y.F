C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c2_adv_y.F,v 1.3 2001/09/21 13:11:43 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_C2_ADV_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_C2_ADV_Y( 
     I           bi,bj,k,
     I           vTrans,
     I           tracer,
     O           vT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional flux due to advection of a tracer
C using centered second-order interpolation:
C \begin{equation*}
C F^y_{adv} = V \overline{\theta}^j
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vTrans               :: meridional volume transport
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vT                   :: meridional advective flux
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        vT(i,j) = 
     &   vTrans(i,j)*(Tracer(i,j)+Tracer(i,j-1))*0.5 _d 0
       ENDDO
      ENDDO

      RETURN
      END
