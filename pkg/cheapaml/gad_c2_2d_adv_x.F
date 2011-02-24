C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/gad_c2_2d_adv_x.F,v 1.3 2011/02/24 16:11:41 wienders Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_C2_2d_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_C2_2d_ADV_X( 
     I   bi,bj,
     I   uVel,
     I   tracer,
     O   uT,
     I   myThid )
      
C !DESCRIPTION:
C Calculates the area integrated zonal flux due to advection of a tracer using
C centered second-order interpolation:
C \begin{equation*}
C F^x_{adv} = U \overline{\theta}^i
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  uvel               :: zonal velocity
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj
      _RL uvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT                   :: zonal advective flux
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO j=1-Oly,sNy+Oly
        uT(1-Olx,j)=0.
        DO i=1-Olx+1,sNx+Olx
          uT(i,j) = 
     &       uVel(i,j,bi,bj)*(Tracer(i,j)+Tracer(i-1,j))*0.5 _d 0
        ENDDO
      ENDDO
      
      RETURN
      END
