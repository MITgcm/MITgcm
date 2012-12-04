C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_gad_c2_adv_r.F,v 1.4 2012/12/04 17:26:10 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: CHEAPAML_GAD_C2_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE CHEAPAML_GAD_C2_ADV_R(
     I           bi,bj,
     I           rTrans,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated vertical flux due to advection of a tracer
C using centered second-order interpolation:
C \begin{equation*}
C F^r_{adv} = W \overline{\theta}^k
C \end{equation*}
c modified for use in Cheapaml

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  rTrans               :: vertical volume transport
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                   :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

c      DO j=1-OLy,sNy+OLy
c       DO i=1-OLx,sNx+OLx
       DO j=1,sNy
        DO i=1,sNx
         wT(i,j) = rTrans(i,j)*
     &        (Tracer(i,j)+Tracer(i,j))*0.5 _d 0
        ENDDO
       ENDDO

      RETURN
      END
