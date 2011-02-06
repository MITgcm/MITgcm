C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_gad_c2_adv_r.F,v 1.1 2011/02/06 00:18:05 jmc Exp $
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
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                   :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP


       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) = rTrans(i,j)*
     &        (Tracer(i,j,bi,bj)+Tracer(i,j,bi,bj))*0.5 _d 0
        ENDDO
       ENDDO

      RETURN
      END
