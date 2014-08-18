C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c2_adv_r.F,v 1.5 2014/08/18 12:22:46 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_C2_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_C2_ADV_R(
     I           bi, bj, k,
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

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  rTrans               :: vertical volume transport
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                   :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  km1                  :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER i,j,km1
CEOP

      km1=max(1,k-1)

      IF ( k.EQ.1 .OR. k.GT.Nr) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         wT(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         wT(i,j) = maskC(i,j,km1,bi,bj)*
     &     rTrans(i,j)*
     &        (tracer(i,j,k)+tracer(i,j,km1))*0.5 _d 0
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
