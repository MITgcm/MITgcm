C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diff_r.F,v 1.3 2001/09/21 13:11:43 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DIFF_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIFF_R( 
     I           bi,bj,k,
     I           KappaR,
     I           tracer,
     O           dfr,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated vertical flux due to down-gradient
C diffusion of a tracer:
C \begin{equation*}
C F^r_{diff} = - A^r \kappa_r \frac{1}{\Delta r_c} \delta_k \theta
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
C  KappaR               :: vertical diffusivity
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL KappaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dfr                  :: vertical diffusive flux
      _RL dfr   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  km1                  :: =k+1 for k<Nr, =Nr for k>-Nr
      INTEGER i,j,km1
CEOP

      km1=max(1,k-1)

      IF (k.eq.1 .OR. k.gt.Nr) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         dfr(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         dfr(i,j) =
     &    -_rA(i,j,bi,bj)
     &     *KappaR(i,j,k)*recip_drC(k)
     &     *(Tracer(i,j,km1,bi,bj)-Tracer(i,j,k,bi,bj))*rkFac
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
