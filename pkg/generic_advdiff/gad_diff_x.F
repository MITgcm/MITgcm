#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DIFF_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIFF_X(
     I           bi,bj,k,
     I           xA, diffKh,
     I           tracer,
     O           dfx,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated zonal flux due to down-gradient diffusion
C of a tracer:
C \begin{equation*}
C F^x_{diff} = - A^x \kappa_h \frac{1}{\Delta x_c} \delta_i \theta
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
# include "EEPARAMS.h"
# include "PARAMS.h"
# include "DYNVARS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  xA                   :: area of face at U points
C  diffKh               :: horizontal diffusivity
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dfx                  :: zonal diffusive flux
      _RL dfx   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
      IF ( smag3D_diffCoeff.GT.zeroRL ) THEN
        DO j=1-OLy,sNy+OLy
         dfx(1-OLx,j) = 0.
         DO i=1-OLx+1,sNx+OLx
          dfx(i,j) = -( diffKh
     &                + halfRL*( smag3D_diffK(i-1,j,k,bi,bj)
     &                         + smag3D_diffK( i, j,k,bi,bj) )
     &                )*xA(i,j)
     &             *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &             *( tracer(i,j) - tracer(i-1,j) )
     &             *cosFacU(j,bi,bj)
         ENDDO
        ENDDO
      ELSE
#endif /* ALLOW_SMAG_3D_DIFFUSIVITY */
        DO j=1-OLy,sNy+OLy
         dfx(1-OLx,j) = 0.
         DO i=1-OLx+1,sNx+OLx
          dfx(i,j) = -diffKh*xA(i,j)
     &             *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &             *( tracer(i,j) - tracer(i-1,j) )
     &             *cosFacU(j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
      ENDIF
#endif /* ALLOW_SMAG_3D_DIFFUSIVITY */

      RETURN
      END
