C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/gad_diff_2d_x.F,v 1.6 2012/08/27 19:07:55 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DIFF_2d_X

C     !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIFF_2d_X(
     I   bi,bj,
     I   diffKh,
     I   tracer,
     O   dfx,
     I   myThid )

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

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  diffKh               :: horizontal diffusivity
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj
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

      DO j=1-Oly,sNy+Oly
        dfx(1-Olx,j)=0.
        DO i=1-Olx+1,sNx+Olx
          dfx(i,j) = -diffKh
     &       *_recip_dxC(i,j,bi,bj)
     &       *(Tracer(i,j)-Tracer(i-1,j))
     &       *CosFacU(j,bi,bj)
        ENDDO
      ENDDO

      RETURN
      END

