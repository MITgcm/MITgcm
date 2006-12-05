C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diff_y.F,v 1.4 2006/12/05 05:26:46 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DIFF_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIFF_Y(
     I           bi,bj,k,
     I           yA, diffKh,
     I           tracer,
     O           dfy,
     I           myThid )

C !DESCRIPTION:
C Calculates the area integrated meridional flux due to down-gradient
C diffusion of a tracer:
C \begin{equation*}
C F^y_{diff} = - A^y \kappa_h \frac{1}{\Delta y_c} \delta_j \theta
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  yA                   :: area of face at V points
C  diffKh               :: horizontal diffusivity
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dfx                  :: meridional diffusive flux
      _RL dfy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO i=1-Olx,sNx+Olx
       dfy(i,1-Oly)=0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        dfy(i,j) = -diffKh*yA(i,j)
     &           *_recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &           *(tracer(i,j)-tracer(i,j-1))
#ifdef ISOTROPIC_COS_SCALING
     &           *cosFacV(j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
