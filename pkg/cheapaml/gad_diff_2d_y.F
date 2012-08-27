C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/gad_diff_2d_y.F,v 1.7 2012/08/27 19:07:55 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DIFF_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIFF_2d_Y(
     I           bi,bj,
     I           diffKh,
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
#include "EEPARAMS.h"

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
C  dfx                  :: meridional diffusive flux
      _RL dfy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j,jG
CEOP

      DO i=1-Olx,sNx+Olx
        dfy(i,1-Oly)=0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx,sNx+Olx
          dfy(i,j) = -diffKh
     $       *_recip_dyC(i,j,bi,bj)
     $       *(Tracer(i,j)-Tracer(i,j-1))
#ifdef ISOTROPIC_COS_SCALING
     $       *CosFacV(j,bi,bj)
#endif
          jG = myYGlobalLo-1+(bj-1)*sNy+j
          if(jG.eq.ny.or.jG.eq.1)dfy(i,j)=0.d0
        ENDDO
      ENDDO

      RETURN
      END

