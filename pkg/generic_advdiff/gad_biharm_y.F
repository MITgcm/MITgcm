C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_biharm_y.F,v 1.4 2006/12/05 05:26:46 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_BIHARM_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_BIHARM_Y(
     I           bi,bj,k,
     I           yA,del2T,diffK4,
     O           dfy,
     I           myThid )

C !DESCRIPTION:
C Calculates the meridional flux due to bi-harmonic diffusion of a tracer.
C Routine takes the laplacian of the tracer as argument and calculates
C the meridional gradient:
C \begin{equation*}
C F^y_{diff} = \kappa_4 \partial_y \nabla^2 \theta
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  yA                   :: area of face at V points
C  del2T                :: Laplacian of tracer
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS yA   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2T(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffK4
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dfy                  :: meridional diffusive flux
      _RL dfy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

C     Difference of meridional fluxes ...
c     DO i=1-Olx,sNx+Olx
c      dfy(i,1-Oly) = 0.
c     ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
c        dfy(i,j) = diffK4*yA(i,j)
         dfy(i,j) = dfy(i,j) + diffK4*yA(i,j)
     &    *_recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &    *(del2T(i,j)-del2T(i,j-1))
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &    *sqCosFacV(j,bi,bj)
#else
     &    *cosFacV(j,bi,bj)
#endif
#endif
       ENDDO
      ENDDO

      RETURN
      END
