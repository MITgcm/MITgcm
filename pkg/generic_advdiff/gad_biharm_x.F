C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_biharm_x.F,v 1.4 2006/12/05 05:26:46 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_BIHARM_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_BIHARM_X(
     I           bi,bj,k,
     I           xA,del2T,diffK4,
     U           dfx,
     I           myThid )

C !DESCRIPTION:
C Calculates the zonal flux due to bi-harmonic diffusion of a tracer.
C Routine takes the laplacian of the tracer as argument and calculates
C the zonal gradient:
C \begin{equation*}
C F^x_{diff} = \kappa_4 \partial_x \nabla^2 \theta
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  xA                   :: area of face at U points
C  del2T                :: Laplacian of tracer
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS xA   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2T(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffK4
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dfx                  :: zonal diffusive flux
      _RL dfx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

C     Difference of zonal fluxes ...
      DO j=1-Oly,sNy+Oly
c      dfx(1-Olx,j) = 0.
       DO i=1-Olx+1,sNx+Olx
c        dfx(i,j) = diffK4*xA(i,j)
         dfx(i,j) = dfx(i,j) + diffK4*xA(i,j)
     &    *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &    *(del2T(i,j)-del2T(i-1,j))
#ifdef COSINEMETH_III
     &    *sqCosFacU(j,bi,bj)
#else
     &    *cosFacU(j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
