C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_del2.F,v 1.5 2006/12/05 22:20:06 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DEL2

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DEL2(
     I           bi,bj,k,
     I           dTdx,dTdy,
     O           del2,
     I           myThid )

C !DESCRIPTION:
C Calculates the horizontal Laplacian of a tracer:
C \begin{equation*}
C \nabla^2 \theta = \partial_{xx} \theta + \partial_{yy} \theta
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  dTx                  :: zonal tracer gradient
C  dTy                  :: meridional tracer gradient
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL dTdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  del2                 :: Laplacian of tracer
      _RL del2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

C--   Initialise to zero.
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        del2(i,j)= 0. _d 0
       ENDDO
      ENDDO

C     Difference of zonal fluxes & meridional fluxes
C      and divide by grid-cell volume
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        del2(i,j)=recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &           *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &           *( ( dTdx(i+1,j)-dTdx(i,j) )
     &             +( dTdy(i,j+1)-dTdy(i,j) )
     &            )
       ENDDO
      ENDDO

      RETURN
      END
