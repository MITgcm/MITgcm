C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_del2.F,v 1.2 2001/09/20 20:07:06 adcroft Exp $
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

C     Difference of zonal fluxes ...
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        del2(i,j)=dTdx(i+1,j)-dTdx(i,j)
       ENDDO
       del2(sNx+Olx,j)=0.
      ENDDO

C     ... add difference of meridional fluxes and divide by volume
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        del2(i,j)=recip_rA(i,j,bi,bj)
     &           *recip_drF(k)
     &           *_recip_hFacC(i,j,k,bi,bj)
     &           *(
     &              del2(i,j)
     &            +( dTdy(i,j+1)-dTdy(i,j) )
     &            )
       ENDDO
      ENDDO

      RETURN
      END
