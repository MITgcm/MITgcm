C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_grad_x.F,v 1.3 2001/09/21 13:11:43 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_GRAD_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_GRAD_X( 
     I           bi,bj,k,
     I           xA,
     I           tracer,
     O           dTdx,
     I           myThid )

C !DESCRIPTION:
C Calculates the zonal gradient of a tracer: $\partial_x \theta
C = \frac{1}{\Delta x_c} \delta_i \theta$
C and multiplies by the area.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  tracer               :: tracer field
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dTdx                 :: zonal gradient
      _RL dTdx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

C     o Zonal tracer gradient
      DO j=1-Oly,sNy+Oly
       dTdx(1-Olx,j)=0.
       DO i=1-Olx+1,sNx+Olx
        dTdx(i,j) = _recip_dxC(i,j,bi,bj)*xA(i,j)
     &               *(tracer(i,j)-tracer(i-1,j))
#ifdef COSINEMETH_III
     &               *sqCosFacU(j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
