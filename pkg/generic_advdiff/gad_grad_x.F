C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_grad_x.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_GRAD_X( 
     I           bi,bj,k,
     I           xA,
     I           tracer,
     O           dTdx,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTdx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

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
