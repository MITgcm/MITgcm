C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_biharm_x.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_BIHARM_X( 
     I           bi,bj,k,
     I           xA,del2T,diffK4,
     O           dfx,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS xA   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2T(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffK4
      _RL dfx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

C     Difference of zonal fluxes ...
      DO j=1-Oly,sNy+Oly
       dfx(1-Olx,j) = 0.
       DO i=1-Olx+1,sNx+Olx
         dfx(i,j) = diffK4
     &    *xA(i,j)
     &    *(del2T(i,j)-del2T(i-1,j))*_recip_dxC(i,j,bi,bj)
#ifdef COSINEMETH_III
     &    *sqCosFacU(j,bi,bj)
#else
     &    *CosFacU(j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
