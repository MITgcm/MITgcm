C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diff_x.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_DIFF_X( 
     I           bi,bj,k,
     I           xA, diffKh,
     I           tracer,
     O           dfx,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfx   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=1-Oly,sNy+Oly
       dfx(1-Olx,j)=0.
       DO i=1-Olx+1,sNx+Olx
        dfx(i,j) = -diffKh*xA(i,j)
     &      *_recip_dxC(i,j,bi,bj)
     &      *(Tracer(i,j)-Tracer(i-1,j))
     &      *CosFacU(j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
