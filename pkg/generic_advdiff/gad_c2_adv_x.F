C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c2_adv_x.F,v 1.1 2001/05/30 19:34:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_C2_ADV_X( 
     I           bi,bj,k,
     I           uTrans,
     I           tracer,
     O           uT,
     I           myThid )
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       DO i=1-Olx+1,sNx+Olx
        uT(i,j) = 
     &   uTrans(i,j)*(Tracer(i,j)+Tracer(i-1,j))*0.5 _d 0
       ENDDO
      ENDDO

      RETURN
      END
