#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_V_GRAD_KE(
     I        bi,bj,k,
     I        KE,
     O        dKEdy,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_V_GRAD_KE                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dKEdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          dKEdy(i,j)=-_recip_dyC(i,j,bi,bj)*(KE(i,j)-KE(i,j-1))
     &               *_maskS(i,j,k,bi,bj)*recip_deepFacC(k)
        ENDDO
      ENDDO

      RETURN
      END
