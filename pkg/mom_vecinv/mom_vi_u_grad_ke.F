#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_U_GRAD_KE(
     I        bi,bj,k,
     I        KE,
     O        dKEdx,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_U_GRAD_KE                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dKEdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
          dKEdx(i,j)=-_recip_dxC(i,j,bi,bj)*(KE(i,j)-KE(i-1,j))
     &               *_maskW(i,j,k,bi,bj)*recip_deepFacC(k)
        ENDDO
      ENDDO

      RETURN
      END
