#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE INIT_SUMVARS( myThid)
C     *==========================================================*
C     | Initialize all the output variables from the atmos and   |
c     | seaice models that are accumulated and passed to the ocn.|
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j

      DO i=1-OLx,sNx+OLx
        DO j=1-OLy,sNy+OLy

          sum_runoff(i,j)= 0. _d 0
          sum_precip(i,j)= 0. _d 0
          sum_evap(i,j)= 0. _d 0
          sum_qnet(i,j)= 0. _d 0
          sum_fu(i,j)= 0. _d 0
          sum_fv(i,j)= 0. _d 0
          sum_wspeed(i,j)= 0. _d 0
          sum_solarnet(i,j)= 0. _d 0
          sum_slp(i,j)= 0. _d 0
          sum_pCO2(i,j)= 0. _d 0
          sum_prcIce(i,j)= 0. _d 0
          sum_snowPrc(i,j)= 0. _d 0
          sum_evapIce(i,j)= 0. _d 0
          sum_sHeat(i,j)= 0. _d 0
          sum_flxCnB(i,j)= 0. _d 0

        ENDDO
      ENDDO

      RETURN
      END
