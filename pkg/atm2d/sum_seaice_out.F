#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE SUM_SEAICE_OUT( myThid )
C     *==========================================================*
C     | Sum output from temp seaice step for passing to ocean.   |
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"

C     === Global SeaIce Variables ===
#include "THSICE_VARS.h"

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j

      DO j=1, sNy
        DO i=1,sNx
        
          IF (iceMask(i,j,1,1) .NE. 0. _d 0) THEN
            sum_sHeat(i,j) = sum_sHeat(i,j) + sHeating(i,j,1,1)
            sum_flxCnB(i,j)= sum_flxCnB(i,j) + flxCndBt(i,j,1,1)

C Add contribution of SW passing through ice
C note sign convention of sum_solarnet is +=up, opposite of ice model
            sum_solarnet(i,j)= sum_solarnet(i,j)
     &                  - icFlxSW(i,j,1,1)*iceMask(i,j,1,1)
C zero out icFlxSw or it will be double counted in thsice_step_fwd
            icFlxSW(i,j,1,1)= 0. _d 0

          ENDIF
          
        ENDDO
      ENDDO  


      RETURN
      END
