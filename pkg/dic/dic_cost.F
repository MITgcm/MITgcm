#include "DIC_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: DIC_COST
C     !INTERFACE:
      SUBROUTINE DIC_COST (
     I                      myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE DIC_COST
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#ifdef DIC_BIOTIC
#include "DIC_COST.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC_COST
C     !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      _RL loc_cost(nSx,nSy), glb_cost
CEOP

C--   Calculate costfunction
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         loc_cost(bi,bj) = 0. _d 0
         DO i=1,sNx
          DO j=1,sNy
            loc_cost(bi,bj) = loc_cost(bi,bj)
     &                      + FluxCO2(i,j,bi,bj)*rA(i,j,bi,bj)
     &                       *maskC(i,j,1,bi,bj)*dTtracerLev(1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL( loc_cost, glb_cost, myThid )

      _BEGIN_MASTER( myThid )
       totcost = totcost + glb_cost

       WRITE(standardMessageUnit,'(A,I9,1PE23.15)')
     &  ' DIC_COST: iter,totcost =', myIter, totcost
      _END_MASTER( myThid )

#endif /* ALLOW_DIC_COST */

      RETURN
      END
