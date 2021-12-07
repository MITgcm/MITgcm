#include "THSICE_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      SUBROUTINE THSICE_COST_DRIVER( myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE thsice_cost_driver
c     ==================================================================
c
c     o Evaluate cost function contributions of
c       thsice observations
c
c     ==================================================================
c     SUBROUTINE thsice_cost_driver
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
c#include "GRID.h"

c#ifdef ALLOW_COST
c# include "CTRL_SIZE.h"
c# include "ctrl.h"
c# include "ctrl_dummy.h"
c#endif

c     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

c     == end of interface ==

#ifdef ALLOW_THSICE_COST_TEST
       CALL THSICE_COST_TEST( myTime, myIter, myThid )
#endif

      RETURN
      END
