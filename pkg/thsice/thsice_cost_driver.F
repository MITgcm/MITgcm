C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_cost_driver.F,v 1.2 2013/04/13 23:14:43 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      subroutine thsice_cost_driver( mytime, myiter, mythid )

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

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_COST
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == end of interface ==

#ifdef ALLOW_THSICE_COST_TEST
       CALL THSICE_COST_TEST( mytime, myiter, mythid )
#endif

      end
