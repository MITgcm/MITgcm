C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_cost_driver.F,v 1.1 2013/03/30 01:23:58 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      subroutine thsice_cost_driver( myiter, mytime, mythid )

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
       CALL THSICE_COST_TEST( myiter, mytime, mythid )
#endif

      end
