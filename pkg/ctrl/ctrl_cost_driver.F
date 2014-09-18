C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_cost_driver.F,v 1.2 2014/09/18 18:07:39 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

      subroutine ctrl_cost_driver( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_cost_driver
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE ctrl_cost_driver
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "CTRL_GENARR.h"
#endif

c     == routine arguments ==

      integer myThid

c     == local variables ==

#ifdef ALLOW_CTRL

#ifdef ALLOW_GENTIM2D_CONTROL
      integer startrec
      integer endrec
      integer iarr
#endif

c     == end of interface ==

c--   Evaluate the individual cost function contributions.

#ifdef ALLOW_GENTIM2D_CONTROL
      do iarr = 1, maxCtrlTim2D

      if (xx_gentim2d_weight(iarr).NE.' ') then
      startrec = ncvarrecstart(300+iarr)
      endrec   = ncvarrecsend(300+iarr)
      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_gentim2d_file(1),xx_gentim2d_dummy(1),
     &     xx_gentim2d_period(1),
     &     0. _d 0, wgentim2d(1-Olx,1-Oly,1,1,iarr),
     &     num_gentim2d(1,1,iarr), num_gentim2dm(1,1,iarr),
     &     objf_gentim2d(1,1,iarr), objf_gentim2dm(1,1,iarr), 
     &     objf_gentim2dsmoo(1,1,iarr), 0. _d 0, 0. _d 0,
     &     maskC, myThid )
      endif

      enddo
#endif

#endif


      return 
      end
