C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_genctrl.F,v 1.1 2013/02/05 21:48:07 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_genctrl( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_genctrl
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE cost_genctrl
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "CTRL_GENARR.h"
#endif
#include "ecco_cost.h"

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer startrec
      integer endrec
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      integer iarr
#endif

c     == end of interface ==

c--   Evaluate the individual cost function contributions.

#ifdef ALLOW_CTRL
#ifdef ALLOW_GENTIM2D_CONTROL
      do iarr = 1, maxCtrlTim2D

      startrec = ncvarrecstart(300+iarr)
      endrec   = ncvarrecsend(300+iarr)
      call cost_forcing_gen (
     &     myiter, mytime, startrec, endrec,
     &     xx_gentim2d_file(1),xx_gentim2d_dummy(1),
     &     xx_gentim2d_period(1),
     &     0. _d 0, wgentim2d(1-Olx,1-Oly,1,1,iarr),
     &     num_gentim2d(1,1,iarr), num_gentim2dm(1,1,iarr),
     &     objf_gentim2d(1,1,iarr), objf_gentim2dm(1,1,iarr), 
     &     objf_gentim2dsmoo(1,1,iarr), 0. _d 0, 0. _d 0,
     &     maskC, mythid )

      enddo
#endif
#endif

      end
