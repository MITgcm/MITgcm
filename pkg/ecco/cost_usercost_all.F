C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_usercost_all.F,v 1.1 2010/02/16 23:33:37 gforget Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_usercost_all( myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_usercost_all
c     ==================================================================
c
c     o Evaluate user defined penalty terms
c
c     ==================================================================
c     SUBROUTINE cost_usercost_all
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "ecco_cost.h"

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer startrec
      integer endrec

c     == end of interface ==

c--   Evaluate user defined cost function contributions.
c--   [to be added here by user]

      end

