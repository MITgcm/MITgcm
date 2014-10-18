C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_usercost_all.F,v 1.4 2014/10/18 18:15:45 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


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

#ifdef ECCO_CTRL_DEPRECATED
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ctrl.h"
#include "ecco_cost.h"
#endif

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

#ifdef ECCO_CTRL_DEPRECATED
c     == local variables ==

      integer startrec
      integer endrec

c     == end of interface ==

c--   Evaluate user defined cost function contributions.
c--   [to be added here by user]
#endif

      end

