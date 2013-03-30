C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_cost_init_varia.F,v 1.1 2013/03/30 01:23:58 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      subroutine thsice_cost_init_varia( mythid )

c     ==================================================================
c     SUBROUTINE thsice_cost_init_varia
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE thsice_cost_init_varia
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#ifdef ALLOW_COST
# include "THSICE_COST.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

      logical exst

c     == external functions ==

c     == end of interface ==
      jtlo = myByLo(mythid)
      jthi = myByHi(mythid)
      itlo = myBxLo(mythid)
      ithi = myBxHi(mythid)

c--   Initialize the tiled cost function contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
# ifdef ALLOW_COST
          objf_thsice(bi,bj)     = 0. _d 0
          num_thsice(bi,bj)      = 0. _d 0
# endif
        enddo
      enddo

#endif

      return
      end
