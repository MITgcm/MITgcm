#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_init_varia( myThid )

c     ==================================================================
c     SUBROUTINE seaice_cost_init_varia
c     ==================================================================
c
c     o Initialise the variable cost function part.
c     added sea-ice term: menemenlis@jpl.nasa.gov 26-Feb-2003
c
c     ==================================================================
c     SUBROUTINE seaice_cost_init_varia
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#ifdef ALLOW_COST
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==
      integer myThid

#if (defined ALLOW_COST && defined ALLOW_COST_ICE)

c     == local variables ==
      integer bi,bj
#ifdef ALLOW_SEAICE_COST_EXPORT
      integer i,j
#endif

c     == end of interface ==

c--   Initialize the tiled cost function contributions.
      do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
          objf_ice(bi,bj)        = 0. _d 0
          objf_ice_export(bi,bj) = 0. _d 0
c
          num_ice(bi,bj)         = 0. _d 0

#ifdef ALLOW_SEAICE_COST_EXPORT
          do j=1-OLy,sNy+OLy
            do i=1-OLx,sNx+OLx
               uHeffExportCell(i,j,bi,bj) = 0. _d 0
               vHeffExportCell(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
#endif
        enddo
      enddo

      _BARRIER

#endif /* ALLOW_COST */

      return
      end
