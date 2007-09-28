C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_init_varia.F,v 1.3 2007/09/28 00:52:18 heimbach Exp $

#include "AD_CONFIG.h"
#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_init_varia( mythid )

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
# include "SEAICE.h"
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer imin, imax
      integer jmin, jmax
      integer i,j,k

      logical exst

c     == external functions ==

c     == end of interface ==
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-OLy
      jmax = sny+OLy
      imin = 1-OLx
      imax = snx+OLy

c--   Initialize the tiled cost function contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_ice(bi,bj)     = 0. _d 0
          objf_smrarea(bi,bj) = 0. _d 0
          objf_ice_export(bi,bj) = 0. _d 0
c
          num_ice(bi,bj)      = 0. _d 0
          num_smrarea(bi,bj)  = 0. _d 0
        enddo
      enddo

      k = 1
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
#ifdef ALLOW_SEAICE_COST_EXPORT
               uHeffExportCell(i,j,bi,bj) = 0. _d 0
               vHeffExportCell(i,j,bi,bj) = 0. _d 0
#endif
            enddo
          enddo
        enddo
      enddo

      _BARRIER

#endif

      return
      end

