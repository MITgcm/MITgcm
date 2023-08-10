#include "ECCO_OPTIONS.h"

      subroutine cost_averagesinit( mythid )

c     ==================================================================
c     SUBROUTINE cost_averagesinit
c     ==================================================================
c
c     o Set average fields for temperature, salinity, surface pressure,
c       and averaging counters to zero. The average fields are declared
c       in the header file ecco_cost.h.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_averagesinit
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer jmin,jmax
      integer imin,imax
#ifdef ALLOW_GENCOST_CONTRIBUTION
      integer i,j,k,kk
#endif

c     == end of interface ==

c--   Set the loop ranges.
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)

#ifdef ALLOW_GENCOST_CONTRIBUTION
          do  k=1,NGENCOST
            do j = jmin,jmax
              do i = imin,imax
                gencost_barfld(i,j,bi,bj,k) = 0. _d 0
                gencost_modfld(i,j,bi,bj,k) = 0. _d 0
              enddo
            enddo
            gencost_dummy(k) = 0. _d 0
          enddo
#ifdef ALLOW_GENCOST3D
          do  kk=1,NGENCOST3D
            do j = jmin,jmax
              do i = imin,imax
                do k = 1,nr
                gencost_bar3d(i,j,k,bi,bj,kk) = 0. _d 0
                gencost_mod3d(i,j,k,bi,bj,kk) = 0. _d 0
                enddo
              enddo
            enddo
          enddo
#endif /* ALLOW_GENCOST3D */
#endif /* ALLOW_GENCOST_CONTRIBUTION */

        enddo
      enddo

      return
      end
