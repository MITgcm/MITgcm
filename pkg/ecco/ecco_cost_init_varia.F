#include "ECCO_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_cost_init_varia( myThid )

c     ==================================================================
c     SUBROUTINE ecco_cost_init_varia
c     ==================================================================
c
c     o Initialise the variable cost function part.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     changed: Christian Eckert eckert@mit.edu 18-Apr-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c     heimbach@mit.edu 05-Nov-2003 Now ecco part of cost
c
c     ==================================================================
c     SUBROUTINE ecco_cost_init_varia
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_COST
# include "cost.h"
#endif

c     == routine arguments ==
      integer myThid

c     == local variables ==
      integer bi,bj
      integer imin, imax
      integer jmin, jmax
      integer i,j,k
c     logical exst

c     == end of interface ==

      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLy

c--   Initialise adjoint of monthly mean files calculated
c--   in cost_averagesfields (and their ad...).
      call cost_averagesinit( myThid )
      _BARRIER

#ifndef ALLOW_TANGENTLINEAR_RUN
cph(
cph   The following init. shoud not be applied if in the middle
cph   of a divided adjoint run
cph)
c      inquire( file='costfinal', exist=exst )
c      if ( .NOT. exst) then
c         call ecco_cost_init_barfiles( myThid )
c      endif
#endif

c--   Initialize the tiled cost function contributions.
      do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_GENCOST_CONTRIBUTION
          do k=1,NGENCOST
            objf_gencost(bi,bj,k)    = 0. _d 0
            num_gencost(bi,bj,k)    = 0. _d 0
          enddo
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
          objf_obcsn(bi,bj)    = 0. _d 0
          objf_obcss(bi,bj)    = 0. _d 0
          objf_obcsw(bi,bj)    = 0. _d 0
          objf_obcse(bi,bj)    = 0. _d 0
          objf_ageos(bi,bj)    = 0. _d 0
          num_obcsn(bi,bj)    = 0. _d 0
          num_obcss(bi,bj)    = 0. _d 0
          num_obcsw(bi,bj)    = 0. _d 0
          num_obcse(bi,bj)    = 0. _d 0
          num_ageos(bi,bj)    = 0. _d 0
#endif

          do j = jmin,jmax
            do i = imin,imax
              frame(i,j)        = 1. _d 0
              cosphi(i,j,bi,bj) = 1. _d 0
            enddo
          enddo

        enddo
      enddo

c--   Initialise the "global" parts of the cost function.
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
      _BEGIN_MASTER( myThid )
        objf_obcsvol = 0. _d 0
        num_obcsvol = 0. _d 0
      _END_MASTER( myThid )
#endif

      _BARRIER

      RETURN
      END
