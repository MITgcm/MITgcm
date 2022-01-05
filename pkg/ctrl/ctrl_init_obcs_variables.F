#include "CTRL_OPTIONS.h"

      subroutine ctrl_init_obcs_variables( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_init_obcs_variables
c     ==================================================================
c
c     o Set parts of the vector of control variables and initialize the
c       rest to zero.
c
c     started: heimbach@mit.edu 25-Mar-2002
c
c     ==================================================================
c     SUBROUTINE ctrl_init_obcs_variables
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
c#include "GRID.h"
#include "ctrl.h"
#include "CTRL_OBCS.h"

c     == routine arguments ==
      integer myThid

#ifdef ALLOW_OBCS
c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer iobcs

c     == end of interface ==

#ifdef ALLOW_OBCSN_CONTROL
      do iobcs = 1, nobcs
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
            do k = 1,Nr
              do i = 1-OLx, sNx+OLx 
                xx_obcsn0(i,k,bi,bj,iobcs) = 0. _d 0
                xx_obcsn1(i,k,bi,bj,iobcs) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_OBCSS_CONTROL
      do iobcs = 1, nobcs
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
            do k = 1,Nr
              do i = 1-OLx, sNx+OLx 
                xx_obcss0(i,k,bi,bj,iobcs) = 0. _d 0
                xx_obcss1(i,k,bi,bj,iobcs) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_OBCSW_CONTROL
      do iobcs = 1, nobcs
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
            do k = 1,Nr
              do j = 1-OLy, sNy+OLy 
                xx_obcsw0(j,k,bi,bj,iobcs) = 0. _d 0
                xx_obcsw1(j,k,bi,bj,iobcs) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_OBCSE_CONTROL
      do iobcs = 1, nobcs
        do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
            do k = 1,Nr
              do j = 1-OLy, sNy+OLy 
                xx_obcse0(j,k,bi,bj,iobcs) = 0. _d 0
                xx_obcse1(j,k,bi,bj,iobcs) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo
#endif
#endif /* ALLOW_OBCS */

      return
      end
