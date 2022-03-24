#include "COST_OPTIONS.h"

      subroutine cost_dependent_init( mythid )

c     ==================================================================
c     SUBROUTINE cost_dependent_init
c     ==================================================================
c
c     o Initialise the variable ad cost function part.
c
c     started: heimbach@mit.edu 17-Jan-2002
c
c     ==================================================================
c     SUBROUTINE cost_dependent_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"
#include "adcost.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

#if ( defined ALLOW_COST_VECTOR || defined  ALLOW_COST_STATE_FINAL )
      integer i
#endif
#ifdef ALLOW_COST_STATE_FINAL
      integer j, k
#endif
      integer bi,bj

c     == end of interface ==

#ifdef ALLOW_OPENAD
      fc%v = 0.0
#else
      fc   = 0. _d 0
#endif

#ifdef ALLOW_AUTODIFF

#ifdef ALLOW_OPENAD
      adfc = 1.0
#else
      if ( myProcId .EQ. 0 ) then
       adfc = 1. _d 0
      endif
#endif

      do bj = myByLo(mythid), myByHi(mythid)
       do bi = myBxLo(mythid), myBxHi(mythid)
#ifdef ALLOW_COST_VECTOR
        do i=1,sNx
         objf_vector(i,bi,bj) = 0. _d 0
         adobjf_vector(i,bi,bj) = 1. _d 0
        end do
#endif
#ifdef ALLOW_COST_STATE_FINAL
         do j=1,sNy
          do i=1,sNx
           do k=1,4*Nr
            objf_state_final(i,j,bi,bj,k) = 0. _d 0
           enddo
           objf_state_final(i,j,bi,bj,4*Nr+1) = 0. _d 0
cph No init. of cost_state_final here,
cph because we need it in ADM*TLM
          end do
         end do
#endif
        enddo
      enddo

#endif /* ALLOW_AUTODIFF */

      return
      end
