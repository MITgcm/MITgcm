C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_oad_all/Attic/cost_dependent_init.F,v 1.2 2012/07/05 22:50:54 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

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

      integer i,j,k
      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

c     == external functions ==

c     == end of interface ==
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      fc%v   = 0.0

#ifdef ALLOW_AUTODIFF

      adfc = 1.0
      do bj = jtlo,jthi
       do bi = itlo,ithi
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
