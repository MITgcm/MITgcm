C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_dependent_init.F,v 1.1 2002/01/17 17:01:47 heimbach Exp $

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

      fc   = 0.0

#if ( defined (ALLOW_ADJOINT_RUN) || \
      defined (ALLOW_GRADIENT_CHECK) || \
      defined (ALLOW_ECCO_OPTIMIZATION) )

      adfc = 1.0
      do bj = jtlo,jthi
        do bi = itlo,ithi
#ifdef ALLOW_COST_VECTOR
          do i=1,sNx
            objf_vector(i,bi,bj) = 0. _d 0
            adobjf_vector(i,bi,bj) = 1. _d 0
          end do
#endif
        enddo
      enddo

#endif

      return
      end

