C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.6 2003/10/02 21:34:45 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_final( mythid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "cost.h"
#include "ctrl.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer i,j,k
      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

#if (defined (ALLOW_COST_VECTOR))

      CALL COST_VECTOR (myThid)

#elif (defined (ALLOW_COST_STATE_FINAL))

      CALL COST_STATE_FINAL (myThid)

#else /* ALLOW_COST_VECTOR undef */

#ifdef ALLOW_COST_TEST
      CALL COST_TEST (myThid)
#endif

#ifdef ALLOW_COST_ATLANTIC_HEAT
      CALL COST_ATLANTIC_HEAT (myThid)
#endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          print*,' --> objf_test(bi,bj)   =',objf_test(bi,bj)
          print*,' --> objf_tracer(bi,bj) =',objf_tracer(bi,bj)
          print*,' --> objf_atl(bi,bj) =',objf_atl(bi,bj)

          fc = fc
     &            + mult_test   * objf_test(bi,bj)
     &            + mult_tracer * objf_tracer(bi,bj)
     &            + mult_atl    * objf_atl(bi,bj)
        enddo
      enddo

      print*,' local fc = ', fc

c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

#endif /* ALLOW_COST_VECTOR */

      print*,' global fc = ', fc

c--   set averaging freq. to zero to avoid re-write of
c--   averaged fields in reverse checkpointing loops
      taveFreq = 0.

#endif /* ALLOW_COST */

      return
      end

