C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.7 2003/11/06 22:03:49 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "COST_CPPOPTIONS.h"

      subroutine cost_final( mythid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c     heimbach@mit.edu 05-Nov-2003 Modularize cost package
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

#if (defined (ALLOW_ECCO))
      CALL ECCO_COST_FINAL (myThid)

#elif (defined (ALLOW_COST_VECTOR))
      CALL COST_VECTOR (myThid)

#elif (defined (ALLOW_COST_STATE_FINAL))
      CALL COST_STATE_FINAL (myThid)

#else /* above stuff undef */

# ifdef ALLOW_COST_TEST
      CALL COST_TEST (myThid)
# endif
# ifdef ALLOW_COST_ATLANTIC_HEAT
      CALL COST_ATLANTIC_HEAT (myThid)
# endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          print*,' --> objf_test(bi,bj)   =',objf_test(bi,bj)
          print*,' --> objf_tracer(bi,bj) =',objf_tracer(bi,bj)
          print*,' --> objf_atl(bi,bj)    =',objf_atl(bi,bj)

          fc = fc
     &            + mult_test   * objf_test(bi,bj)
     &            + mult_tracer * objf_tracer(bi,bj)
     &            + mult_atl    * objf_atl(bi,bj)
        enddo
      enddo

      print*,' local fc = ', fc

c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

      print*,' global fc = ', fc

#endif /* ALLOW_ECCO */

c--   set averaging freq. to zero to avoid re-write of
c--   averaged fields in reverse checkpointing loops
      taveFreq = 0.

#endif /* ALLOW_COST */

      return
      end

