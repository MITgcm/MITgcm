C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.2 2001/07/13 13:37:45 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_Final(
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_Final
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
c     SUBROUTINE cost_Final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"
#include "ctrl.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_COST
c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

#ifdef ALLOW_COST_TEST
      CALL COST_TEST (myThid)
#endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          print*,' --> objf_test(bi,bj)   =',objf_test(bi,bj)
          print*,' --> objf_tracer(bi,bj) =',objf_tracer(bi,bj)

          fc = fc
     &            + mult_test   * objf_test(bi,bj)
     &            + mult_tracer * objf_tracer(bi,bj)
        enddo
      enddo

      print*,' fc = ', fc

c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

#endif /* ALLOW_COST */

      return
      end

