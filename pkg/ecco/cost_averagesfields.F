#include "ECCO_OPTIONS.h"

      subroutine cost_averagesfields( mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_averagesfields
c     ==================================================================
c
c     o Compute time averages of etaN, theta, and salt. The counters
c       are explicitly calculated instead of being incremented. This
c       reduces dependencies. The latter is useful for the adjoint code
c       generation.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 24-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_averagesfields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "ECCO_SIZE.h"
#include "ECCO.h"

c     == routine arguments ==

      _RL     mytime
      integer mythid

c     == local variables ==

      integer myiter
      integer k
      logical first
      logical startofday
      logical startofmonth
      logical startofyear
      logical inday
      logical inmonth
      logical inyear
      logical last
      logical endofday
      logical endofmonth
      logical endofyear
#ifdef ALLOW_GENCOST_CONTRIBUTION
      logical startofgen(NGENCOST)
      logical endofgen(NGENCOST)
      logical ingen(NGENCOST)
      integer sum1gen(NGENCOST)
      integer genrec(NGENCOST)
      integer kk
#endif

c     == external functions ==

c     == end of interface ==

      myiter = niter0 + INT((mytime-starttime)/deltaTClock+0.5)

c--   Get the time flags and record numbers for the time averaging.

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_CALL('cost_averagesflags',myThid)
#endif
      call cost_averagesflags(
     I                    myiter,     mytime,       mythid,
     O                    first,      last,
     O                    startofday, startofmonth, startofyear,
     O                    inday,      inmonth,      inyear,
     O                    endofday,   endofmonth,   endofyear,
     O                    sum1day,    dayrec,
     O                    sum1mon,    monrec,
     O                    sum1year,   yearrec
     &                  )

#ifdef ALLOW_GENCOST_CONTRIBUTION
      call cost_gencost_assignperiod(
     I                    startofday, startofmonth, startofyear,
     I                    inday,      inmonth,      inyear,
     I                    endofday,   endofmonth,   endofyear,
     O                    startofgen, endofgen,     ingen,
     O                    sum1gen,    genrec,
     I                    myiter, mythid )
      call cost_gencost_customize( mythid )

      do k = 1, NGENCOST
      if ( (using_gencost(k)).AND.(.NOT.gencost_barskip(k)) ) then
      if ( .NOT.gencost_is3d(k) ) then
        call cost_averagesgeneric(
     &       gencost_barfile(k),
     &       gencost_barfld(1-Olx,1-Oly,1,1,k),
     &       gencost_modfld(1-Olx,1-Oly,1,1,k),
     &       gencost_dummy(k),
     &       first, last,
     &       startofgen(k), endofgen(k), ingen(k),
     &       sum1gen(k), genrec(k), 1, mythid )
#ifdef ALLOW_GENCOST3D
      else
        kk=gencost_pointer3d(k)
        call cost_averagesgeneric(
     &     gencost_barfile(k),
     &     gencost_bar3d(1-Olx,1-Oly,1,1,1,kk),
     &     gencost_mod3d(1-Olx,1-Oly,1,1,1,kk),
     &     gencost_dummy(k),
     &     first, last,
     &     startofgen(k), endofgen(k), ingen(k),
     &     sum1gen(k), genrec(k), nr, mythid )
#endif
      endif

      endif
      end do
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('cost_averagesfields',myThid)
#endif

      return
      end
