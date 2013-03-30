C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_assignperiod.F,v 1.10 2013/03/30 11:57:47 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_assignperiod(
     I                    startofday, startofmonth, startofyear,
     I                    inday,      inmonth,      inyear,
     I                    endofday,   endofmonth,   endofyear,
     O                    startofgen, endofgen,     ingen,
     O                    sum1gen,    genrec,
     I                    mythid )

c     ==================================================================
c     SUBROUTINE cost_gencost_assignperiod
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "optim.h"
#include "ecco_cost.h"

c     == routine arguments ==

      logical startofday
      logical startofmonth
      logical startofyear
      logical inday
      logical inmonth
      logical inyear
      logical endofday
      logical endofmonth
      logical endofyear
      logical startofgen(NGENCOST)
      logical endofgen(NGENCOST)
      logical ingen(NGENCOST)
      integer sum1gen(NGENCOST)
      integer genrec(NGENCOST)

      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer k

c     == end of interface ==

      do k = 1, NGENCOST
       if ( (using_gencost(k)).AND.( gencost_flag(k).GE.1 ) ) then
         if ( gencost_avgperiod(k) .EQ. 'day' .OR.
     &        gencost_avgperiod(k) .EQ. 'DAY' ) then
            startofgen(k) = startofday
            endofgen(k)   = endofday
            ingen(k)      = inday
            sum1gen(k)    = sum1day
            genrec(k)     = dayrec
         else if ( gencost_avgperiod(k) .EQ. 'month' .OR.
     &        gencost_avgperiod(k) .EQ. 'MONTH' ) then
            startofgen(k) = startofmonth
            endofgen(k)   = endofmonth
            ingen(k)      = inmonth
            sum1gen(k)    = sum1mon
            genrec(k)     = monrec
         else if ( gencost_avgperiod(k) .EQ. 'year' .OR.
     &        gencost_avgperiod(k) .EQ. 'YEAR' ) then
            startofgen(k) = startofyear
            endofgen(k)   = endofyear
            ingen(k)      = inyear
            sum1gen(k)    = sum1year
            genrec(k)     = yearrec
         else
            STOP 'gencost_avgperiod wrongly specified'
         end if
       end if
      end do

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
