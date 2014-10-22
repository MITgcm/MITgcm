C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_assignperiod.F,v 1.12 2014/10/22 13:28:27 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_assignperiod(
     I                    startofday, startofmonth, startofyear,
     I                    inday,      inmonth,      inyear,
     I                    endofday,   endofmonth,   endofyear,
     O                    startofgen, endofgen,     ingen,
     O                    sum1gen,    genrec,
     I                    myiter, mythid )

c     ==================================================================
c     SUBROUTINE cost_gencost_assignperiod
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

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

      integer myiter, mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer k

c     == end of interface ==

      do k = 1, NGENCOST
       if ( (using_gencost(k)).AND.( (gencost_flag(k).GE.1).OR.
     &         (gencost_avgperiod(k).NE.'     ') ) ) then
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
         else if ( gencost_avgperiod(k) .EQ. 'step' .OR.
     &        gencost_avgperiod(k) .EQ. 'STEP' ) then
c the following is un-tested, but should work
c with the revised logic in cost_averagesgeneric
c (write everytime startofgen AND endofgen) 
            startofgen(k) = .TRUE.
            endofgen(k)   = .TRUE.
            ingen(k)      = .TRUE.
            sum1gen(k)    = 1
            genrec(k)     = 1+ myiter - niter0
         else
            STOP 'gencost_avgperiod wrongly specified'
         end if
       end if
      end do

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
