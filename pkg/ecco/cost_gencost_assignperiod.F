C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_assignperiod.F,v 1.3 2010/08/29 19:28:50 gforget Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

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
#include "ctrl_dummy.h"

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

      integer num_var

c     == end of interface ==

      do num_var = 1, NGENCOST
         if ( gencost_avgperiod(num_var) .EQ. 'day' .OR.
     &        gencost_avgperiod(num_var) .EQ. 'DAY' ) then
            startofgen(num_var) = startofday
            endofgen(num_var)   = endofday
            ingen(num_var)      = inday
            sum1gen(num_var)    = sum1day
            genrec(num_var)     = dayrec
         else if ( gencost_avgperiod(num_var) .EQ. 'month' .OR.
     &        gencost_avgperiod(num_var) .EQ. 'MONTH' ) then
            startofgen(num_var) = startofmonth
            endofgen(num_var)   = endofmonth
            ingen(num_var)      = inmonth
            sum1gen(num_var)    = sum1mon
            genrec(num_var)     = monrec
         else if ( gencost_avgperiod(num_var) .EQ. 'year' .OR.
     &        gencost_avgperiod(num_var) .EQ. 'YEAR' ) then
            startofgen(num_var) = startofyear
            endofgen(num_var)   = endofyear
            ingen(num_var)      = inyear
            sum1gen(num_var)    = sum1year
            genrec(num_var)     = yearrec
#ifndef ALLOW_GENCOST_FREEFORM
         else
            STOP 'gencost_avgperiod wrongly specified'
#endif
         end if
      end do
      
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
