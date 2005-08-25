C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_init_fixed.F,v 1.1 2005/08/25 16:10:42 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_init_fixed( mythid )

c     ==================================================================
c     SUBROUTINE seaice_cost_init_fixed
c     ==================================================================
c
c     o Set contributions to the cost function and the cost function
c       itself to zero. The cost function and the individual contribu-
c       tions are defined in the header file "SEAICE_COST.h".
c
c     ==================================================================
c     SUBROUTINE seaice_cost_init_fixed
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SEAICE_COST.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer tempDate1(4)
      integer tempDate2(4)

      logical exst

c     == external functions ==

#ifdef ALLOW_CAL
      integer  cal_IntMonths
      external cal_IntMonths
      integer  cal_IntDays
      external cal_IntDays
#endif

c     == end of interface ==

#ifdef ALLOW_CAL

      _BEGIN_MASTER( myThid )

c--   Sea-ice cost function start and stop times in model seconds.

        call cal_FullDate  ( costIceStart1 , costIceStart2,
     &                       tempDate1     , mythid )
        call cal_TimePassed( modelstartdate, tempDate1    ,
     &                       tempDate2     , mythid )
        call cal_ToSeconds ( tempDate2 , costIceStart , mythid )
        costIceStart = costIceStart + startTime

        call cal_FullDate  ( costIceEnd1   , costIceEnd2  ,
     &                       tempDate1     , mythid )
        call cal_TimePassed( modelstartdate, tempDate1    ,
     &                       tempDate2     , mythid )
        call cal_ToSeconds ( tempDate2 , costIceEnd , mythid )
        costIceEnd = costIceEnd + startTime

      _END_MASTER( mythid )

#endif /* ALLOW_CAL */

c--   Get the weights that are to be used for the individual cost
c--   function contributions.
cph      call seaice_cost_weights( mythid )

c--   Initialise adjoint of monthly mean files calculated
c--   in cost_averagesfields (and their ad...).
cph(
cph   The following init. shoud not be applied if in the middle
cph   of a divided adjoint run
cph)
#ifndef ALLOW_TANGENTLINEAR_RUN
cph!!! and I think it needs to be seen by TAF
cph!!! for repeated TLM runs
cph!!!
      inquire( file='costfinal', exist=exst )
      if ( .NOT. exst) then
cph         call seaice_cost_init_barfiles( mythid )
      endif
#endif

      _BARRIER

      end
