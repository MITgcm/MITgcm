C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_init_fixed.F,v 1.6 2012/04/19 23:00:01 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#include "AD_CONFIG.h"

      subroutine seaice_cost_init_fixed( mythid )

C     ==================================================================
C     SUBROUTINE seaice_cost_init_fixed
C     ==================================================================
C
C     o Set contributions to the cost function and the cost function
C       itself to zero. The cost function and the individual contribu-
C       tions are defined in the header file "SEAICE_COST.h".
C
C     ==================================================================
C     SUBROUTINE seaice_cost_init_fixed
C     ==================================================================

      implicit none

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#ifdef ALLOW_COST
# include "SEAICE_COST.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     == routine arguments ==
      integer mythid

#ifdef ALLOW_COST
C     == local variables ==
      integer tempDate1(4)
      integer tempDate2(4)
      logical exst

C     == external functions ==
c#ifdef ALLOW_CAL
c     integer  cal_IntMonths
c     external cal_IntMonths
c     integer  cal_IntDays
c     external cal_IntDays
c#endif

C     == end of interface ==

#ifdef ALLOW_CAL

      _BEGIN_MASTER( myThid )

C--   Sea-ice cost function start and stop times in model seconds.
        call cal_FullDate  ( costIceStart1 , costIceStart2,
     &                       tempDate1     , mythid )
        call cal_TimePassed( modelStartDate, tempDate1    ,
     &                       tempDate2     , mythid )
        call cal_ToSeconds ( tempDate2 , costIceStart , mythid )
        costIceStart = costIceStart + startTime

        if ( costIceEnd1.EQ.0 ) then
C-      use "modelEndDate" as default value for costIceEnd
          costIceEnd1 = modelEndDate(1)
          costIceEnd2 = modelEndDate(2)
        endif
        call cal_FullDate  ( costIceEnd1   , costIceEnd2  ,
     &                       tempDate1     , mythid )
        call cal_TimePassed( modelStartDate, tempDate1    ,
     &                       tempDate2     , mythid )
        call cal_ToSeconds ( tempDate2 , costIceEnd , mythid )
        costIceEnd = costIceEnd + startTime
C--
        if ( smrareadatfile .ne. ' ' )
     &   call cal_FullDate( smrareastartdate1, smrareastartdate2,
     &                      smrareastartdate,  mythid )

      _END_MASTER( mythid )

#endif /* ALLOW_CAL */

C--   Get the weights that are to be used for the individual cost
C--   function contributions.
      call seaice_cost_weights( mythid )

C--   Initialise adjoint of monthly mean files calculated
C--   in cost_averagesfields (and their ad...).
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

#endif /* ALLOW_COST */

      RETURN
      END
