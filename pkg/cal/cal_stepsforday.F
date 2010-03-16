C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_stepsforday.F,v 1.3 2010/03/16 00:11:46 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_StepsForDay(
     I                            iday,
     O                            firststep,
     O                            laststep,
     O                            nsteps,
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE cal_StepsForDay
c     ==================================================================
c
c     o Given the current day of the integration this routine returns
c       first, the last and the number of model steps the will have to
c       be performed.
c
c       This routine also checks consistency of variables quite
c       extensively.
c
c     started: Christian Eckert eckert@mit.edu  06-Apr-2000
c
c     changed:
c
c     ==================================================================
c     SUBROUTINE cal_StepsForDay
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer iday
      integer firststep
      integer laststep
      integer nsteps
      integer mythid

c     == local variables ==

      integer ierr
      integer mdstep
      integer numdays
      integer numsteps
      integer frac1
      integer frac2
      integer frac3
      integer frac4
      integer fullsteps
      integer firstyear
      integer firstmonth
      integer firstday
      integer lyfirst
      integer startsecs
      integer lastday
      integer endsecs

c     == external ==

      external cal_IntDays
      integer  cal_IntDays
      external cal_IsLeap
      integer  cal_IsLeap

c     == end of interface ==

      numdays    = cal_IntDays( mythid )
      lyfirst    = cal_IsLeap( firstyear, mythid )

      mdstep     = int(modelstep)

      firstyear  = modelstartdate(1)/10000
      firstmonth = mod(modelstartdate(1)/100,100)
      firstday   = mod(modelstartdate(1),100)
      lastday    = mod(modelenddate(1),100)

      startsecs  = (modelstartdate(2)/10000)*secondsperhour +
     &             mod(modelstartdate(2)/100,100)*secondsperminute +
     &             mod(modelstartdate(2),100)
      endsecs    = (modelenddate(2)/10000)*secondsperhour +
     &             mod(modelenddate(2)/100,100)*secondsperminute +
     &             mod(modelenddate(2),100)

      if ( numdays .eq. 1 ) then
        if ( iday .eq. firstday ) then
c--       Get the number of steps in the first day.
          if ( firstday .eq. lastday ) then
            firststep = 1
            laststep  = modelintsteps
          else if ( mod(firstday+1,ndaymonth(firstmonth,lyfirst)) .eq.
     &              lastday ) then
c--         This can only happen if we end at midnight of the next day.
            if ( modelenddate(2) .eq. 0 ) then
              firststep = 1
              laststep  = modelintsteps
c--           Note: This holds only if modelenddate was determined
c--                 such that it coincides with the model final time.
            else
c--           We do not end at midnight of the first day of
c--           the next month.
              ierr = 2604
              call cal_PrintError( ierr, mythid )
              stop ' stopped in cal_StepsForDay.'
            endif
          else
c--         The first and the last day are inconsistent with iday.
            ierr = 2603
            call cal_PrintError( ierr, mythid )
            stop ' stopped in cal_StepsForDay.'
          endif
        else
c--       The variables numdays and iday are inconsistent;
c--       ( iday .gt. numdays ).
          ierr = 2602
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_StepsForDay.'
        endif

      else if ( numdays .gt. 1 ) then
c--     More than one day of integration.
        if ( iday .eq. 1 ) then
          firststep = 1
          laststep  = int((secondsperday - startsecs)/mdstep)
        else if ( ( iday .gt. 1      ) .and.
     &            ( iday .lt. numdays) ) then
c--       Somewhere between first and last month.
c--       The first steps in iday.
          fullsteps = int((secondsperday - startsecs)/mdstep)
          numsteps  = fullsteps
c--       What is left in the first day (frac1).
          frac1     = (secondsperday - startsecs) - fullsteps*mdstep
          fullsteps = int(secondsperday/modelstep)
c--       What is left in a complete day (frac2).
          frac2     = secondsperday - fullsteps*mdstep
c--       What is left up to the current day (frac3).
          frac3     = frac1 + frac2*(iday - 1)
          numsteps  = numsteps + (iday - 1)*fullsteps +
     &                frac3/mdstep
          laststep  = numsteps
          firststep = laststep - secondsperday/mdstep + 1

        else if ( iday .eq. numdays ) then
c--       The last day of integration.
c--       The first step in iday.
          fullsteps = int((secondsperday - startsecs)/mdstep)
          numsteps  = fullsteps
c--       What is left in the first day (frac1).
          frac1     = (secondsperday - startsecs) - fullsteps*mdstep
          fullsteps = int(secondsperday/modelstep)
c--       What is left in a complete day (frac2).
          frac2     = secondsperday - fullsteps*mdstep
c--       What is left up to the day before the last (frac3).
          frac3     = frac1 + frac2*(iday - 2)
          numsteps  = numsteps + (iday - 2)*fullsteps
c--       The last step in iday.
          if ( modelenddate(2) .eq. 0 ) then
c--         This can only happen if we end at midnight of the next day.
            laststep  = numsteps + fullsteps
            firststep = numsteps + 1
c--         Note: There should be no fraction left
c--               ( mod(frac3,mdstep) = frac3/mdstep ) if modelenddate
c--               is based on an integral number of timesteps.
          else
            frac4     = frac3 + endsecs
            numsteps  = numsteps + frac4/mdstep
            laststep  = numsteps

c--         Note: There should be no fraction left
c--               ( mod(frac4,mdstep = frac4/mdstep ) if modelenddate
c--               is based on an integral number of timesteps.

            firststep = laststep - endsecs/mdstep + 1
          endif
        else
c--       The variables iday and numdays are inconsistent.
          ierr = 2605
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_DaysForMonth.'
        endif
      else
c--     The number of days to integrate is wrong; check cal_IntDays.
        ierr = 2601
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_StepsForDay.'
      endif

c--   The number of days to integrate in the given month.
      nsteps = laststep - firststep + 1

      return
      end

