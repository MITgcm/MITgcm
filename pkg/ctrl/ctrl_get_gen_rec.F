C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_get_gen_rec.F,v 1.15 2012/08/10 19:38:57 jmc Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

      subroutine ctrl_get_gen_rec(
     I                        xx_genstartdate,
     I                        xx_genperiod,
     O                        fac,
     O                        first,
     O                        changed,
     O                        count0,
     O                        count1,
     I                        mytime,
     I                        myiter,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE ctrl_get_gen_rec
c     ==================================================================
c
c     o Get flags, counters, and the linear interpolation factor for a
c       given control vector contribution.
c     o New, generic, for new routine ctrl_get_gen
c
c     ==================================================================
c     SUBROUTINE ctrl_get_gen_rec
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ctrl.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#include "PARAMS.h"

c     == routine arguments ==

      integer xx_genstartdate(4)
      _RL     xx_genperiod
      _RL     fac
      logical first
      logical changed
      integer count0
      integer count1
      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

#ifdef ALLOW_CAL

      integer mydate(4)
      integer previousdate(4)
      integer difftime(4)

      integer fldcount
      _RL     fldsecs
      integer prevfldcount
      _RL     prevfldsecs
      integer flddate(4)

      integer fldstartdate(4)
      _RL     fldperiod

      integer startrec

      logical lArgErr
#else
C     Declarations for code, adapted from external_fields_load,
C     for simplied default model calendar without exf/cal
      _RL myRelTime, tmpFac
      INTEGER countP
#endif

#ifdef ECCO_VERBOSE
      character*(max_len_mbuf) msgbuf
#endif

c     == end of interface ==

#ifdef ALLOW_CAL
      lArgErr = .true.
      fldperiod = 0.

c     Map the field parameters.

      call cal_CopyDate(
     I     xx_genstartdate,
     O     fldstartdate,
     I     mythid
     &     )
      fldperiod = xx_genperiod
      lArgErr = .false.

c--   Check the field argument.
      if ( lArgErr ) then
         print*,' The subroutine *ctrl_get_gen_rec* has been called'
         print*,' with an illegal field specification.'
         stop   ' ... stopped in ctrl_get_gen_rec.'
      endif

      if ( xx_genperiod .eq. -12. _d 0 ) then
c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
       call cal_GetMonthsRec(
     O      fac, first, changed,
     O      count0, count1,
     I      mytime, myiter, mythid
     &      )
      elseif ( fldperiod .eq. 0. _d 0 ) then
c     Read field only once in the beginning. Hack: count1=count0 causes
c     the model to read the first record twice, but since this this is
c     done only the first time around it is not too much of an overhead.
       first   = ((mytime - modelstart) .lt. 0.5*modelstep)
       changed = .false.
       fac     = 1. _d 0
       count0  = 1
       count1  = count0
      else
c     fldperiod .ne. 0
c--   Determine the current date.
       call cal_GetDate( myiter, mytime, mydate, mythid )

c     Determine first record:
       call cal_TimePassed( fldstartdate, modelstartdate,
     &                      difftime, mythid )
       call cal_ToSeconds ( difftime, fldsecs, mythid )
       startrec = int((modelstart + startTime - fldsecs)/
     &                fldperiod) + 1

c     Determine the flux record just before mycurrentdate.
       call cal_TimePassed( fldstartdate, mydate, difftime,
     &                      mythid )
       call cal_ToSeconds( difftime, fldsecs, mythid )
       fldsecs  = int((fldsecs+0.5)/fldperiod)*fldperiod
       fldcount = int((fldsecs+0.5)/fldperiod) + 1

c     Set switches for reading new records.
       first = ((mytime - modelstart) .lt. 0.5*modelstep)

       if ( first) then
        changed = .false.
       else
        call cal_GetDate( myiter-1, mytime-modelstep,
     &                    previousdate, mythid )

        call cal_TimePassed( fldstartdate, previousdate,
     &                       difftime, mythid )
        call cal_ToSeconds( difftime, prevfldsecs, mythid )
        prevfldsecs  = int((prevfldsecs+0.5)/fldperiod)*fldperiod
        prevfldcount = int((prevfldsecs+0.5)/fldperiod) + 1

        if (fldcount .ne. prevfldcount) then
         changed = .true.
        else
         changed = .false.
        endif
       endif

c      count0 = fldcount
c      count1 = fldcount + 1
       count0 = fldcount - startrec + 1
       count1 = fldcount - startrec + 2

       call cal_TimeInterval( fldsecs, 'secs', difftime, mythid )
       call cal_AddTime( fldstartdate, difftime, flddate, mythid )
       call cal_TimePassed( flddate, mydate, difftime, mythid )
       call cal_ToSeconds( difftime, fldsecs, mythid )

c     Weight belonging to irec for linear interpolation purposes.
c     Note: The weight as chosen here is 1. - fac of the "old"
c           MITgcm estimation program.
       fac = 1. - fldsecs/fldperiod

c     fldperiod .ne. 0.
      endif
#else /* not ALLOW_CAL */
C     Code, adapted from external_fields_load, for simplied
C     default model calendar without exf/cal, but
C     based on myTime, myIter, deltaTclock, externForcingCycle, and startTime

      myRelTime = myTime - startTime
      first = (myRelTime .lt. 0.5*deltaTClock)
      if ( xx_genperiod .eq. 0. _d 0
     &     .or. externForcingCycle .eq. 0. _d 0 ) then
C     control parameter is constant in time and only needs to be updated
C     once in the beginning
       changed = .false.
       count0  = 1
       count1  = 1
       fac     = 1. _d 0
      else

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   countP, count0, count1, tmpFac, fac,
     I                   externForcingCycle, xx_genperiod,
     I                   deltaTclock, myTime, myThid )

       IF ( count0.NE.countP ) THEN
        changed = .true.
       ELSE
        changed = .false.
       ENDIF
       IF ( first ) changed = .false.

      endif

#endif /* ALLOW_CAL */

#ifdef ECCO_VERBOSE
c     Do some printing for the protocol.
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,2x,l2,2x,l2,2x,D15.8)')
     &    '                     first, changed, fac:',
     &                          first, changed, fac
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i4,i4)')
     &    '                          count0, count1:',
     &                               count0, count1
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      return
      end
