C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_get_gen_rec.F,v 1.5 2007/06/19 03:42:30 gforget Exp $

#include "PACKAGES_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"


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

#ifdef ECCO_VERBOSE
      character*(max_len_mbuf) msgbuf
#endif

c     == end of interface ==

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

c--   Determine the current date.
      call cal_GetDate( myiter, mytime, mydate, mythid )

c     Determine first record:
        call cal_TimePassed( fldstartdate, modelstartdate,
     &                       difftime, mythid )
        call cal_ToSeconds ( difftime, fldsecs, mythid )
        startrec = int((modelstart + startTime - fldsecs)/
     &                 fldperiod) + 1

c     Determine the flux record just before mycurrentdate.
      call cal_TimePassed( fldstartdate, mydate, difftime,
     &                     mythid )
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
c           MITgcm's estimation program.
      fac = 1. - fldsecs/fldperiod

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

#endif /* ALLOW_CAL */

      return
      end

