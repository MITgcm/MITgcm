c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffieldrec.F,v 1.6 2003/03/06 00:47:33 heimbach Exp $

#include "CPP_OPTIONS.h"


      subroutine exf_GetFFieldRec(
     I                             fldstartdate, fldperiod,
     O                             fac,
     O                             first,
     O                             changed,
     O                             count0,
     O                             count1,
     I                             mytime,
     I                             myiter,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_GetFFieldRec
c     ==================================================================
c
c     o Get flags, counters, and the linear interpolation factor for a
c       given field.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c              Curtis Heisey cheisey@mit.edu    19-Dec-2002
c              - added "repeatPeriod" for cycling of forcing datasets
c
c              Dimitris Menemenlis menemenlis@jpl.nasa.gov 27-Dec-2002
c              - bug fix for verification/global_with_exf
c
c     ==================================================================
c     SUBROUTINE exf_GetFFieldRec
c     ==================================================================

      implicit none

c     == global variables ==

c cal: modelstart, modelstep
#include "EEPARAMS.h"
#include "cal.h"
#include "exf_param.h"

c     == routine arguments ==

      integer fldstartdate(4)
      _RL     fldperiod
      _RL     fac
      logical first
      logical changed
      integer count0
      integer count1
      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      integer mydate(4)
      integer previousdate(4)
      integer difftime(4)

      integer fldcount
      _RL     fldsecs
      _RL     fldsecs0
      _RL     fldsecs1
      integer prevfldcount
      _RL     prevfldsecs
      integer flddate(4)


#ifdef EXF_VERBOSE
      character*(max_len_mbuf) msgbuf
#endif

c     == end of interface ==

c     Determine the current date.
      call cal_GetDate( myiter, mytime, mydate, mythid )

c     Determine the flux record just before mycurrentdate.
      call cal_TimePassed( fldstartdate, mydate, difftime, mythid )
      call cal_ToSeconds( difftime, fldsecs, mythid )
      fldsecs  = int((fldsecs+0.5)/fldperiod)*fldperiod
      fldcount = int((fldsecs+0.5)/fldperiod) + 1

c     If using repeating data (e.g. monthly means) then make
c     fldsecs cycle around
      if (repeatPeriod.ne.0.) then
         fldsecs0=mod(fldsecs,repeatPeriod)
c        print *,'repeat: ',fldsecs
c     Determine the flux record just after mycurrentdate.
         count0 = int(fldsecs0/fldperiod) + 1
         fldsecs1  = int((fldsecs+fldperiod+0.5)/fldperiod)*fldperiod
         fldsecs1=mod(fldsecs1,repeatPeriod+0.5)
c        print *,'repeat: ',fldsecs1
         count1 = int((fldsecs1+0.5)/fldperiod) + 1
      endif

c     Set switches for reading new records.
      first = ((mytime - modelstart) .lt. 0.5*modelstep)

      if ( first) then
        changed = .false.
      else
        call cal_GetDate( myiter-1, mytime-modelstep,
     &                    previousdate, mythid )

        call cal_TimePassed( fldstartdate, previousdate, difftime,
     &                       mythid )
        call cal_ToSeconds( difftime, prevfldsecs, mythid )
        prevfldsecs  = int((prevfldsecs+0.5)/fldperiod)*fldperiod
        prevfldcount = int((prevfldsecs+0.5)/fldperiod) + 1

        if (fldcount .ne. prevfldcount) then
          changed = .true.
        else
          changed = .false.
        endif
      endif

      if (.NOT.repeatPeriod.ne.0.) then
         count0 = fldcount
         count1 = fldcount + 1
      endif

      call cal_TimeInterval( fldsecs, 'secs', difftime, mythid )
      call cal_AddTime( fldstartdate, difftime, flddate, mythid )
      call cal_TimePassed( flddate, mydate, difftime, mythid )
      call cal_ToSeconds( difftime, fldsecs, mythid )

c     Weight belonging to irec for linear interpolation purposes.
c     Note: The weight as chosen here is 1. - fac of the "old" MITgcm's
c           estimation program.
      fac = 1. - fldsecs/fldperiod

#ifdef EXF_VERBOSE
c     Do some printing for the protocol.
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,2x,l2,2x,l2,2x,D15.8)')
     &    ' exf_GetFFieldsRec: first, changed, fac:',
     &                         first, changed, fac
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i4,i4)')
     &    ' exf_GetFFieldsRec:      count0, count1:',
     &                              count0, count1
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      end
