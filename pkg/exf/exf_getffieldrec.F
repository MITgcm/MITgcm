C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffieldrec.F,v 1.16 2008/01/25 11:23:06 dimitri Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_GetFFieldRec(
     I                             fldstartdate, fldperiod,
     I                             fldstartdate1, fldstartdate2,
     I                             usefldyearlyfields,
     O                             fac,
     O                             first, changed,
     O                             count0, count1, year0, year1,
     I                             mytime,
     I                             myiter, mythid
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
c     menemenlis@jpl.nasa.gov
c     27-Dec-2002 bug fix for verification/global_with_exf
c     8-Oct-2003 speed-up computations for long integration interval
c
c     ==================================================================
c     SUBROUTINE exf_GetFFieldRec
c     ==================================================================

      implicit none

c     == global variables ==

c cal: modelstart, modelstep
#include "EEPARAMS.h"
#include "cal.h"
#include "EXF_PARAM.h"

c     == routine arguments ==

      _RL     fldstartdate, fldperiod
      integer fldstartdate1, fldstartdate2
      logical usefldyearlyfields
      _RL     fac
      logical first, changed
      integer count0, count1, year0, year1
      _RL     mytime
      integer myiter, mythid

c     == local variables ==

      integer mydate(4)
      integer previousdate(4)
      integer nextperiod(4)
      integer difftime(4)

      _RL     fldsecs, fldsectot, fldsecs0, fldsecs1
      _RL     prevfldsecs
      integer prevfldcount

      integer date_array(4)
      integer startinyear(4)
      integer yi,yf,yp,yn
      integer mi,mf,mp,mn
      integer di,df,dp,dn
      integer si,sf,sp,sn
      integer li,lf,lp,ln
      integer wi,wf,wp,wn
      integer nextiter
      _RL nexttime

#ifdef EXF_VERBOSE
      character*(max_len_mbuf) msgbuf
#endif

c     == end of interface ==

c     Set some default values.
      first = ((mytime - modelstart) .lt. 0.5*modelstep)
      changed = .false.

      if (.not.usefldyearlyfields) then

c     Determine offset in seconds from beginning of input data
c     to current date.
       fldsecs = mytime - fldstartdate

c     Variable needed to set switches for reading new records.
       prevfldsecs = 0.
       if ( .not. first ) prevfldsecs = fldsecs - modelstep

c     Determine the flux records just before and after mycurrentdate.
       if ( repeatPeriod .eq. 0. ) then

        if ( fldsecs .lt. 0 ) then
         print *, 'flux data not available for this date'
         stop 'ABNORMAL END: S/R EXF_GETFFIELDREC'
        endif
        count0      = int((fldsecs+0.5)/fldperiod) + 1
        count1      = count0 + 1
        prevfldcount= int((prevfldsecs+0.5)/fldperiod) + 1
        fldsecs     = fldsecs - int((fldsecs+0.5)/fldperiod)*fldperiod

       else
c     if ( repeatPeriod .gt. 0. )

c     if using repeating data then make fldsecs cycle around.
        do while ( fldsecs     .lt. 0 )
         fldsecs  = fldsecs     + repeatPeriod
        enddo
        fldsecs0    = mod(fldsecs,repeatPeriod)
        count0      = int((fldsecs0+0.5)/fldperiod) + 1
        fldsecs1    = mod(fldsecs+fldperiod,repeatPeriod)
        count1      = int((fldsecs1+0.5)/fldperiod) + 1
        do while ( prevfldsecs .lt. 0 )
         prevfldsecs = prevfldsecs + repeatPeriod
        enddo
        prevfldsecs = mod(prevfldsecs,repeatPeriod)
        prevfldcount= int((prevfldsecs+0.5)/fldperiod) + 1
        fldsecs     = fldsecs0-int((fldsecs0+0.5)/fldperiod)*fldperiod

       endif

c     Weight belonging to count0 for linear interpolation purposes.
       fac = 1. - fldsecs/fldperiod

c     Set switch for reading new record.
       if (.not.first .and. count0.ne.prevfldcount) changed = .true.

      else
c     if (usefldyearlyfields)

c     Compute year, month, day, second (yi, mi, di, si) of field start date
       call cal_FullDate( fldstartdate1, fldstartdate2,
     &      date_array, mythid )
       call cal_ConvDate( date_array,yi,mi,di,si,li,wi,mythid )

c     Determine the flux year and record just before mycurrentdate.
       call cal_GetDate( myiter, mytime, mydate, mythid )
       call cal_ConvDate( mydate,yf,mf,df,sf,lf,wf,mythid )
       year0 = yf
       if ( mf.lt.mi .or. (mf.eq.mi .and. df.lt.di) .or.
     &      (mf.eq.mi .and. df.eq.di .and. sf.lt.si) )
     &      year0 = yf - 1
       startinyear(1) = year0*10000 + 100 + 1
       startinyear(2) = date_array(2)
       startinyear(3) = date_array(3)
       startinyear(4) = date_array(4)
       call cal_TimePassed( startinyear, mydate, difftime, mythid )
       call cal_ToSeconds( difftime, fldsectot, mythid )
       count0  = int((fldsectot+0.5)/fldperiod) + 1

c     Weight belonging to count0 for linear interpolation purposes.
       fldsecs = fldsectot - int((fldsectot+0.5)/fldperiod)*fldperiod
       fac = 1. - fldsecs/fldperiod

c     Determine the flux year and record just after mycurrentdate.
       nexttime = fldsecs + fldperiod
       nextiter = int(nexttime/modelstep +0.0001)
       call cal_GetDate( nextiter, nexttime, nextperiod, mythid )
       call cal_ConvDate( nextperiod,yn,mn,dn,sn,ln,wn,mythid )
       year1 = yn
       if ( mn.lt.mi .or. (mn.eq.mi .and. dn.lt.di) .or.
     &      (mn.eq.mi .and. dn.eq.di .and. sn.lt.si) )
     &      year1 = yn - 1
       startinyear(1) = year1*10000 + 100 + 1
       startinyear(2) = date_array(2)
       startinyear(3) = date_array(3)
       startinyear(4) = date_array(4)
       call cal_TimePassed( startinyear, mydate, difftime, mythid )
       call cal_ToSeconds( difftime, fldsectot, mythid )
       count1  = int((fldsectot+0.5)/fldperiod) + 1

c     Set switch for reading new record.
       if ( .not. first ) then
        call cal_GetDate(myiter-1,mytime-modelstep,previousdate,mythid)
        call cal_ConvDate( previousdate,yp,mp,dp,sp,lp,wp,mythid )
        if ( mp.lt.mi .or. (mp.eq.mi .and. dp.lt.di) .or.
     &       (mp.eq.mi .and. dp.eq.di .and. sp.lt.si) )
     &       yp = yp - 1
        startinyear(1) = year0*10000 + 100 + 1
        startinyear(2) = date_array(2)
        startinyear(3) = date_array(3)
        startinyear(4) = date_array(4)
        call cal_TimePassed( startinyear, mydate, difftime, mythid )
        call cal_ToSeconds( difftime, fldsectot, mythid )
        prevfldcount = int((fldsectot+0.5)/fldperiod) + 1
        if (yp.ne.year0 .or. prevfldcount.ne.count0) changed = .true.
       endif

      endif

#ifdef EXF_VERBOSE
c     Do some printing for the protocol.
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' exf_GetFFieldsRec:'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,2x,l2,2x,l2,2x,D15.8)')
     &    ' exf_GetFFieldsRec: first, changed, fac:',
     &                         first, changed, fac
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,3(1x,i6))')
     &    ' exf_GetFFieldsRec: myiter, count0, count1:',
     &                         myiter, count0, count1
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' exf_GetFFieldsRec:'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      end
