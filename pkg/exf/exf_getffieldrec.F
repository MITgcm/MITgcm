c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffieldrec.F,v 1.13 2005/05/19 21:46:15 ce107 Exp $

#include "EXF_OPTIONS.h"


      subroutine exf_GetFFieldRec(
     I                             fldstartdate, fldperiod,
     I                             fldstartdate1, fldstartdate2,
     I                             usefldyearlyfields,
     O                             fac,
     O                             first,
     O                             changed,
     O                             count0,
     O                             count1,
     O                             yp, 
     O                             yf,
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
#include "exf_param.h"

c     == routine arguments ==

      _RL     fldstartdate
      _RL     fldperiod
      integer fldstartdate1
      integer fldstartdate2
      logical usefldyearlyfields
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
      integer nextperiod(4)
      integer difftime(4)

      integer fldcount
      _RL     fldsecs
      _RL     fldsectot
      _RL     fldsecs0
      _RL     fldsecs1
      _RL     prevfldsecs
      integer  prevfldcount

      integer iprint
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

c     Determine offset in seconds from beginning of input data
c     to current date.

c     This is very slow for a long integration interval.
c     call cal_GetDate( myiter, mytime, mydate, mythid )
c     call cal_TimePassed( fldstartdate, mydate, difftime, mythid )
c     call cal_ToSeconds( difftime, fldsecs, mythid )

      fldsecs = mytime - fldstartdate

c     Variables needed to set switches for reading new records.
      first = ((mytime - modelstart) .lt. 0.5*modelstep)
      if ( .not. first ) then

c      This is very slow for a long integration interval.
c      call cal_GetDate(myiter-1,mytime-modelstep,previousdate,mythid)
c      call cal_TimePassed(fldstartdate,previousdate,difftime,mythid )
c      call cal_ToSeconds( difftime, prevfldsecs, mythid )

       prevfldsecs = fldsecs - modelstep

      else
       prevfldsecs = 0
      endif

c     Determine the flux records just before and after mycurrentdate.
      if (repeatPeriod.eq.0.) then

         if ( fldsecs .lt. 0 ) then
            print *, 'flux data not available for this date'
            STOP 'ABNORMAL END: S/R EXF_GETFFIELDREC'
         endif
         count0      = int((fldsecs+0.5)/fldperiod) + 1
         count1      = count0 + 1
         prevfldcount= int((prevfldsecs+0.5)/fldperiod) + 1
         fldsecs     = fldsecs - int((fldsecs+0.5)/fldperiod)*fldperiod

      elseif (repeatPeriod.gt.0.) then

c     If using repeating data (e.g. monthly means) then make
c     fldsecs cycle around.
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

      else

         print *, 'repeatPeriod must be positive'
         STOP 'ABNORMAL END: S/R EXF_GETFFIELDREC'

      endif

c     Weight belonging to irec for linear interpolation purposes.
      fac = 1. - fldsecs/fldperiod

c     Set switches for reading new records.
      if ( first) then
         changed = .false.
      else
         if (count0 .ne. prevfldcount) then
            changed = .true.
         else
            changed = .false.
         endif
      endif

c ---------------------------------------------------------------------
c ---------------------------------------------------------------------

      if (usefldyearlyfields) then

        if (repeatPeriod.NE.0.) then
          print *, 'Use of usefldyearlyfields AND repeatPeriod',
     &            'not implemented'
          STOP 'ABNORMAL END: S/R EXF_GETFFIELDREC'
        endif

cph(
cph-exf-print        iprint = yp
cph)

c     overwrite count0/1 indices by those w.r.t. yearly files
c     fac, first, changed remain valid

        call cal_FullDate( fldstartdate1, fldstartdate2,
     &                  date_array, mythid )
        call cal_ConvDate( date_array,yi,mi,di,si,li,wi,mythid )

        call cal_GetDate( myiter, mytime, mydate, mythid )
        call cal_ConvDate( mydate,yf,mf,df,sf,lf,wf,mythid )

        if ( yf .EQ. yi ) then
           startinyear(1) = date_array(1)
        else if ( mf.EQ.1 .AND. df.EQ.1 .AND.
     &          mydate(2) .LT. date_array(2) ) then
           if ( (yf-1) .EQ. yi ) then
              startinyear(1) = date_array(1)
           else
              startinyear(1) = (yf-1)*10000 + 100 + 1
           endif
        else
           startinyear(1) = yf*10000 + 100 + 1
           yi = yf
           if ( mf.EQ.1 .AND. df.EQ.1 .AND.
     &          mydate(2) .EQ. date_array(2) ) then
              first = .TRUE.
           endif
        endif
        startinyear(2) = date_array(2)
        startinyear(3) = date_array(3)
        startinyear(4) = date_array(4)

cph-exf-print        if (iprint.EQ.3000) then
cph-exf-print           print *, 'ph-exf startin ', startinyear(1), startinyear(2)
cph-exf-print           print *, 'ph-exf mydate  ', mydate(1), mydate(2)
cph-exf-print        endif

        call cal_TimePassed( startinyear, mydate, difftime, mythid )
        call cal_ToSeconds( difftime, fldsectot, mythid )
        fldsecs  = int(fldsectot/fldperiod)*fldperiod
        fldcount = int(fldsecs/fldperiod) + 1

        if ( first) then
          changed = .false.
          yp = yf
        else
          call cal_GetDate( myiter-1, mytime-modelstep,
     &                   previousdate, mythid )
          call cal_ConvDate( previousdate,yp,mp,dp,sp,lp,wp,mythid )

          if ( yp .NE. yf ) then
             startinyear(1) = yp*10000 + 100 + 1
             startinyear(2) = date_array(2)
             startinyear(3) = previousdate(3)
             startinyear(4) = date_array(4)
          endif

          call cal_TimePassed( startinyear, previousdate, difftime,
     &                         mythid )
          call cal_ToSeconds( difftime, prevfldsecs, mythid )
          prevfldsecs  = int(prevfldsecs/fldperiod)*fldperiod
          prevfldcount = int(prevfldsecs/fldperiod) + 1

          if (fldcount .ne. prevfldcount) then
            changed = .true.
          else
            changed = .false.
          endif
        endif

        count0 = fldcount
        count1 = fldcount + 1

        nexttime = mytime - (fldsectot-fldsecs) + fldperiod
        nextiter = INT(nexttime/modelstep +0.0001)
        
cph-exf-print        if (iprint.EQ.3000) then
cph-exf-print           print *, 'ph-exf fldsec ', fldsectot, fldsecs
cph-exf-print           print *, 'ph-exf next ', nexttime, nexttime-mytime, 
cph-exf-print     &          INT((nexttime-mytime)/modelstep)
cph-exf-print        endif

        call cal_GetDate(
     &       nextiter, nexttime, nextperiod, mythid)
        call cal_ConvDate( nextperiod,yn,mn,dn,sn,ln,wn,mythid )
cph-exf-print        if (iprint.EQ.3000) print *, 'ph-exf nextperiod ', 
cph-exf-print     &       nextiter, nextperiod(1), nextperiod(2)
        if ( yn.GT.yi ) then
           count1 = 1
           yf = yn
        endif

      endif

cph-exf-print      if (iprint.EQ.3000) then
cph-exf-print         print *, 'ph-exf-rec yp, yf, yn     ', 
cph-exf-print     &        yp, yf, yn
cph-exf-print         print *, 'ph-exf-rec myiter, c0, c1 ', 
cph-exf-print     &        myiter, count0, count1, changed
cph-exf-print      endif

c ---------------------------------------------------------------------
c ---------------------------------------------------------------------

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
        write(msgbuf,'(a,3(1x,i6))')
     &    ' exf_GetFFieldsRec: myiter, count0, count1:',
     &                         myiter, count0, count1
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      end
