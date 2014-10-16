C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_getrec.F,v 1.16 2014/10/16 20:04:23 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

      subroutine ctrl_GetRec(
     I                        thefield,
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
c     SUBROUTINE ctrl_GetRec
c     ==================================================================
c
c     o Get flags, counters, and the linear interpolation factor for a
c       given control vector contribution.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  24-Feb-2000
c
c              - Changed Routine names (package prefix: ecco_)
c
c     ==================================================================
c     SUBROUTINE ctrl_GetRec
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ctrl.h"
#include "CTRL_OBCS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif

c     == routine arguments ==

      character*(*) thefield
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

      logical lArgErr

      character*(max_len_mbuf) msgBuf
CML#ifdef ECCO_VERBOSE
CML      character*(max_len_mbuf) msgbuf
CML#endif

c     == end of interface ==

      write(msgBuf,'(A)')
     &     'Oops, I thought that this routine is never used.'
      call print_error( msgBuf , 1)
      write(msgBuf,'(A)')
     &     'To continue, remove the stop statement from ctrl_getrec.F'
      call print_error( msgBuf , 1)
      write(msgBuf,'(A)')
     &     'or use s/r ctrl_get_gen_rec instead.'
      call print_error( msgBuf , 1)
      stop 'ABNORMAL END: S/R CTRL_GETREC'

      lArgErr = .true.
      fldperiod = 0.

c     Map the field parameters.

      if ( thefield .eq. 'xx_obcsn'   ) then
         call cal_CopyDate(
     I        xx_obcsnstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_obcsnperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_obcss'   ) then
         call cal_CopyDate(
     I        xx_obcssstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_obcssperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_obcsw'   ) then
         call cal_CopyDate(
     I        xx_obcswstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_obcswperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_obcse'   ) then
         call cal_CopyDate(
     I        xx_obcsestartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_obcseperiod
         lArgErr = .false.
c
#ifdef ECCO_CTRL_DEPRECATED
      else if ( thefield .eq. 'xx_hflux'   ) then
         call cal_CopyDate(
     I        xx_hfluxstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_hfluxperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_atemp'   ) then
         call cal_CopyDate(
     I        xx_atempstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_atempperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_sflux'   ) then
         call cal_CopyDate(
     I        xx_sfluxstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_sfluxperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_aqh'   ) then
         call cal_CopyDate(
     I        xx_aqhstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_aqhperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_precip'   ) then
         call cal_CopyDate(
     I        xx_precipstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_precipperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_swflux'   ) then
         call cal_CopyDate(
     I        xx_swfluxstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_swfluxperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_swdown'   ) then
         call cal_CopyDate(
     I        xx_swdownstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_swdownperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_lwflux'   ) then
         call cal_CopyDate(
     I        xx_lwfluxstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_lwfluxperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_lwdown'   ) then
         call cal_CopyDate(
     I        xx_lwdownstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_lwdownperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_evap'   ) then
         call cal_CopyDate(
     I        xx_evapstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_evapperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_snowprecip'   ) then
         call cal_CopyDate(
     I        xx_snowprecipstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_snowprecipperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_apressure'   ) then
         call cal_CopyDate(
     I        xx_apressurestartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_apressureperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_runoff'   ) then
         call cal_CopyDate(
     I        xx_runoffstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_runoffperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_tauu'   ) then
         call cal_CopyDate(
     I        xx_tauustartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_tauuperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_uwind'   ) then
         call cal_CopyDate(
     I        xx_uwindstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_uwindperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_tauv'   ) then
         call cal_CopyDate(
     I        xx_tauvstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_tauvperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_vwind'   ) then
         call cal_CopyDate(
     I        xx_vwindstartdate,
     O        fldstartdate,
     I        mythid
     &           )
         fldperiod = xx_vwindperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_sst'   ) then
         call cal_CopyDate(
     I        xx_sststartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_sstperiod
         lArgErr = .false.
c
      else if ( thefield .eq. 'xx_sss'   ) then
         call cal_CopyDate(
     I        xx_sssstartdate,
     O        fldstartdate,
     I        mythid
     &        )
         fldperiod = xx_sssperiod
         lArgErr = .false.
#endif /* ECCO_CTRL_DEPRECATED */
      endif

c--   Check the field argument.
      if ( lArgErr ) then
         print*,' The subroutine *ctrl_GetRec* has been called'
         print*,' with an illegal field specification.'
         stop   ' ... stopped in ctrl_GetRec.'
      endif

c--   Determine the current date.
      call cal_GetDate( myiter, mytime, mydate, mythid )

c     Determine the flux record just before mycurrentdate.
      call cal_TimePassed( fldstartdate, mydate, difftime,
     &                     mythid )
      call cal_ToSeconds( difftime, fldsecs, mythid )
cgg   Added a 0.5 safety net.
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
cgg   Added a 0.5 safety net.
        prevfldsecs  = int((prevfldsecs+0.5)/fldperiod)*fldperiod
        prevfldcount = int((prevfldsecs+0.5)/fldperiod) + 1

        if (fldcount .ne. prevfldcount) then
          changed = .true.
        else
          changed = .false.
        endif
      endif

      count0 = fldcount
      count1 = fldcount + 1

      call cal_TimeInterval( fldsecs, 'secs', difftime, mythid )
      call cal_AddTime( fldstartdate, difftime, flddate, mythid )
      call cal_TimePassed( flddate, mydate, difftime, mythid )
      call cal_ToSeconds( difftime, fldsecs, mythid )

c     Weight belonging to irec for linear interpolation purposes.
c     Note: The weight as chosen here is 1. - fac of the "old"
c           MITgcm estimation program.
      fac = 1. - fldsecs/fldperiod

#ifdef ECCO_VERBOSE
c     Do some printing for the protocol.
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a)')
     &    ' ctrl_getrec:                 thefield:  ',
     &                                     thefield
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
