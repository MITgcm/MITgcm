c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_readparms.F,v 1.1 2001/05/14 22:08:40 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_clim_readparms(
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE exf_clim_readparms
c     ==================================================================
c
c     o This routine initialises the climatologic forcing
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-20000
c
c     ==================================================================
c     SUBROUTINE exf_clim_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "exf.h"

#include "exf_clim_param.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer i

c     == end of interface ==

c     Surface flux data.
      namelist /exf_clim_nml/
     & climtempstartdate1, climtempstartdate2, climtempperiod,
     & climsaltstartdate1, climsaltstartdate2, climsaltperiod,
     &  climsststartdate1,  climsststartdate2, climsstperiod,
     &  climsssstartdate1,  climsssstartdate2, climsssperiod,
     &       climtempfile,       climsaltfile,   climsstfile,
     &        climsssfile,
     &     exf_clim_iprec, exf_clim_yftype

      _BEGIN_MASTER(mythid)

c     Set default values.

c     Calendar data.
      climtempstartdate1 = 0
      climtempstartdate2 = 0
      climtempperiod     = 0

      climsaltstartdate1 = 0
      climsaltstartdate2 = 0
      climsaltperiod     = 0

      climsststartdate1  = 0
      climsststartdate2  = 0
      climsstperiod      = 0

      climsssstartdate1  = 0
      climsssstartdate2  = 0
      climsssperiod      = 0

c     Data files.
      climtempfile       = ' '
      climsaltfile       = ' '
      climsstfile        = ' '
      climsssfile        = ' '

c       Initialise the date arrays.
      do i = 1,4
         climtempstartdate(i) = 0
         climsaltstartdate(i) = 0
         climsststartdate(i)  = 0
         climsssstartdate(i)  = 0
      enddo

c     Initialise file type and field precision
      exf_clim_iprec  = 32
      exf_clim_yftype = 'RL'

c     Check for the availability of the right calendar version.
      if ( calendarversion .ne. usescalendarversion ) then
         print*,' exf_Init: You are not using the appropriate'
         print*,'           version of the calendar package.'
         print*
         print*,' Please use Calendar version: ',
     &        usescalendarversion
         stop ' stopped in exf_Init.'
      endif

c     Next, read the forcing data file.
      call nml_filter( 'data.exf_clim', scrunit1, myThid )
      if (scrunit1 .eq. 0) then
         stop 'exf_clim_readparms: reading namelist failed'
      end if
      read(  scrunit1, nml = exf_clim_nml )
      close( scrunit1 )

c     Complete the start date specifications for the forcing
c     fields to get a complete calendar date array.

c     check for consistency

      if (.NOT. (exf_clim_iprec .EQ. 32
     &      .OR. exf_clim_iprec .EQ. 64)) then
      stop 'stopped in exf_clim_readparms: value of iprec not allowed'
      else if
     &   (.NOT. (exf_clim_yftype .EQ. 'RS'
     &      .OR. exf_clim_yftype .EQ. 'RL'))
     &          then
      stop 'stopped in exf_clim_readparms: value of yftype not allowed'
      end if           

#ifdef ALLOW_CLIMTEMP_RELAXATION
        call cal_FullDate( climtempstartdate1, climtempstartdate2,
     &                     climtempstartdate,             mythid )
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
        call cal_FullDate( climsaltstartdate1, climsaltstartdate2,
     &                     climsaltstartdate,             mythid )
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
        call cal_FullDate( climsststartdate1, climsststartdate2,
     &                     climsststartdate,            mythid )
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
        call cal_FullDate( climsssstartdate1, climsssstartdate2,
     &                     climsssstartdate,            mythid )
#endif

      _END_MASTER( mythid )

      _BARRIER

      end
