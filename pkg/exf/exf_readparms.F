c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_readparms.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_readparms(
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE exf_readparms
c     ==================================================================
c
c     o This routine initialises the package that calculates external
c       forcing fields for a given timestep of the MITgcmUV. Parameters
c       for this package are set in "data.externalforcing". Some additional
c       precompiler switches have to be specified in "EXF_CPPOPTIONS.h" .
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c
c     changed: Ralf.Giering@FastOpt.de 25-Mai-20000
c              - moved relaxation and climatology stuff to extra routines
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - added obcs parameters
c
c     ==================================================================
c     SUBROUTINE exf_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "exf.h"
#include "exf_param.h"
#include "exf_constants.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer i

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c     Surface flux data.
      namelist /exf_nml/
     &    hfluxstartdate1,    hfluxstartdate2,   hfluxperiod,
     &    atempstartdate1,    atempstartdate2,   atempperiod,
     &      aqhstartdate1,      aqhstartdate2,     aqhperiod,
     &    sfluxstartdate1,    sfluxstartdate2,   sfluxperiod,
     &   precipstartdate1,   precipstartdate2,  precipperiod,
     &  ustressstartdate1,  ustressstartdate2, ustressperiod,
     &  vstressstartdate1,  vstressstartdate2, vstressperiod,
     &    uwindstartdate1,    uwindstartdate2,   uwindperiod,
     &    vwindstartdate1,    vwindstartdate2,   vwindperiod,
     &   swfluxstartdate1,   swfluxstartdate2,  swfluxperiod,
     &   lwfluxstartdate1,   lwfluxstartdate2,  lwfluxperiod,
     &     obcsstartdate1,     obcsstartdate2,    obcsperiod,
     &          hfluxfile,          atempfile,       aqhfile,
     &         precipfile,          sfluxfile,   ustressfile,
     &        vstressfile,          uwindfile,     vwindfile,
     &         swfluxfile,         lwfluxfile,      obcsfile,
     &          exf_iprec,         exf_yftype

      _BEGIN_MASTER(mythid)

c     Set default values.

c     Calendar data.
      hfluxstartdate1    = 0
      hfluxstartdate2    = 0
      hfluxperiod        = 0.0 _d 0

      atempstartdate1    = 0
      atempstartdate2    = 0
      atempperiod        = 0.0 _d 0

      aqhstartdate1      = 0
      aqhstartdate2      = 0
      aqhperiod          = 0.0 _d 0

      sfluxstartdate1    = 0
      sfluxstartdate2    = 0
      sfluxperiod        = 0.0 _d 0

      precipstartdate1   = 0
      precipstartdate2   = 0
      precipperiod       = 0.0 _d 0

      ustressstartdate1  = 0
      ustressstartdate2  = 0
      ustressperiod      = 0.0 _d 0

      vstressstartdate1  = 0
      vstressstartdate2  = 0
      vstressperiod      = 0.0 _d 0

      uwindstartdate1    = 0
      uwindstartdate2    = 0
      uwindperiod        = 0.0 _d 0

      vwindstartdate1    = 0
      vwindstartdate2    = 0
      vwindperiod        = 0.0 _d 0

      swfluxstartdate1   = 0
      swfluxstartdate2   = 0
      swfluxperiod       = 0.0 _d 0

      lwfluxstartdate1   = 0
      lwfluxstartdate2   = 0
      lwfluxperiod       = 0.0 _d 0

      obcsstartdate1     = 0
      obcsstartdate2     = 0
      obcsperiod         = 0.0 _d 0

c     Data files.
      hfluxfile          = ' '
      atempfile          = ' '
      aqhfile            = ' '
      precipfile         = ' '
      sfluxfile          = ' '
      ustressfile        = ' '
      vstressfile        = ' '
      uwindfile          = ' '
      vwindfile          = ' '
      swfluxfile         = ' '
      lwfluxfile         = ' '
      obcsfile           = ' '

c     Initialise the date arrays.
      do i = 1,4
         hfluxstartdate(i)    = 0
         atempstartdate(i)    = 0
         aqhstartdate(i)      = 0
         precipstartdate(i)   = 0
         sfluxstartdate(i)    = 0
         ustressstartdate(i)  = 0
         vstressstartdate(i)  = 0
         uwindstartdate(i)    = 0
         vwindstartdate(i)    = 0
         swfluxstartdate(i)   = 0
         lwfluxstartdate(i)   = 0
         obcsstartdate(i)     = 0
      enddo

c     Initialise file type and field precision
      exf_iprec       = 32
      exf_yftype      = 'RL'

c     Check for the availability of the right calendar version.
      if ( calendarversion .ne. usescalendarversion ) then
         print*,' exf_readparms: You are not using the appropriate'
         print*,'           version of the calendar package.'
         print*
         print*,' You are using Calendar version: ', calendarversion
         print*,' Please use    Calendar version: ', usescalendarversion
         stop ' stopped in exf_readparms.'
      endif

c     Next, read the forcing data file.
      call nml_filter( 'data.exf', scrunit1, myThid )
      if (scrunit1 .eq. 0) then
         stop 'exf_readparms: reading namelist failed'
      end if
      read(  scrunit1, nml = exf_nml )
      close( scrunit1 )

c     Complete the start date specifications for the forcing
c     fields to get a complete calendar date array.

c     check for consistency

      if (.NOT. 
     &     (exf_iprec .EQ. 32 .OR. exf_iprec .EQ. 64)
     &     ) then
         stop 'stop in exf_readparms: value of exf_iprec not allowed'
      else if (.NOT. 
     &        (exf_yftype .EQ. 'RS' .OR. 
     &        exf_yftype .EQ. 'RL')
     &        ) then
         stop 'stop in exf_readparms: value of exf_yftype not allowed'
      end if

#ifdef ALLOW_BULKFORMULAE

#ifdef ALLOW_ATM_TEMP
      call cal_FullDate(   atempstartdate1,   atempstartdate2,
     &                     atempstartdate,            mythid )
      call cal_FullDate(     aqhstartdate1,     aqhstartdate2,
     &                       aqhstartdate,            mythid )
      call cal_FullDate(  swfluxstartdate1,  swfluxstartdate2,
     &                    swfluxstartdate,            mythid )
      call cal_FullDate(  lwfluxstartdate1,  lwfluxstartdate2,
     &                    lwfluxstartdate,            mythid )
      call cal_FullDate(  precipstartdate1,  precipstartdate2,
     &                    precipstartdate,            mythid )
#else
      call cal_FullDate(   hfluxstartdate1,   hfluxstartdate2,
     &                     hfluxstartdate,            mythid )
      call cal_FullDate(   sfluxstartdate1,  sfluxstartdate2,
     &                     sfluxstartdate,           mythid )
#ifdef ALLOW_KPP
      call cal_FullDate(  swfluxstartdate1,  swfluxstartdate2,
     &                    swfluxstartdate,            mythid )
#endif

#endif

#ifdef ALLOW_ATM_WIND
      call cal_FullDate(   uwindstartdate1,   uwindstartdate2,
     &                     uwindstartdate,            mythid )
      call cal_FullDate(   vwindstartdate1,   vwindstartdate2,
     &                     vwindstartdate,            mythid )
#else
      call cal_FullDate( ustressstartdate1, ustressstartdate2,
     &                   ustressstartdate,            mythid )
      call cal_FullDate( vstressstartdate1, vstressstartdate2,
     &                   vstressstartdate,            mythid )
#endif

#else
      call cal_FullDate(   hfluxstartdate1,  hfluxstartdate2,
     &                     hfluxstartdate,           mythid )
      call cal_FullDate(   sfluxstartdate1,  sfluxstartdate2,
     &                     sfluxstartdate,           mythid )
      call cal_FullDate( ustressstartdate1, ustressstartdate2,
     &                   ustressstartdate,            mythid )
      call cal_FullDate( vstressstartdate1, vstressstartdate2,
     &                   vstressstartdate,            mythid )
#ifdef ALLOW_KPP
      call cal_FullDate(  swfluxstartdate1,  swfluxstartdate2,
     &                    swfluxstartdate,            mythid )
#endif

#endif

#ifdef ALLOW_OBCS
      call cal_FullDate(   obcsstartdate1,  obcsstartdate2,
     &                     obcsstartdate,           mythid )
#endif

      _END_MASTER( mythid )

      _BARRIER

c--   Summarize the External forcing's setup.
      call exf_summary( mythid )


c--   set climatology parameters
      call exf_clim_readparms( mythid )

c--   summarize climatologic forcing configuration
      call exf_clim_summary( mythid )

      end
