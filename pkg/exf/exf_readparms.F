c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_readparms.F,v 1.7 2003/03/06 00:47:33 heimbach Exp $

#include "EXF_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine exf_readparms( mythid )

c     ==================================================================
c     SUBROUTINE exf_readparms
c     ==================================================================
c
c     o This routine initialises the package that calculates external
c       forcing fields for a given timestep of the MITgcmUV. Parameters
c       for this package are set in "data.externalforcing". Some additional
c       precompiler switches have to be specified in "EXF_CPPOPTIONS.h".
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c     changed: Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c     changed: Ralf.Giering@FastOpt.de 25-Mai-20000
c              - moved relaxation and climatology to extra routines
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - added obcs parameters
c     changed: Virginie Thierry, vthierry@ucsd.edu 04-June-2001
c              - added new obcs parameters (for each boundaries) 
c     included runoff D. Stammer, Nov. 25, 2001
c     included pressure forcing. heimbach@mit.edu 05-Nov-2002
c     added "repeatPeriod" for cycling of forcing datasets 19-Dec-2002
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
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
     &    repeatPeriod,
     &    hfluxstartdate1,    hfluxstartdate2,   hfluxperiod,
     &    atempstartdate1,    atempstartdate2,   atempperiod,
     &      aqhstartdate1,      aqhstartdate2,     aqhperiod,
     &    sfluxstartdate1,    sfluxstartdate2,   sfluxperiod,
     &     evapstartdate1,     evapstartdate2,    evapperiod,
     &   precipstartdate1,   precipstartdate2,  precipperiod,
     &   runoffstartdate1,   runoffstartdate2,  runoffperiod,
     &  ustressstartdate1,  ustressstartdate2, ustressperiod,
     &  vstressstartdate1,  vstressstartdate2, vstressperiod,
     &    uwindstartdate1,    uwindstartdate2,   uwindperiod,
     &    vwindstartdate1,    vwindstartdate2,   vwindperiod,
     &   swfluxstartdate1,   swfluxstartdate2,  swfluxperiod,
     &   lwfluxstartdate1,   lwfluxstartdate2,  lwfluxperiod,
     &   swdownstartdate1,   swdownstartdate2,  swdownperiod,
     &   lwdownstartdate1,   lwdownstartdate2,  lwdownperiod,
     &    obcsNstartdate1,    obcsNstartdate2,   obcsNperiod,
     &    obcsSstartdate1,    obcsSstartdate2,   obcsSperiod,
     &    obcsEstartdate1,    obcsEstartdate2,   obcsEperiod,
     &    obcsWstartdate1,    obcsWstartdate2,   obcsWperiod,
     &apressurestartdate1,apressurestartdate2,apressureperiod,
     &          hfluxfile,          atempfile,       aqhfile,
     &          sfluxfile,         precipfile,    runofffile,
     &        ustressfile,        vstressfile,      evapfile,
     &          uwindfile,          vwindfile,
     &         swfluxfile,         lwfluxfile, apressurefile,
     &         swdownfile,         lwdownfile,
     &          exf_iprec,         exf_yftype,
     &     exf_inscal_hfl,     exf_inscal_ust, exf_inscal_vst,
     &     exf_inscal_swf,     exf_inscal_sst, exf_inscal_sss,
     &     exf_inscal_sfl,  exf_inscal_runoff,exf_inscal_apressure,
     &    exf_outscal_hfl,    exf_outscal_ust,exf_outscal_vst,
     &    exf_outscal_swf,    exf_outscal_sst,exf_outscal_sss,
     &    exf_outscal_sfl,  exf_outscal_apressure,
     &         hfluxconst,
     &         atempconst,
     &         aqhconst,
     &         sfluxconst,
     &         evapconst,
     &         precipconst,
     &         runoffconst,
     &         ustressconst,
     &         vstressconst,
     &         uwindconst,
     &         vwindconst,
     &         swfluxconst,
     &         lwfluxconst,
     &         swdownconst,
     &         lwdownconst,
     &         apressureconst,
     &         EXFwindOnBgrid

      _BEGIN_MASTER(mythid)

c     Set default values.

c     Calendar data.
      hfluxstartdate1    = 0
      hfluxstartdate2    = 0
      hfluxperiod        = 0.0 _d 0
      hfluxconst         = 0.0 _d 0

      atempstartdate1    = 0
      atempstartdate2    = 0
      atempperiod        = 0.0 _d 0
      atempconst         = 0.0 _d 0

      aqhstartdate1      = 0
      aqhstartdate2      = 0
      aqhperiod          = 0.0 _d 0
      aqhconst           = 0.0 _d 0

      sfluxstartdate1    = 0
      sfluxstartdate2    = 0
      sfluxperiod        = 0.0 _d 0
      sfluxconst         = 0.0 _d 0

      evapstartdate1   = 0
      evapstartdate2   = 0
      evapperiod       = 0.0 _d 0
      evapconst        = 0.0 _d 0

      precipstartdate1   = 0
      precipstartdate2   = 0
      precipperiod       = 0.0 _d 0
      precipconst        = 0.0 _d 0

      runoffstartdate1   = 0
      runoffstartdate2   = 0
      runoffperiod       = 0.0 _d 0
      runoffconst        = 0.0 _d 0

      ustressstartdate1  = 0
      ustressstartdate2  = 0
      ustressperiod      = 0.0 _d 0
      ustressconst       = 0.0 _d 0

      vstressstartdate1  = 0
      vstressstartdate2  = 0
      vstressperiod      = 0.0 _d 0
      vstressconst       = 0.0 _d 0

      uwindstartdate1    = 0
      uwindstartdate2    = 0
      uwindperiod        = 0.0 _d 0
      uwindconst         = 0.0 _d 0

      vwindstartdate1    = 0
      vwindstartdate2    = 0
      vwindperiod        = 0.0 _d 0
      vwindconst         = 0.0 _d 0

      swfluxstartdate1   = 0
      swfluxstartdate2   = 0
      swfluxperiod       = 0.0 _d 0
      swfluxconst        = 0.0 _d 0

      lwfluxstartdate1   = 0
      lwfluxstartdate2   = 0
      lwfluxperiod       = 0.0 _d 0
      lwfluxconst        = 0.0 _d 0

      swdownstartdate1   = 0
      swdownstartdate2   = 0
      swdownperiod       = 0.0 _d 0
      swdownconst        = 0.0 _d 0

      lwdownstartdate1   = 0
      lwdownstartdate2   = 0
      lwdownperiod       = 0.0 _d 0
      lwdownconst        = 0.0 _d 0

      obcsNstartdate1    = 0
      obcsNstartdate2    = 0
      obcsNperiod        = 0.0 _d 0

      obcsSstartdate1    = 0
      obcsSstartdate2    = 0
      obcsSperiod        = 0.0 _d 0

      obcsEstartdate1    = 0
      obcsEstartdate2    = 0
      obcsEperiod        = 0.0 _d 0

      obcsWstartdate1    = 0
      obcsWstartdate2    = 0
      obcsWperiod        = 0.0 _d 0

      apressurestartdate1    = 0
      apressurestartdate2    = 0
      apressureperiod        = 0.0 _d 0
      apressureconst         = 0.0 _d 0

      repeatPeriod           = 0.0 _d 0

c     Data files.
      hfluxfile          = ' '
      atempfile          = ' '
      aqhfile            = ' '
      evapfile           = ' '
      precipfile         = ' '
      sfluxfile          = ' '
      runofffile         = ' '
      ustressfile        = ' '
      vstressfile        = ' '
      uwindfile          = ' '
      vwindfile          = ' '
      swfluxfile         = ' '
      lwfluxfile         = ' '
      swdownfile         = ' '
      lwdownfile         = ' '
      apressurefile      = ' '

c     Initialise the date arrays.
      do i = 1,4
         hfluxstartdate(i)    = 0
         atempstartdate(i)    = 0
         aqhstartdate(i)      = 0
         evapstartdate(i)     = 0
         precipstartdate(i)   = 0
         sfluxstartdate(i)    = 0
         runoffstartdate(i)   = 0
         ustressstartdate(i)  = 0
         vstressstartdate(i)  = 0
         uwindstartdate(i)    = 0
         vwindstartdate(i)    = 0
         swfluxstartdate(i)   = 0
         lwfluxstartdate(i)   = 0
         swdownstartdate(i)   = 0
         lwdownstartdate(i)   = 0
         obcsNstartdate(i)    = 0
         obcsSstartdate(i)    = 0
         obcsEstartdate(i)    = 0
         obcsWstartdate(i)    = 0
         apressurestartdate(i)= 0
      enddo

c     Initialise file type and field precision
      exf_iprec       = 32
      exf_yftype      = 'RL'

c     Input scaling factors.
      exf_inscal_hfl       =  1. _d 0
      exf_inscal_ust       =  1. _d 0
      exf_inscal_vst       =  1. _d 0
      exf_inscal_swf       =  1. _d 0
      exf_inscal_sst       =  1. _d 0
      exf_inscal_sss       =  1. _d 0
      exf_inscal_apressure =  1. _d 0
      exf_inscal_sfl       =  1. _d 0
cds   convert runoff from m/yr to m/s and change sign.
      exf_inscal_runoff    = -1.0/(86400.*365.0)

c     Output scaling factors.
      exf_outscal_hfl      =  1. _d 0
      exf_outscal_ust      =  1. _d 0
      exf_outscal_vst      =  1. _d 0
      exf_outscal_swf      =  1. _d 0
      exf_outscal_sst      =  1. _d 0
      exf_outscal_sss      =  1. _d 0
      exf_outscal_apressure=  1. _d 0
      exf_outscal_sfl      =  1. _d 0
c
      EXFwindOnBgrid = .FALSE.

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

#ifdef ALLOW_ATM_WIND
      call cal_FullDate(    uwindstartdate1,    uwindstartdate2,
     &                      uwindstartdate ,             mythid )
      call cal_FullDate(    vwindstartdate1,    vwindstartdate2,
     &                      vwindstartdate ,             mythid )
#else
      call cal_FullDate(  ustressstartdate1,  ustressstartdate2,
     &                    ustressstartdate ,             mythid )
      call cal_FullDate(  vstressstartdate1,  vstressstartdate2,
     &                    vstressstartdate ,             mythid )
#endif

#ifdef ALLOW_ATM_TEMP
      call cal_FullDate(    atempstartdate1,    atempstartdate2,
     &                      atempstartdate ,             mythid )
      call cal_FullDate(      aqhstartdate1,      aqhstartdate2,
     &                        aqhstartdate ,             mythid )
      call cal_FullDate(   lwfluxstartdate1,   lwfluxstartdate2,
     &                     lwfluxstartdate ,             mythid )
      call cal_FullDate(   precipstartdate1,   precipstartdate2,
     &                     precipstartdate ,             mythid )
#else
      call cal_FullDate(    hfluxstartdate1,    hfluxstartdate2,
     &                      hfluxstartdate ,             mythid )
      call cal_FullDate(    sfluxstartdate1,    sfluxstartdate2,
     &                      sfluxstartdate ,             mythid )
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      call cal_FullDate(   swfluxstartdate1,   swfluxstartdate2,
     &                     swfluxstartdate ,             mythid )
#endif

#ifdef EXF_READ_EVAP
      call cal_FullDate(     evapstartdate1,     evapstartdate2,
     &                       evapstartdate ,             mythid )
#endif

#ifdef ALLOW_RUNOFF
      call cal_FullDate(   runoffstartdate1,   runoffstartdate2,
     &                     runoffstartdate ,             mythid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      call cal_FullDate(   swdownstartdate1,   swdownstartdate2,
     &                     swdownstartdate ,             mythid )
      call cal_FullDate(   lwdownstartdate1,   lwdownstartdate2,
     &                     lwdownstartdate ,             mythid )
#endif

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_NORTH
      call cal_FullDate(    obcsNstartdate1,    obcsNstartdate2,
     &                      obcsNstartdate ,             mythid )
#endif
#ifdef ALLOW_OBCS_SOUTH
      call cal_FullDate(    obcsSstartdate1,    obcsSstartdate2,
     &                      obcsSstartdate ,             mythid )
#endif
#ifdef ALLOW_OBCS_EAST
      call cal_FullDate(    obcsEstartdate1,    obcsEstartdate2,
     &                      obcsEstartdate ,             mythid )
#endif
#ifdef ALLOW_OBCS_WEST
      call cal_FullDate(    obcsWstartdate1,    obcsWstartdate2,
     &                      obcsWstartdate ,             mythid )
#endif
#endif /* ALLOW_OBCS */

#ifdef ATMOSPHERIC_LOADING
      call cal_FullDate(apressurestartdate1,apressurestartdate2,
     &                  apressurestartdate ,             mythid )
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
