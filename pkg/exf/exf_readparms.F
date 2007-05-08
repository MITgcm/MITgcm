C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_readparms.F,v 1.47 2007/05/08 03:49:33 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
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
c       precompiler switches have to be specified in "EXF_OPTIONS.h".
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
#include "PARAMS.h"
#include "cal.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer i, idummy
      integer date_array(4), difftime(4)
      integer iUnit

      character*(max_len_mbuf) msgbuf

c     == end of interface ==

c     Surface flux data.
      NAMELIST /EXF_NML_01/
     &      windstressmax,       repeatPeriod,    exf_albedo,
     &   ocean_emissivity,     ice_emissivity, snow_emissivity,
     &          exf_iceCd,          exf_iceCe,     exf_iceCh,
     &   exf_scal_BulkCdn,     climtempfreeze,
     &          exf_iprec,         exf_yftype,   exf_monFreq,
     & useExfYearlyFields,  twoDigitYear,   useExfCheckRange,
     & useStabilityFct_overIce, selectStressGridPosition,
     & hu, ht, umin, atmrho, atmcp, cen2kel, gravity_mks,
     & cdrag_1, cdrag_2, cdrag_3, cstanton_1, cstanton_2, cdalton,
     & flamb, flami, zolmin, zref,
     & cvapor_fac, cvapor_exp, cvapor_fac_ice, cvapor_exp_ice,
     & humid_fac, gamma_blk, saltsat, psim_fac

      NAMELIST /EXF_NML_02/
     &          hfluxfile,          atempfile,       aqhfile,
     &          sfluxfile,         precipfile,    runofffile,
     &        ustressfile,        vstressfile,      evapfile,
     &     snowprecipfile,          uwindfile,     vwindfile,
     &         wspeedfile,         swfluxfile,    lwfluxfile,
     &      apressurefile,         swdownfile,    lwdownfile,
     &        climsstfile,        climsssfile,      
     &    hfluxstartdate1,    hfluxstartdate2,   hfluxperiod,
     &    atempstartdate1,    atempstartdate2,   atempperiod,
     &      aqhstartdate1,      aqhstartdate2,     aqhperiod,
     &    sfluxstartdate1,    sfluxstartdate2,   sfluxperiod,
     &     evapstartdate1,     evapstartdate2,    evapperiod,
     &   precipstartdate1,   precipstartdate2,  precipperiod,
     & snowprecipstartdate1, snowprecipstartdate2, snowprecipperiod,
     &   runoffstartdate1,   runoffstartdate2,  runoffperiod,
     &  ustressstartdate1,  ustressstartdate2, ustressperiod,
     &  vstressstartdate1,  vstressstartdate2, vstressperiod,
     &    uwindstartdate1,    uwindstartdate2,   uwindperiod,
     &    vwindstartdate1,    vwindstartdate2,   vwindperiod,
     &   wspeedstartdate1,   wspeedstartdate2,  wspeedperiod,
     &   swfluxstartdate1,   swfluxstartdate2,  swfluxperiod,
     &   lwfluxstartdate1,   lwfluxstartdate2,  lwfluxperiod,
     &   swdownstartdate1,   swdownstartdate2,  swdownperiod,
     &   lwdownstartdate1,   lwdownstartdate2,  lwdownperiod,
     &apressurestartdate1,apressurestartdate2,apressureperiod,
     &  climsststartdate1,  climsststartdate2,  climsstperiod,
     &  climsssstartdate1,  climsssstartdate2,  climsssperiod

      NAMELIST /EXF_NML_03/
     &   exf_inscal_hflux,  exf_inscal_sflux,      exf_inscal_evap,
     & exf_inscal_ustress,  exf_inscal_vstress,
     &   exf_inscal_uwind,  exf_inscal_vwind,    exf_inscal_wspeed,
     &   exf_inscal_atemp,  exf_offset_atemp,       exf_inscal_aqh,
     &     exf_inscal_sst,  exf_inscal_sss,
     &  exf_inscal_swflux,  exf_inscal_lwflux,   exf_inscal_precip,
     &  exf_inscal_runoff,  exf_inscal_apressure, exf_inscal_snowprecip,
     &  exf_inscal_swdown,  exf_inscal_lwdown,
     & exf_inscal_climsst, exf_inscal_climsss,
     &  exf_outscal_hflux,  exf_outscal_ustress, exf_outscal_vstress,
     & exf_outscal_swflux,  exf_outscal_sst,     exf_outscal_sss,
     &  exf_outscal_sflux,  exf_outscal_apressure,
     &  hfluxconst, atempconst, aqhconst, sfluxconst, evapconst, 
     &  precipconst, snowprecipconst, runoffconst, ustressconst,
     &  vstressconst, uwindconst, vwindconst, wspeedconst, swfluxconst,
     &  lwfluxconst, swdownconst, lwdownconst, apressureconst,
     &  climsstconst,   climsssconst,
     &     hflux_exfremo_intercept, hflux_exfremo_slope,
     &     atemp_exfremo_intercept, atemp_exfremo_slope,
     &     aqh_exfremo_intercept, aqh_exfremo_slope,
     &     sflux_exfremo_intercept, sflux_exfremo_slope,
     &     evap_exfremo_intercept, evap_exfremo_slope,
     &     precip_exfremo_intercept, precip_exfremo_slope,
     &     snowprecip_exfremo_intercept, snowprecip_exfremo_slope,
     &     runoff_exfremo_intercept, runoff_exfremo_slope,
     &     ustress_exfremo_intercept, ustress_exfremo_slope,
     &     vstress_exfremo_intercept, vstress_exfremo_slope,
     &     uwind_exfremo_intercept, uwind_exfremo_slope,
     &     vwind_exfremo_intercept, vwind_exfremo_slope,
     &     wspeed_exfremo_intercept, wspeed_exfremo_slope,
     &     swflux_exfremo_intercept, swflux_exfremo_slope,
     &     lwflux_exfremo_intercept, lwflux_exfremo_slope,
     &     swdown_exfremo_intercept, swdown_exfremo_slope,
     &     lwdown_exfremo_intercept, lwdown_exfremo_slope,
     &     apressure_exfremo_intercept, apressure_exfremo_slope,
     &     climsst_exfremo_intercept, climsst_exfremo_slope,
     &     climsss_exfremo_intercept, climsss_exfremo_slope

      NAMELIST /EXF_NML_04/
     & idummy
#ifdef USE_EXF_INTERPOLATION
     & , ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     & vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     & ustress_nlon, ustress_nlat, vstress_nlon, vstress_nlat,
     & hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     & sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     & hflux_nlon, hflux_nlat, sflux_nlon, sflux_nlat,
     & swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     & lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     & swflux_nlon, swflux_nlat, lwflux_nlon, lwflux_nlat,
     & atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     & atemp_nlon, atemp_nlat,
     & aqh_lon0, aqh_lon_inc, aqh_lat0, aqh_lat_inc, aqh_nlon, aqh_nlat,
     &evap_lon0,evap_lon_inc,evap_lat0,evap_lat_inc,evap_nlon,evap_nlat,
     & precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     & runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     & precip_nlon, precip_nlat, runoff_nlon, runoff_nlat,
     & snowprecip_lon0, snowprecip_lon_inc, snowprecip_nlon,
     & snowprecip_lat0, snowprecip_lat_inc, snowprecip_nlat,
     & uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     & vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     & uwind_nlon, uwind_nlat, vwind_nlon, vwind_nlat,
     & wspeed_lon0, wspeed_lon_inc, wspeed_lat0, wspeed_lat_inc,
     & wspeed_nlon, wspeed_nlat,
     & swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     & lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     & swdown_nlon, swdown_nlat, lwdown_nlon, lwdown_nlat,
     & apressure_lon0,apressure_lon_inc,apressure_nlon,
     & apressure_lat0,apressure_lat_inc,apressure_nlat,
     & climsst_lon0, climsst_lon_inc, climsst_nlon, 
     & climsst_lat0, climsst_lat_inc, climsst_nlat,
     & climsss_lon0, climsss_lon_inc, climsss_nlon, 
     & climsss_lat0, climsss_lat_inc, climsss_nlat
#endif

      NAMELIST /EXF_NML_OBCS/
     &    obcsNstartdate1,    obcsNstartdate2,   obcsNperiod,
     &    obcsSstartdate1,    obcsSstartdate2,   obcsSperiod,
     &    obcsEstartdate1,    obcsEstartdate2,   obcsEperiod,
     &    obcsWstartdate1,    obcsWstartdate2,   obcsWperiod

      _BEGIN_MASTER(mythid)

c     Set default values.

      year2sec           = 365.*86400.
      exf_monFreq        = monitorFreq

C-  default value should be set to main model parameter:
c     cen2kel     =  celsius2K
c     gravity_mks = gravity     <- not yet a parameter
c     atmcp       =  atm_Cp
c     humid_fac   =  atm_Rq     <- default is zero !!!

      cen2kel        =      273.150  _d 0
      gravity_mks    =        9.81   _d 0
      atmrho         =        1.200  _d 0
      atmcp          =     1005.000  _d 0
      flamb          =  2500000.000  _d 0
      flami          =   334000.000  _d 0
      cvapor_fac     =   640380.000  _d 0
      cvapor_exp     =     5107.400  _d 0
      cvapor_fac_ice = 11637800.000  _d 0
      cvapor_exp_ice =     5897.800  _d 0
      humid_fac      =        0.606  _d 0
      gamma_blk      =        0.010  _d 0
      saltsat        =        0.980  _d 0
      cdrag_1        =        0.0027000 _d 0
      cdrag_2        =        0.0001420 _d 0
      cdrag_3        =        0.0000764 _d 0
      cstanton_1     =        0.0327 _d 0
      cstanton_2     =        0.0180 _d 0
      cdalton        =        0.0346 _d 0
      zolmin         =     -100.000  _d 0
      psim_fac       =        5.000  _d 0
      zref           =       10.000  _d 0
      hu             =       10.000  _d 0
      ht             =        2.000  _d 0
      umin           =        0.5    _d 0
      useStabilityFct_overIce = .FALSE.
      exf_iceCd        = 1.63 _d -3
      exf_iceCe        = 1.63 _d -3
      exf_iceCh        = 1.63 _d -3
      exf_albedo       = 0.1 _d 0
c--   this default is chosen to be backward compatible with
c--   an earlier setting of 5.5 = ocean_emissivity*stefanBoltzmann
      ocean_emissivity = 5.50 _d-8 / 5.670 _d-8
      ice_emissivity   = 0.95 _d 0
      snow_emissivity  = 0.95 _d 0

c     Calendar data.
      hfluxstartdate1    = 0
      hfluxstartdate2    = 0
      hfluxperiod        = 0.0 _d 0
      hfluxconst         = 0.0 _d 0
      hflux_exfremo_intercept = 0.0 _d 0
      hflux_exfremo_slope = 0.0 _d 0

      atempstartdate1    = 0
      atempstartdate2    = 0
      atempperiod        = 0.0 _d 0
      atempconst         = celsius2K
      atemp_exfremo_intercept = 0.0 _d 0
      atemp_exfremo_slope = 0.0 _d 0

      aqhstartdate1      = 0
      aqhstartdate2      = 0
      aqhperiod          = 0.0 _d 0
      aqhconst           = 0.0 _d 0
      aqh_exfremo_intercept = 0.0 _d 0
      aqh_exfremo_slope = 0.0 _d 0

      sfluxstartdate1    = 0
      sfluxstartdate2    = 0
      sfluxperiod        = 0.0 _d 0
      sfluxconst         = 0.0 _d 0
      sflux_exfremo_intercept = 0.0 _d 0
      sflux_exfremo_slope = 0.0 _d 0

      evapstartdate1   = 0
      evapstartdate2   = 0
      evapperiod       = 0.0 _d 0
      evapconst        = 0.0 _d 0
      evap_exfremo_intercept = 0.0 _d 0
      evap_exfremo_slope = 0.0 _d 0

      precipstartdate1   = 0
      precipstartdate2   = 0
      precipperiod       = 0.0 _d 0
      precipconst        = 0.0 _d 0
      precip_exfremo_intercept = 0.0 _d 0
      precip_exfremo_slope = 0.0 _d 0

      snowprecipstartdate1   = 0
      snowprecipstartdate2   = 0
      snowprecipperiod       = 0.0 _d 0
      snowprecipconst        = 0.0 _d 0
      snowprecip_exfremo_intercept = 0.0 _d 0
      snowprecip_exfremo_slope = 0.0 _d 0

      runoffstartdate1   = 0
      runoffstartdate2   = 0
      runoffperiod       = 0.0 _d 0
      runoffconst        = 0.0 _d 0
      runoff_exfremo_intercept = 0.0 _d 0
      runoff_exfremo_slope = 0.0 _d 0

      ustressstartdate1  = 0
      ustressstartdate2  = 0
      ustressperiod      = 0.0 _d 0
      ustressconst       = 0.0 _d 0
      ustress_exfremo_intercept = 0.0 _d 0
      ustress_exfremo_slope = 0.0 _d 0

      vstressstartdate1  = 0
      vstressstartdate2  = 0
      vstressperiod      = 0.0 _d 0
      vstressconst       = 0.0 _d 0
      vstress_exfremo_intercept = 0.0 _d 0
      vstress_exfremo_slope = 0.0 _d 0

      uwindstartdate1    = 0
      uwindstartdate2    = 0
      uwindperiod        = 0.0 _d 0
      uwindconst         = 0.0 _d 0
      uwind_exfremo_intercept = 0.0 _d 0
      uwind_exfremo_slope = 0.0 _d 0

      vwindstartdate1    = 0
      vwindstartdate2    = 0
      vwindperiod        = 0.0 _d 0
      vwindconst         = 0.0 _d 0
      vwind_exfremo_intercept = 0.0 _d 0
      vwind_exfremo_slope = 0.0 _d 0

      wspeedstartdate1    = 0
      wspeedstartdate2    = 0
      wspeedperiod        = 0.0 _d 0
      wspeedconst         = 0.0 _d 0
      wspeed_exfremo_intercept = 0.0 _d 0
      wspeed_exfremo_slope = 0.0 _d 0

      swfluxstartdate1   = 0
      swfluxstartdate2   = 0
      swfluxperiod       = 0.0 _d 0
      swfluxconst        = 0.0 _d 0
      swflux_exfremo_intercept = 0.0 _d 0
      swflux_exfremo_slope = 0.0 _d 0

      lwfluxstartdate1   = 0
      lwfluxstartdate2   = 0
      lwfluxperiod       = 0.0 _d 0
      lwfluxconst        = 0.0 _d 0
      lwflux_exfremo_intercept = 0.0 _d 0
      lwflux_exfremo_slope = 0.0 _d 0

      swdownstartdate1   = 0
      swdownstartdate2   = 0
      swdownperiod       = 0.0 _d 0
      swdownconst        = 0.0 _d 0
      swdown_exfremo_intercept = 0.0 _d 0
      swdown_exfremo_slope = 0.0 _d 0

      lwdownstartdate1   = 0
      lwdownstartdate2   = 0
      lwdownperiod       = 0.0 _d 0
      lwdownconst        = 0.0 _d 0
      lwdown_exfremo_intercept = 0.0 _d 0
      lwdown_exfremo_slope = 0.0 _d 0

      apressurestartdate1    = 0
      apressurestartdate2    = 0
      apressureperiod        = 0.0 _d 0
      apressureconst         = 0.0 _d 0
      apressure_exfremo_intercept = 0.0 _d 0
      apressure_exfremo_slope = 0.0 _d 0

      climsststartdate1  = 0
      climsststartdate2  = 0
      climsstperiod      = 0
      climsstconst         = 0.0 _d 0
      climsst_exfremo_intercept = 0.0 _d 0
      climsst_exfremo_slope = 0.0 _d 0

      climsssstartdate1  = 0
      climsssstartdate2  = 0
      climsssperiod      = 0
      climsssconst         = 0.0 _d 0
      climsss_exfremo_intercept = 0.0 _d 0
      climsss_exfremo_slope = 0.0 _d 0

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

      repeatPeriod       = 0.0 _d 0
      windstressmax      = 2.0 _d 0

      exf_scal_BulkCdn   = 1.0  _d 0

c     Initialise freezing temperature of sea water
      climtempfreeze     = -1.9 _d 0

c     Data files.
      hfluxfile          = ' '
      atempfile          = ' '
      aqhfile            = ' '
      evapfile           = ' '
      precipfile         = ' '
      snowprecipfile     = ' '
      sfluxfile          = ' '
      runofffile         = ' '
      ustressfile        = ' '
      vstressfile        = ' '
      uwindfile          = ' '
      vwindfile          = ' '
      wspeedfile         = ' '
      swfluxfile         = ' '
      lwfluxfile         = ' '
      swdownfile         = ' '
      lwdownfile         = ' '
      apressurefile      = ' '
      climsstfile        = ' '
      climsssfile        = ' '

c     Start dates.
      hfluxstartdate     = 0.
      atempstartdate     = 0.
      aqhstartdate       = 0.
      evapstartdate      = 0.
      precipstartdate    = 0.
      snowprecipstartdate = 0.
      sfluxstartdate     = 0.
      runoffstartdate    = 0.
      ustressstartdate   = 0.
      vstressstartdate   = 0.
      uwindstartdate     = 0.
      vwindstartdate     = 0.
      wspeedstartdate    = 0.
      swfluxstartdate    = 0.
      lwfluxstartdate    = 0.
      swdownstartdate    = 0.
      lwdownstartdate    = 0.
      obcsNstartdate     = 0.
      obcsSstartdate     = 0.
      obcsEstartdate     = 0.
      obcsWstartdate     = 0.
      apressurestartdate = 0.
      climsststartdate   = 0.
      climsssstartdate   = 0.

c     Initialise file type and field precision
      exf_iprec            = 32
      exf_yftype           = 'RL'
      useExfYearlyFields   = .FALSE.
      twoDigitYear         = .FALSE.
      useExfCheckRange     = .TRUE.
      selectStressGridPosition = UNSET_I

c     Input scaling factors.
      exf_inscal_hflux     =  1. _d 0
      exf_inscal_sflux     =  1. _d 0
      exf_inscal_ustress   =  1. _d 0
      exf_inscal_vstress   =  1. _d 0
      exf_inscal_uwind     =  1. _d 0
      exf_inscal_vwind     =  1. _d 0
      exf_inscal_wspeed    =  1. _d 0
      exf_inscal_swflux    =  1. _d 0
      exf_inscal_lwflux    =  1. _d 0
      exf_inscal_precip    =  1. _d 0
      exf_inscal_snowprecip =  1. _d 0
      exf_inscal_sst       =  1. _d 0
      exf_inscal_sss       =  1. _d 0
      exf_inscal_atemp     =  1. _d 0
      exf_offset_atemp     =  0. _d 0
      exf_inscal_aqh       =  1. _d 0
      exf_inscal_evap      =  1. _d 0
      exf_inscal_apressure =  1. _d 0
      exf_inscal_runoff    =  1. _d 0
      exf_inscal_swdown    =  1. _d 0
      exf_inscal_lwdown    =  1. _d 0
      exf_inscal_climsst   =  1. _d 0
      exf_inscal_climsss   =  1. _d 0

c     Output scaling factors.
      exf_outscal_hflux    =  1. _d 0
      exf_outscal_sflux    =  1. _d 0
      exf_outscal_ustress  =  1. _d 0
      exf_outscal_vstress  =  1. _d 0
      exf_outscal_swflux   =  1. _d 0
      exf_outscal_sst      =  1. _d 0
      exf_outscal_sss      =  1. _d 0
      exf_outscal_apressure=  1. _d 0

#ifdef USE_EXF_INTERPOLATION
      ustress_lon0   = thetaMin
      uwind_lon0     = thetaMin 
      vstress_lon0   = thetaMin + delX(1) / 2
      hflux_lon0     = thetaMin + delX(1) / 2
      sflux_lon0     = thetaMin + delX(1) / 2
      swflux_lon0    = thetaMin + delX(1) / 2
      runoff_lon0    = thetaMin + delX(1) / 2 
      atemp_lon0     = thetaMin + delX(1) / 2
      aqh_lon0       = thetaMin + delX(1) / 2 
      evap_lon0      = thetaMin + delX(1) / 2
      precip_lon0    = thetaMin + delX(1) / 2 
      snowprecip_lon0 = thetaMin + delX(1) / 2 
      vwind_lon0     = thetaMin + delX(1) / 2 
      wspeed_lon0    = thetaMin + delX(1) / 2 
      lwflux_lon0    = thetaMin + delX(1) / 2 
      swdown_lon0    = thetaMin + delX(1) / 2 
      lwdown_lon0    = thetaMin + delX(1) / 2 
      apressure_lon0 = thetaMin + delX(1) / 2
      vstress_lat0   = phimin
      vwind_lat0     = phimin
      wspeed_lat0    = phimin
      ustress_lat0   = phimin   + delY(1) / 2
      hflux_lat0     = phimin   + delY(1) / 2
      sflux_lat0     = phimin   + delY(1) / 2
      runoff_lat0    = phimin   + delY(1) / 2
      swflux_lat0    = phimin   + delY(1) / 2
      atemp_lat0     = phimin   + delY(1) / 2
      aqh_lat0       = phimin   + delY(1) / 2
      evap_lat0      = phimin   + delY(1) / 2
      precip_lat0    = phimin   + delY(1) / 2
      snowprecip_lat0 = phimin   + delY(1) / 2
      uwind_lat0     = phimin   + delY(1) / 2
      lwflux_lat0    = phimin   + delY(1) / 2
      swdown_lat0    = phimin   + delY(1) / 2
      lwdown_lat0    = phimin   + delY(1) / 2
      apressure_lat0 = phimin   + delY(1) / 2
      ustress_nlon   = Nx
      ustress_nlat   = Ny
      vstress_nlon   = Nx
      vstress_nlat   = Ny
      hflux_nlon     = Nx
      hflux_nlat     = Ny
      sflux_nlon     = Nx
      sflux_nlat     = Ny
      swflux_nlon    = Nx
      swflux_nlat    = Ny
      runoff_nlon    = Nx
      runoff_nlat    = Ny
      atemp_nlon     = Nx
      atemp_nlat     = Ny
      aqh_nlon       = Nx
      aqh_nlat       = Ny
      evap_nlon      = Nx
      evap_nlat      = Ny
      precip_nlon    = Nx
      snowprecip_nlon = Nx
      precip_nlat    = Ny
      snowprecip_nlat = Ny
      uwind_nlon     = Nx
      uwind_nlat     = Ny
      vwind_nlon     = Nx
      vwind_nlat     = Ny
      wspeed_nlon    = Nx
      wspeed_nlat    = Ny
      lwflux_nlon    = Nx
      lwflux_nlat    = Ny
      swdown_nlon    = Nx
      swdown_nlat    = Ny
      lwdown_nlon    = Nx
      lwdown_nlat    = Ny
      apressure_nlon = Nx
      apressure_nlat = Ny
      Ustress_lon_inc   = delX(1)
      vstress_lon_inc   = delX(1)
      hflux_lon_inc     = delX(1)
      sflux_lon_inc     = delX(1)
      swflux_lon_inc    = delX(1)
      runoff_lon_inc    = delX(1)
      atemp_lon_inc     = delX(1)
      aqh_lon_inc       = delX(1)
      evap_lon_inc      = delX(1)
      precip_lon_inc    = delX(1)
      snowprecip_lon_inc = delX(1)
      uwind_lon_inc     = delX(1)
      vwind_lon_inc     = delX(1)
      wspeed_lon_inc    = delX(1)
      lwflux_lon_inc    = delX(1)
      swdown_lon_inc    = delX(1)
      lwdown_lon_inc    = delX(1)
      apressure_lon_inc = delX(1)
      climsst_lon0    = thetaMin + delX(1) / 2
      climsss_lon0    = thetaMin + delX(1) / 2
      climsst_lat0    = phimin   + delY(1) / 2
      climsss_lat0    = phimin   + delY(1) / 2
      climsst_nlon    = Nx
      climsst_nlat    = Ny
      climsss_nlon    = Nx
      climsss_nlat    = Ny
      climsst_lon_inc = delX(1)
      climsss_lon_inc = delX(1)
      DO i=1,MAX_LAT_INC
         IF (i.LT.Ny) THEN
            vstress_lat_inc(i)   =  delY(i)
            vwind_lat_inc(i)     =  delY(i)
            wspeed_lat_inc(i)    =  delY(i)
            ustress_lat_inc(i)   = (delY(i) + delY(i)) / 2.
            hflux_lat_inc(i)     = (delY(i) + delY(i)) / 2.
            sflux_lat_inc(i)     = (delY(i) + delY(i)) / 2.
            swflux_lat_inc(i)    = (delY(i) + delY(i)) / 2.
            runoff_lat_inc(i)    = (delY(i) + delY(i)) / 2.
            atemp_lat_inc(i)     = (delY(i) + delY(i)) / 2.
            aqh_lat_inc(i)       = (delY(i) + delY(i)) / 2.
            evap_lat_inc(i)      = (delY(i) + delY(i)) / 2.
            precip_lat_inc(i)    = (delY(i) + delY(i)) / 2.
            snowprecip_lat_inc(i) = (delY(i) + delY(i)) / 2.
            uwind_lat_inc(i)     = (delY(i) + delY(i)) / 2.
            lwflux_lat_inc(i)    = (delY(i) + delY(i)) / 2.
            swdown_lat_inc(i)    = (delY(i) + delY(i)) / 2.
            lwdown_lat_inc(i)    = (delY(i) + delY(i)) / 2.
            apressure_lat_inc(i) = (delY(i) + delY(i)) / 2.
            climsst_lat_inc(i)   = (delY(i) + delY(i)) / 2.
            climsss_lat_inc(i)   = (delY(i) + delY(i)) / 2.
         ELSE
            ustress_lat_inc(i)   = 0.
            vstress_lat_inc(i)   = 0.
            hflux_lat_inc(i)     = 0.
            sflux_lat_inc(i)     = 0.
            swflux_lat_inc(i)    = 0.
            runoff_lat_inc(i)    = 0.
            atemp_lat_inc(i)     = 0.
            aqh_lat_inc(i)       = 0.
            evap_lat_inc(i)      = 0.
            precip_lat_inc(i)    = 0.
            snowprecip_lat_inc(i) = 0.
            uwind_lat_inc(i)     = 0.
            vwind_lat_inc(i)     = 0.
            wspeed_lat_inc(i)    = 0.
            lwflux_lat_inc(i)    = 0.
            swdown_lat_inc(i)    = 0.
            lwdown_lat_inc(i)    = 0.
            apressure_lat_inc(i) = 0.
            climsst_lat_inc(i)   = 0.
            climsss_lat_inc(i)   = 0.
         ENDIF
      ENDDO
#endif /* USE_EXF_INTERPOLATION */

c     Next, read the forcing data file.
      WRITE(msgBuf,'(A)') 'EXF_READPARMS: opening data.exf'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.exf', 'EXF_READPARMS',
     O                          iUnit,
     I                          myThid )

      WRITE(msgBuf,'(A)') 
     &     'EXF_READPARMS: reading EXF_NML_01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      READ(  iUnit, nml = EXF_NML_01 )
      WRITE(msgBuf,'(A)') 
     &     'EXF_READPARMS: reading EXF_NML_02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      READ(  iUnit, nml = EXF_NML_02 )
      WRITE(msgBuf,'(A)') 
     &     'EXF_READPARMS: reading EXF_NML_03'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      READ(  iUnit, nml = EXF_NML_03 )
      WRITE(msgBuf,'(A)') 
     &     'EXF_READPARMS: reading EXF_NML_04'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      READ(  iUnit, nml = EXF_NML_04 )
#ifdef ALLOW_OBCS
      WRITE(msgBuf,'(A)') 
     &     'EXF_READPARMS: reading EXF_NML_OBCS'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      READ(  iUnit, nml = EXF_NML_OBCS )
#endif

      WRITE(msgBuf,'(A)') 
     &     'EXF_READPARMS: finished reading data.exf'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                SQUEEZE_RIGHT , 1)

      CLOSE( iUnit )

c     Hard-wire certain parameters
      hq = ht

      call exf_check( mythid )

c     Complete the start date specifications for the forcing
c     fields to get a complete calendar date array.

#ifdef ALLOW_ATM_WIND
      if ( uwindfile .NE. ' ' .and. uwindperiod .gt. 0. ) then
         call cal_FullDate  ( uwindstartdate1    ,      uwindstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,    uwindstartdate     ,mythid )
         uwindstartdate     = modelstart   + uwindstartdate
      endif
      if ( vwindfile .NE. ' ' .and. vwindperiod .gt. 0. ) then
         call cal_FullDate  ( vwindstartdate1    ,      vwindstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,    vwindstartdate     ,mythid )
         vwindstartdate     = modelstart   + vwindstartdate
      endif
      if ( wspeedfile .NE. ' ' .and. wspeedperiod .gt. 0. ) then
         call cal_FullDate  ( wspeedstartdate1, wspeedstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,    wspeedstartdate     ,mythid )
         wspeedstartdate     = modelstart   + wspeedstartdate
      endif
#else
      if ( ustressfile .NE. ' ' .and. ustressperiod .gt. 0. ) then
         call cal_FullDate  ( ustressstartdate1  ,    ustressstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,  ustressstartdate     ,mythid )
         ustressstartdate   = modelstart + ustressstartdate
      endif
      if ( vstressfile .NE. ' ' .and. vstressperiod .gt. 0. ) then
         call cal_FullDate  ( vstressstartdate1  ,    vstressstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,  vstressstartdate     ,mythid )
         vstressstartdate   = modelstart + vstressstartdate
      endif
#endif

#ifdef ALLOW_ATM_TEMP
      if ( atempfile .NE. ' ' .and. atempperiod .gt. 0. ) then
         call cal_FullDate  ( atempstartdate1    ,      atempstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,    atempstartdate     ,mythid )
         atempstartdate     = modelstart   + atempstartdate
      endif
      if ( aqhfile .NE. ' ' .and. aqhperiod .gt. 0. ) then
         call cal_FullDate  ( aqhstartdate1      ,        aqhstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,      aqhstartdate     ,mythid )
         aqhstartdate       = modelstart     + aqhstartdate
      endif
      if ( lwfluxfile .NE. ' ' .and. lwfluxperiod .gt. 0. ) then
         call cal_FullDate  ( lwfluxstartdate1   ,     lwfluxstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   lwfluxstartdate     ,mythid )
         lwfluxstartdate    = modelstart  + lwfluxstartdate
      endif
      if ( precipfile .NE. ' ' .and. precipperiod .gt. 0. ) then
         call cal_FullDate  ( precipstartdate1   ,     precipstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   precipstartdate     ,mythid )
         precipstartdate    = modelstart  + precipstartdate
      endif
      if ( snowprecipfile .NE. ' ' .and. snowprecipperiod .gt. 0. ) then
         call cal_FullDate( snowprecipstartdate1, snowprecipstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime, snowprecipstartdate, mythid )
         snowprecipstartdate    = modelstart  + snowprecipstartdate
      endif
#else
      if ( hfluxfile .NE. ' ' .and. hfluxperiod .gt. 0. ) then
         call cal_FullDate  ( hfluxstartdate1    ,      hfluxstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,    hfluxstartdate     ,mythid )
         hfluxstartdate     = modelstart   + hfluxstartdate
      endif
      if ( sfluxfile .NE. ' ' .and. sfluxperiod .gt. 0. ) then
         call cal_FullDate  ( sfluxstartdate1    ,      sfluxstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,    sfluxstartdate     ,mythid )
         sfluxstartdate     = modelstart   + sfluxstartdate
      endif
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      if ( swfluxfile .NE. ' ' .and. swfluxperiod .gt. 0. ) then
         call cal_FullDate  ( swfluxstartdate1   ,     swfluxstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   swfluxstartdate     ,mythid )
         swfluxstartdate    = modelstart  + swfluxstartdate
      endif
#endif

#ifdef EXF_READ_EVAP
      if ( evapfile .NE. ' ' .and. evapperiod .gt. 0. ) then
         call cal_FullDate  ( evapstartdate1     ,       evapstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,     evapstartdate     ,mythid )
         evapstartdate      = modelstart    + evapstartdate
      endif
#endif

#ifdef ALLOW_RUNOFF
      if ( runofffile .NE. ' ' .AND. runoffperiod .gt. 0. ) then
         call cal_FullDate  ( runoffstartdate1   ,     runoffstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   runoffstartdate     ,mythid )
         runoffstartdate    = modelstart  + runoffstartdate
      endif
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      if ( swdownfile .NE. ' ' .and. swdownperiod .gt. 0. ) then
         call cal_FullDate  ( swdownstartdate1   ,     swdownstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   swdownstartdate     ,mythid )
         swdownstartdate    = modelstart  + swdownstartdate
      endif
      if ( lwdownfile .NE. ' ' .and. lwdownperiod .gt. 0. ) then
         call cal_FullDate  ( lwdownstartdate1   ,     lwdownstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   lwdownstartdate     ,mythid )
         lwdownstartdate    = modelstart  + lwdownstartdate
      endif
#endif

#ifdef ATMOSPHERIC_LOADING
      if ( apressurefile .NE. ' ' .and. apressureperiod .gt. 0. ) then
         call cal_FullDate  ( apressurestartdate1,  apressurestartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,  apressurestartdate   ,mythid )
         apressurestartdate = modelstart + apressurestartdate
      endif
#endif

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_NORTH
      if ( obcsNperiod .gt. 0 ) then
         call cal_FullDate  ( obcsNstartdate1    ,      obcsNstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   obcsNstartdate      ,mythid )
         obcsNstartdate     = modelstart + obcsNstartdate
      endif
#endif
#ifdef ALLOW_OBCS_SOUTH
      if ( obcsSperiod .gt. 0 ) then
         call cal_FullDate  ( obcsSstartdate1    ,      obcsSstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   obcsSstartdate      ,mythid )
         obcsSstartdate     = modelstart + obcsSstartdate
      endif
#endif
#ifdef ALLOW_OBCS_EAST
      if ( obcsEperiod .gt. 0 ) then
         call cal_FullDate  ( obcsEstartdate1    ,      obcsEstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,   obcsEstartdate      ,mythid )
         obcsEstartdate     = modelstart + obcsEstartdate
      endif
#endif
#ifdef ALLOW_OBCS_WEST
      if ( obcsWperiod .gt. 0 ) then
         call cal_FullDate  ( obcsWstartdate1    ,      obcsWstartdate2,
     &        date_array                        ,mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds ( difftime,  obcsWstartdate       ,mythid )
         obcsWstartdate     = modelstart + obcsWstartdate
      endif
#endif
#endif /* ALLOW_OBCS */

#ifdef USE_EXF_INTERPOLATION
      if ( climsst_nlat .GT. MAX_LAT_INC )
     & stop 'stopped in exf_clim_readparms: climsst_nlat > MAX_LAT_INC'
      if ( climsss_nlat .GT. MAX_LAT_INC )
     & stop 'stopped in exf_clim_readparms: climsss_nlat > MAX_LAT_INC'
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
      if ( climsstfile .NE. ' ' .AND. climsstperiod .NE. 0. ) then
         call cal_FullDate( climsststartdate1, climsststartdate2,
     &        date_array,                  mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds (difftime,       climsststartdate  ,mythid)
         climsststartdate=modelstart+climsststartdate
      endif
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      if ( climsssfile .NE. ' ' .AND. climsssperiod .NE. 0. ) then
         call cal_FullDate( climsssstartdate1, climsssstartdate2,
     &        date_array,                  mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds (difftime,       climsssstartdate  ,mythid)
         climsssstartdate=modelstart+climsssstartdate
      endif
#endif

      _END_MASTER( mythid )

      _BARRIER

c--   Summarize the External forcing's setup.
      call exf_summary( mythid )

      RETURN
      END
