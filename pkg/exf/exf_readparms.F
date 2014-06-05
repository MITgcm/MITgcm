C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_readparms.F,v 1.81 2014/06/05 19:33:08 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#ifdef ALLOW_EXCH2
# include "W2_OPTIONS.h"
#endif /* ALLOW_EXCH2 */

      SUBROUTINE EXF_READPARMS( myThid )

C     ==================================================================
C     SUBROUTINE exf_readparms
C     ==================================================================
C
C     o This routine initialises the package that calculates external
C       forcing fields for a given timestep of the MITgcmUV. Parameters
C       for this package are set in "data.externalforcing". Some additional
C       precompiler switches have to be specified in "EXF_OPTIONS.h".
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C
C     changed: Christian Eckert eckert@mit.edu  11-Jan-2000
C              - Restructured the code in order to create a package
C                for the MITgcmUV.
C              Christian Eckert eckert@mit.edu  12-Feb-2000
C              - Changed Routine names (package prefix: exf_)
C     changed: Patrick Heimbach, heimbach@mit.edu  04-May-2000
C              - changed the handling of precip and sflux with respect
C                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
C     changed: Ralf.Giering@FastOpt.de 25-Mai-20000
C              - moved relaxation and climatology to extra routines
C              Patrick Heimbach, heimbach@mit.edu  04-May-2000
C              - added obcs parameters
C     changed: Virginie Thierry, vthierry@ucsd.edu 04-June-2001
C              - added new obcs parameters (for each boundaries)
C     included runoff D. Stammer, Nov. 25, 2001
C     included pressure forcing. heimbach@mit.edu 05-Nov-2002
C     added "repeatPeriod" for cycling of forcing datasets 19-Dec-2002
C     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C
C     ==================================================================
C     SUBROUTINE exf_readparms
C     ==================================================================

      implicit none

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef USE_EXF_INTERPOLATION
# ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
# endif /* ALLOW_EXCH2 */
# include "SET_GRID.h"
#endif /* USE_EXF_INTERPOLATION */
c#include "cal.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==
      INTEGER myThid

C     == local variables ==
#ifdef USE_EXF_INTERPOLATION
      INTEGER gridNx, gridNy
      INTEGER j
      _RL inp_lon0, inp_lat0, inp_dLon, inp_dLat
#endif /* USE_EXF_INTERPOLATION */
      INTEGER iUnit
      LOGICAL exf_verbose
      CHARACTER*(2) exf_yftype
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

C     Surface flux data.
      NAMELIST /EXF_NML_01/
     &      windstressmax,       repeatPeriod,    exf_albedo,
     &   ocean_emissivity,     ice_emissivity, snow_emissivity,
     &          exf_iceCd,          exf_iceCe,     exf_iceCh,
     &   exf_scal_BulkCdn,     climtempfreeze, useExfCheckRange,
     &      exf_iprec    ,   exf_iprec_obcs  , exf_yftype,
     &      exf_verbose  ,   exf_debugLev    , exf_monFreq,
     & useExfYearlyFields,  twoDigitYear,
     & useStabilityFct_overIce, readStressOnAgrid, readStressOnCgrid,
     & useAtmWind, useRelativeWind, noNegativeEvap,
     & select_ZenAlbedo, useExfZenIncoming,
     & hu, ht, umin, atmrho, atmcp, cen2kel, gravity_mks,
     & cdrag_1, cdrag_2, cdrag_3, cstanton_1, cstanton_2, cdalton,
     & flamb, flami, zolmin, zref,
     & cvapor_fac, cvapor_exp, cvapor_fac_ice, cvapor_exp_ice,
     & humid_fac, gamma_blk, saltsat, sstExtrapol, psim_fac

      NAMELIST /EXF_NML_02/
     &          hfluxfile,          atempfile,       aqhfile,
     &          sfluxfile,         precipfile,    runofffile,
     &                                         runoftempfile,
     &        ustressfile,        vstressfile,      evapfile,
     &     snowprecipfile,          uwindfile,     vwindfile,
     &         wspeedfile,         swfluxfile,    lwfluxfile,
     &      apressurefile,         swdownfile,    lwdownfile,
     &       areamaskfile,        climsstfile,   climsssfile,
     &                           climustrfile,  climvstrfile,
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
     &  areamaskstartdate1,areamaskstartdate2,areamaskperiod,
     &  climsststartdate1,  climsststartdate2, climsstperiod,
     &  climsssstartdate1,  climsssstartdate2, climsssperiod,
     & climustrstartdate1, climustrstartdate2,climustrperiod,
     & climvstrstartdate1, climvstrstartdate2,climvstrperiod,
     &   areamaskTauRelax,    climsstTauRelax, climsssTauRelax,
     &   climustrTauRelax,climvstrTauRelax

      NAMELIST /EXF_NML_03/
     &   exf_inscal_hflux,  exf_inscal_sflux,      exf_inscal_evap,
     & exf_inscal_ustress,  exf_inscal_vstress,
     &   exf_inscal_uwind,  exf_inscal_vwind,    exf_inscal_wspeed,
     &   exf_inscal_atemp,  exf_offset_atemp,       exf_inscal_aqh,
     &     exf_inscal_sst,  exf_inscal_sss,
     &  exf_inscal_swflux,  exf_inscal_lwflux,   exf_inscal_precip,
     &  exf_inscal_runoff,  exf_inscal_apressure, exf_inscal_snowprecip,
     &  exf_inscal_runoftemp,
     &  exf_inscal_swdown,  exf_inscal_lwdown,
     & exf_inscal_climsst, exf_inscal_climsss,
     & exf_inscal_climustr, exf_inscal_climvstr,
     &  exf_outscal_hflux,  exf_outscal_ustress, exf_outscal_vstress,
     & exf_outscal_swflux,  exf_outscal_sst,     exf_outscal_sss,
     &  exf_outscal_sflux,  exf_outscal_apressure,
     & exf_inscal_areamask, exf_outscal_areamask,
     &  hfluxconst, atempconst, aqhconst, sfluxconst, evapconst,
     &  precipconst, snowprecipconst, runoffconst, ustressconst,
     &  runoftempconst,
     &  vstressconst, uwindconst, vwindconst, wspeedconst, swfluxconst,
     &  lwfluxconst, swdownconst, lwdownconst, apressureconst,
     &  areamaskconst, climsstconst,   climsssconst,
     &  climustrconst, climvstrconst,
     &     hflux_exfremo_intercept, hflux_exfremo_slope,
     &     atemp_exfremo_intercept, atemp_exfremo_slope,
     &     aqh_exfremo_intercept, aqh_exfremo_slope,
     &     sflux_exfremo_intercept, sflux_exfremo_slope,
     &     evap_exfremo_intercept, evap_exfremo_slope,
     &     precip_exfremo_intercept, precip_exfremo_slope,
     &     snowprecip_exfremo_intercept, snowprecip_exfremo_slope,
     &     runoff_exfremo_intercept, runoff_exfremo_slope,
     &     runoftemp_exfremo_intercept, runoftemp_exfremo_slope,
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
     &     areamask_exfremo_intercept, areamask_exfremo_slope,
     &     climsst_exfremo_intercept, climsst_exfremo_slope,
     &     climsss_exfremo_intercept, climsss_exfremo_slope,
     &     climustr_exfremo_intercept, climustr_exfremo_slope,
     &     climvstr_exfremo_intercept, climvstr_exfremo_slope

#ifdef USE_EXF_INTERPOLATION
      NAMELIST /EXF_NML_04/
     & ustress_lon0, ustress_lon_inc, ustress_lat0, ustress_lat_inc,
     & ustress_nlon, ustress_nlat, ustress_interpMethod,
     & vstress_lon0, vstress_lon_inc, vstress_lat0, vstress_lat_inc,
     & vstress_nlon, vstress_nlat, vstress_interpMethod,
     & hflux_lon0, hflux_lon_inc, hflux_lat0, hflux_lat_inc,
     & hflux_nlon, hflux_nlat, hflux_interpMethod,
     & sflux_lon0, sflux_lon_inc, sflux_lat0, sflux_lat_inc,
     & sflux_nlon, sflux_nlat, sflux_interpMethod,
     & swflux_lon0, swflux_lon_inc, swflux_lat0, swflux_lat_inc,
     & swflux_nlon, swflux_nlat, swflux_interpMethod,
     & lwflux_lon0, lwflux_lon_inc, lwflux_lat0, lwflux_lat_inc,
     & lwflux_nlon, lwflux_nlat, lwflux_interpMethod,
     & atemp_lon0, atemp_lon_inc, atemp_lat0, atemp_lat_inc,
     & atemp_nlon, atemp_nlat, atemp_interpMethod,
     & aqh_lon0, aqh_lon_inc, aqh_lat0,aqh_lat_inc,
     & aqh_nlon, aqh_nlat, aqh_interpMethod,
     & evap_lon0, evap_lon_inc, evap_lat0, evap_lat_inc,
     & evap_nlon, evap_nlat, evap_interpMethod,
     & precip_lon0, precip_lon_inc, precip_lat0, precip_lat_inc,
     & precip_nlon, precip_nlat, precip_interpMethod,
     & runoff_lon0, runoff_lon_inc, runoff_lat0, runoff_lat_inc,
     & runoff_nlon, runoff_nlat, runoff_interpMethod,
     & snowprecip_lon0, snowprecip_lon_inc,
     & snowprecip_lat0, snowprecip_lat_inc,
     & snowprecip_nlon, snowprecip_nlat, snowprecip_interpMethod,
     & uwind_lon0, uwind_lon_inc, uwind_lat0, uwind_lat_inc,
     & uwind_nlon, uwind_nlat, uwind_interpMethod,
     & vwind_lon0, vwind_lon_inc, vwind_lat0, vwind_lat_inc,
     & vwind_nlon, vwind_nlat, vwind_interpMethod,
     & wspeed_lon0, wspeed_lon_inc, wspeed_lat0, wspeed_lat_inc,
     & wspeed_nlon, wspeed_nlat, wspeed_interpMethod,
     & swdown_lon0, swdown_lon_inc, swdown_lat0, swdown_lat_inc,
     & swdown_nlon, swdown_nlat, swdown_interpMethod,
     & lwdown_lon0, lwdown_lon_inc, lwdown_lat0, lwdown_lat_inc,
     & lwdown_nlon, lwdown_nlat, lwdown_interpMethod,
     & apressure_lon0,apressure_lon_inc,apressure_lat0,apressure_lat_inc
     & , apressure_nlon, apressure_nlat, apressure_interpMethod,
     & areamask_lon0, areamask_lon_inc, areamask_lat0, areamask_lat_inc,
     & areamask_nlon, areamask_nlat, areamask_interpMethod,
     & climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
     & climsst_nlon, climsst_nlat, climsst_interpMethod,
     & climsss_lon0, climsss_lon_inc,climsss_lat0, climsss_lat_inc,
     & climsss_nlon, climsss_nlat, climsss_interpMethod,
     & climustr_lon0, climustr_lon_inc, climustr_lat0, climustr_lat_inc,
     & climustr_nlon, climustr_nlat, climustr_interpMethod,
     & climvstr_lon0, climvstr_lon_inc, climvstr_lat0, climvstr_lat_inc,
     & climvstr_nlon, climvstr_nlat, climvstr_interpMethod
#endif /* USE_EXF_INTERPOLATION */

#ifdef ALLOW_ICEFRONT
      NAMELIST /EXF_NML_SGRUNOFF/
     &    sgrunoffstartdate1, sgrunoffstartdate2,
     &    sgrunoffstartdate,  sgrunoffperiod,
     &    sgrunoffconst,      sgrunoff_exfremo_intercept,
     &    sgrunoff_exfremo_slope, exf_inscal_sgrunoff
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_OBCS
      NAMELIST /EXF_NML_OBCS/
     &    useOBCSYearlyFields,
     &    obcsNstartdate1,    obcsNstartdate2,   obcsNperiod,
     &    obcsSstartdate1,    obcsSstartdate2,   obcsSperiod,
     &    obcsEstartdate1,    obcsEstartdate2,   obcsEperiod,
     &    obcsWstartdate1,    obcsWstartdate2,   obcsWperiod,
     &    siobNstartdate1,    siobNstartdate2,   siobNperiod,
     &    siobSstartdate1,    siobSstartdate2,   siobSperiod,
     &    siobEstartdate1,    siobEstartdate2,   siobEperiod,
     &    siobWstartdate1,    siobWstartdate2,   siobWperiod
#endif /* ALLOW_OBCS */

#ifdef USE_EXF_INTERPOLATION
# ifdef ALLOW_EXCH2
      gridNx = exch2_mydNx(1)
      gridNy = exch2_mydNy(1)
# else /* ALLOW_EXCH2 */
      gridNx = Nx
      gridNy = Ny
# endif /* ALLOW_EXCH2 */
#endif /* USE_EXF_INTERPOLATION */

      IF ( .NOT.useEXF ) THEN
C-    pkg EXF is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.exf is found
         CALL PACKAGES_UNUSED_MSG( 'useEXF', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C     Set default values.

c     exf_verbose        = debugMode
      exf_verbose        = .FALSE.
      exf_debugLev       = debugLevel
      exf_monFreq        = monitorFreq
      useExfCheckRange   = .TRUE.
      useExfZenAlbedo    = .FALSE.
      select_ZenAlbedo   = 0
      useExfZenIncoming  = .FALSE.
      readStressOnAgrid  = .FALSE.
      readStressOnCgrid  = .FALSE.
#ifdef ALLOW_ATM_WIND
      useAtmWind         = .TRUE.
#else
      useAtmWind         = .FALSE.
#endif
      useRelativeWind    = .FALSE.
      noNegativeEvap     = .FALSE.

C-  default value should be set to main model parameter:
c     cen2kel     =  celsius2K
c     gravity_mks = gravity
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
      sstExtrapol    =        0.0    _d 0
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
C--   this default is chosen to be backward compatible with
C--   an earlier setting of 5.5 = ocean_emissivity*stefanBoltzmann
      ocean_emissivity = 5.50 _d -8 / 5.670 _d -8
      ice_emissivity   = 0.95 _d 0
      snow_emissivity  = 0.95 _d 0

C     Calendar data.
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

      runoftempconst              = 0.0 _d 0
      runoftemp_exfremo_intercept = 0.0 _d 0
      runoftemp_exfremo_slope     = 0.0 _d 0

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

      areamaskstartdate1    = 0
      areamaskstartdate2    = 0
      areamaskperiod        = 0.0 _d 0
      areamaskTauRelax      = 0.0 _d 0
      areamaskconst         = 0.0 _d 0
      areamask_exfremo_intercept = 0. _d 0
      areamask_exfremo_slope = 0. _d 0

      climsststartdate1  = 0
      climsststartdate2  = 0
      climsstperiod      = 0
      climsstTauRelax    = 0.0 _d 0
      climsstconst         = 0.0 _d 0
      climsst_exfremo_intercept = 0.0 _d 0
      climsst_exfremo_slope = 0.0 _d 0

      climsssstartdate1  = 0
      climsssstartdate2  = 0
      climsssperiod      = 0
      climsssTauRelax    = 0.0 _d 0
      climsssconst         = 0.0 _d 0
      climsss_exfremo_intercept = 0.0 _d 0
      climsss_exfremo_slope = 0.0 _d 0

      climustrstartdate1  = 0
      climustrstartdate2  = 0
      climustrperiod      = 0
      climustrTauRelax    = 0.0 _d 0
      climustrconst         = 0.0 _d 0
      climustr_exfremo_intercept = 0.0 _d 0
      climustr_exfremo_slope = 0.0 _d 0

      climvstrstartdate1  = 0
      climvstrstartdate2  = 0
      climvstrperiod      = 0
      climvstrTauRelax    = 0.0 _d 0
      climvstrconst         = 0.0 _d 0
      climvstr_exfremo_intercept = 0.0 _d 0
      climvstr_exfremo_slope = 0.0 _d 0

      sgrunoffstartdate1         = 0
      sgrunoffstartdate2         = 0
      sgrunoffstartdate          = 0.
      sgrunoffperiod             = 0.0 _d 0
      sgrunoffconst              = 0.0 _d 0
      sgrunoff_exfremo_intercept = 0.0 _d 0
      sgrunoff_exfremo_slope     = 0.0 _d 0
      exf_inscal_sgrunoff        = 1. _d 0

      useOBCSYearlyFields = .FALSE.
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

      siobNstartdate1    = UNSET_I
      siobNstartdate2    = UNSET_I
      siobNperiod        = UNSET_RL
      siobSstartdate1    = UNSET_I
      siobSstartdate2    = UNSET_I
      siobSperiod        = UNSET_RL
      siobEstartdate1    = UNSET_I
      siobEstartdate2    = UNSET_I
      siobEperiod        = UNSET_RL
      siobWstartdate1    = UNSET_I
      siobWstartdate2    = UNSET_I
      siobWperiod        = UNSET_RL

      repeatPeriod       = 0.0 _d 0
      windstressmax      = 2.0 _d 0

      exf_scal_BulkCdn   = 1.0  _d 0

C     Initialise freezing temperature of sea water
      climtempfreeze     = -1.9 _d 0

C     Data files.
      hfluxfile          = ' '
      atempfile          = ' '
      aqhfile            = ' '
      evapfile           = ' '
      precipfile         = ' '
      snowprecipfile     = ' '
      sfluxfile          = ' '
      runofffile         = ' '
      runoftempfile      = ' '
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
      areamaskfile       = ' '
      climsstfile        = ' '
      climsssfile        = ' '
      climustrfile       = ' '
      climvstrfile       = ' '

C     Start dates.
      hfluxstartdate     = 0.
      atempstartdate     = 0.
      aqhstartdate       = 0.
      evapstartdate      = 0.
      precipstartdate    = 0.
      snowprecipstartdate= 0.
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
      siobNstartdate     = 0.
      siobSstartdate     = 0.
      siobEstartdate     = 0.
      siobWstartdate     = 0.
      apressurestartdate = 0.
      areamaskstartdate  = 0.
      climsststartdate   = 0.
      climsssstartdate   = 0.
      climustrstartdate  = 0.
      climvstrstartdate  = 0.

C     Initialise file type and field precision
      exf_iprec            = 32
      exf_iprec_obcs       = UNSET_I
      exf_yftype           = 'RL'
      useExfYearlyFields   = .FALSE.
      twoDigitYear         = .FALSE.

C     Input scaling factors.
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
      exf_inscal_snowprecip=  1. _d 0
      exf_inscal_sst       =  1. _d 0
      exf_inscal_sss       =  1. _d 0
      exf_inscal_atemp     =  1. _d 0
      exf_offset_atemp     =  0. _d 0
      exf_inscal_aqh       =  1. _d 0
      exf_inscal_evap      =  1. _d 0
      exf_inscal_apressure =  1. _d 0
      exf_inscal_runoff    =  1. _d 0
      exf_inscal_runoftemp =  1. _d 0
      exf_inscal_swdown    =  1. _d 0
      exf_inscal_lwdown    =  1. _d 0
      exf_inscal_climsst   =  1. _d 0
      exf_inscal_climsss   =  1. _d 0
      exf_inscal_climustr  =  1. _d 0
      exf_inscal_climvstr  =  1. _d 0
      exf_inscal_areamask  =  1. _d 0

C     Output scaling factors.
      exf_outscal_hflux    =  1. _d 0
      exf_outscal_sflux    =  1. _d 0
      exf_outscal_ustress  =  1. _d 0
      exf_outscal_vstress  =  1. _d 0
      exf_outscal_swflux   =  1. _d 0
      exf_outscal_sst      =  1. _d 0
      exf_outscal_sss      =  1. _d 0
      exf_outscal_apressure=  1. _d 0
      exf_outscal_areamask =  1. _d 0

#ifdef USE_EXF_INTERPOLATION
C--   set default input location to match (in case of simple Lat-Lonp grid)
C     model grid cell-center position (leading to trivial interpolation)
      inp_lon0 = xgOrigin + delX(1)*exf_half
      inp_lat0 = ygOrigin + delY(1)*exf_half
      inp_dLon = delX(1)
      inp_dLat = delY(1)

      ustress_lon0   = inp_lon0
      uwind_lon0     = inp_lon0
      vstress_lon0   = inp_lon0
      hflux_lon0     = inp_lon0
      sflux_lon0     = inp_lon0
      swflux_lon0    = inp_lon0
      runoff_lon0    = inp_lon0
      atemp_lon0     = inp_lon0
      aqh_lon0       = inp_lon0
      evap_lon0      = inp_lon0
      precip_lon0    = inp_lon0
      snowprecip_lon0= inp_lon0
      vwind_lon0     = inp_lon0
      wspeed_lon0    = inp_lon0
      lwflux_lon0    = inp_lon0
      swdown_lon0    = inp_lon0
      lwdown_lon0    = inp_lon0
      apressure_lon0 = inp_lon0
      areamask_lon0  = inp_lon0
      vstress_lat0   = inp_lat0
      vwind_lat0     = inp_lat0
      wspeed_lat0    = inp_lat0
      ustress_lat0   = inp_lat0
      hflux_lat0     = inp_lat0
      sflux_lat0     = inp_lat0
      runoff_lat0    = inp_lat0
      swflux_lat0    = inp_lat0
      atemp_lat0     = inp_lat0
      aqh_lat0       = inp_lat0
      evap_lat0      = inp_lat0
      precip_lat0    = inp_lat0
      snowprecip_lat0= inp_lat0
      uwind_lat0     = inp_lat0
      lwflux_lat0    = inp_lat0
      swdown_lat0    = inp_lat0
      lwdown_lat0    = inp_lat0
      apressure_lat0 = inp_lat0
      areamask_lat0  = inp_lat0
      ustress_nlon   = gridNx
      ustress_nlat   = gridNy
      vstress_nlon   = gridNx
      vstress_nlat   = gridNy
      hflux_nlon     = gridNx
      hflux_nlat     = gridNy
      sflux_nlon     = gridNx
      sflux_nlat     = gridNy
      swflux_nlon    = gridNx
      swflux_nlat    = gridNy
      runoff_nlon    = gridNx
      runoff_nlat    = gridNy
      atemp_nlon     = gridNx
      atemp_nlat     = gridNy
      aqh_nlon       = gridNx
      aqh_nlat       = gridNy
      evap_nlon      = gridNx
      evap_nlat      = gridNy
      precip_nlon    = gridNx
      snowprecip_nlon= gridNx
      precip_nlat    = gridNy
      snowprecip_nlat= gridNy
      uwind_nlon     = gridNx
      uwind_nlat     = gridNy
      vwind_nlon     = gridNx
      vwind_nlat     = gridNy
      wspeed_nlon    = gridNx
      wspeed_nlat    = gridNy
      lwflux_nlon    = gridNx
      lwflux_nlat    = gridNy
      swdown_nlon    = gridNx
      swdown_nlat    = gridNy
      lwdown_nlon    = gridNx
      lwdown_nlat    = gridNy
      apressure_nlon = gridNx
      apressure_nlat = gridNy
      areamask_nlon  = gridNx
      areamask_nlat  = gridNy
      ustress_lon_inc   = inp_dLon
      vstress_lon_inc   = inp_dLon
      hflux_lon_inc     = inp_dLon
      sflux_lon_inc     = inp_dLon
      swflux_lon_inc    = inp_dLon
      runoff_lon_inc    = inp_dLon
      atemp_lon_inc     = inp_dLon
      aqh_lon_inc       = inp_dLon
      evap_lon_inc      = inp_dLon
      precip_lon_inc    = inp_dLon
      snowprecip_lon_inc= inp_dLon
      uwind_lon_inc     = inp_dLon
      vwind_lon_inc     = inp_dLon
      wspeed_lon_inc    = inp_dLon
      lwflux_lon_inc    = inp_dLon
      swdown_lon_inc    = inp_dLon
      lwdown_lon_inc    = inp_dLon
      apressure_lon_inc = inp_dLon
      areamask_lon_inc  = inp_dLon

      climsst_lon0    = inp_lon0
      climsss_lon0    = inp_lon0
      climustr_lon0   = inp_lon0
      climvstr_lon0   = inp_lon0
      climsst_lat0    = inp_lat0
      climsss_lat0    = inp_lat0
      climustr_lat0   = inp_lat0
      climvstr_lat0   = inp_lat0
      climsst_nlon    = gridNx
      climsst_nlat    = gridNy
      climsss_nlon    = gridNx
      climsss_nlat    = gridNy
      climustr_nlon   = gridNx
      climustr_nlat   = gridNy
      climvstr_nlon   = gridNx
      climvstr_nlat   = gridNy
      climsst_lon_inc = inp_dLon
      climsss_lon_inc = inp_dLon
      climustr_lon_inc= inp_dLon
      climvstr_lon_inc= inp_dLon

      DO j=1,MAX_LAT_INC
        IF (j.LT.gridNy) THEN
          inp_dLat = (delY(j) + delY(j+1))*exf_half
        ELSE
          inp_dLat = 0.
        ENDIF
        ustress_lat_inc(j)   = inp_dLat
        vstress_lat_inc(j)   = inp_dLat
        hflux_lat_inc(j)     = inp_dLat
        sflux_lat_inc(j)     = inp_dLat
        swflux_lat_inc(j)    = inp_dLat
        runoff_lat_inc(j)    = inp_dLat
        atemp_lat_inc(j)     = inp_dLat
        aqh_lat_inc(j)       = inp_dLat
        evap_lat_inc(j)      = inp_dLat
        precip_lat_inc(j)    = inp_dLat
        snowprecip_lat_inc(j)= inp_dLat
        uwind_lat_inc(j)     = inp_dLat
        vwind_lat_inc(j)     = inp_dLat
        wspeed_lat_inc(j)    = inp_dLat
        lwflux_lat_inc(j)    = inp_dLat
        swdown_lat_inc(j)    = inp_dLat
        lwdown_lat_inc(j)    = inp_dLat
        apressure_lat_inc(j) = inp_dLat
        areamask_lat_inc(j)  = inp_dLat
        climsst_lat_inc(j)   = inp_dLat
        climsss_lat_inc(j)   = inp_dLat
        climustr_lat_inc(j)  = inp_dLat
        climvstr_lat_inc(j)  = inp_dLat
      ENDDO

      ustress_interpMethod   = 12
      vstress_interpMethod   = 22
      hflux_interpMethod     =  1
      sflux_interpMethod     =  1
      swflux_interpMethod    =  1
      runoff_interpMethod    =  1
      atemp_interpMethod     =  1
      aqh_interpMethod       =  1
      evap_interpMethod      =  1
      precip_interpMethod    =  1
      snowprecip_interpMethod=  1
      uwind_interpMethod     = 12
      vwind_interpMethod     = 22
      wspeed_interpMethod    =  1
      lwflux_interpMethod    =  1
      swdown_interpMethod    =  1
      lwdown_interpMethod    =  1
      apressure_interpMethod =  1
      areamask_interpMethod  =  1
      climsst_interpMethod   =  2
      climsss_interpMethod   =  2
      climustr_interpMethod  = 12
      climvstr_interpMethod  = 22

#endif /* USE_EXF_INTERPOLATION */

C     Next, read the forcing data file.
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
#ifdef USE_EXF_INTERPOLATION
      WRITE(msgBuf,'(A)')
     &     'EXF_READPARMS: reading EXF_NML_04'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      READ(  iUnit, nml = EXF_NML_04 )
#endif /* USE_EXF_INTERPOLATION */

#ifdef ALLOW_ICEFRONT
      IF ( useIcefront ) THEN
       WRITE(msgBuf,'(A)')
     &      'EXF_READPARMS: reading EXF_NML_SGRUNOFF'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , 1)
       READ(  iUnit, nml = EXF_NML_SGRUNOFF )
      ENDIF
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_OBCS
      IF ( useOBCS ) THEN
       WRITE(msgBuf,'(A)')
     &      'EXF_READPARMS: reading EXF_NML_OBCS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , 1)
       READ(  iUnit, nml = EXF_NML_OBCS )
      ENDIF
       IF(siobNstartdate1.EQ.UNSET_I ) siobNstartdate1=obcsNstartdate1
       IF(siobNstartdate2.EQ.UNSET_I ) siobNstartdate2=obcsNstartdate2
       IF(siobNperiod    .EQ.UNSET_RL) siobNperiod    =obcsNperiod
       IF(siobSstartdate1.EQ.UNSET_I ) siobSstartdate1=obcsSstartdate1
       IF(siobSstartdate2.EQ.UNSET_I ) siobSstartdate2=obcsSstartdate2
       IF(siobSperiod    .EQ.UNSET_RL) siobSperiod    =obcsSperiod
       IF(siobEstartdate1.EQ.UNSET_I ) siobEstartdate1=obcsEstartdate1
       IF(siobEstartdate2.EQ.UNSET_I ) siobEstartdate2=obcsEstartdate2
       IF(siobEperiod    .EQ.UNSET_RL) siobEperiod    =obcsEperiod
       IF(siobWstartdate1.EQ.UNSET_I ) siobWstartdate1=obcsWstartdate1
       IF(siobWstartdate2.EQ.UNSET_I ) siobWstartdate2=obcsWstartdate2
       IF(siobWperiod    .EQ.UNSET_RL) siobWperiod    =obcsWperiod

       IF(exf_iprec_obcs .EQ. UNSET_I) exf_iprec_obcs =exf_iprec
#endif /* ALLOW_OBCS */

      WRITE(msgBuf,'(A)')
     &     'EXF_READPARMS: finished reading data.exf'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                SQUEEZE_RIGHT , 1)

      CLOSE( iUnit )

C--   Retired parameters
      IF ( exf_yftype.NE.'RL' ) THEN
       STOP 'S/R EXF_READPARAMS: value of exf_yftype not allowed'
      ENDIF

C--   Derive other parameters:
      IF ( exf_verbose ) exf_debugLev = MAX( exf_debugLev, debLevD )
      hq = ht
      stressIsOnCgrid = readStressOnCgrid
#if ( defined (ALLOW_BULKFORMULAE) )
      IF ( useAtmWind ) stressIsOnCgrid = .FALSE.
#endif
#ifdef USE_EXF_INTERPOLATION
      IF ( (ustress_interpMethod.GE.1 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.GE.1 .AND. vstressfile.NE.' ') )
     &   stressIsOnCgrid = .FALSE.
#endif /* USE_EXF_INTERPOLATION */

      IF ( select_ZenAlbedo.GT.0 ) THEN
           useExfZenAlbedo=.TRUE.
      ENDIf

C--   Overwrite tauThetaClimRelax but stop if already set.
C- Note: need this, even if EXF option ALLOW_CLIMSST_RELAXATION is undef;
C        this prevents to apply relaxation towards potentially wrong SST since,
C        with EXF, we skip the update of loaded SST in EXTERNAL_FIELDS_LOAD.
C- Note2: let s see whether we can put this back under ifdef
C        ALLOW_CLIMSST_RELAXATION, but always call EXTERNAL_FIELDS_LOAD.
C        If ALLOW_CLIMSST_RELAXATION is undef, clim.relaxation could still
C        be done outside of exf.
#ifdef ALLOW_CLIMSST_RELAXATION
      IF ( tauThetaClimRelax.NE.0. _d 0 ) THEN
        WRITE(msgBuf,'(2A)') 'EXF_READPARMS: ',
     &   'with EXF, cannot use "tauThetaClimRelax" in "data"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'since SST relax. is handled by EXF',
     &   ' (data.exf, "climsstTauRelax")'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_READPARMS'
      ENDIF
      tauThetaClimRelax = climsstTauRelax
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
C--   Overwrite tauSaltClimRelax but stop if already set.
      IF ( tauSaltClimRelax.NE.0. _d 0 ) THEN
        WRITE(msgBuf,'(2A)') 'EXF_READPARMS: ',
     &   'with EXF, cannot use "tauSaltClimRelax" in "data"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'since SSS relax. is handled by EXF',
     &   ' (data.exf, "climsssTauRelax")'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_READPARMS'
      ENDIF
      tauSaltClimRelax = climsssTauRelax
#endif

C     Complete the start date specifications for the forcing
C     fields to get a complete calendar date array.
C     => moved to EXF_INIT_FIXED

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
