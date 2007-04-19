C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_PARAM.h,v 1.3 2007/04/19 14:37:00 heimbach Exp $
C $Name:  $
c
c
c     ==================================================================
c     HEADER exf_param
c     ==================================================================
c
c     o Header file for the surface flux data. Used by the external
c       forcing package.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c              - Restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed some variables names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - included exf_iprec, exf_yftype to enable easy
c                switch between 32bit/64 bit data format
c
c              Patrick Heimbach, heimbach@mit.edu  01-May-2001
c              - added obcs parameters
c
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     HEADER exf_param
c     ==================================================================

c     year in seconds
      _RL     year2sec

c     Calendar data.
      _RL     repeatPeriod

c     Monitor Frequency (s)
      _RL     exf_monFreq

c     Sea-water albedo
      _RL     exf_albedo
c     longwave surface emissivities (ice and snow emissivities are used
c     in conjunction with thsice/seaice)
      _RL     ocean_emissivity
      _RL     ice_emissivity
      _RL     snow_emissivity

c     Drag coefficient scaling factor
      _RL     exf_scal_BulkCdn

c     Maximum absolute windstress, used to reset unreastically high
c     data values
      _RL     windstressmax

      integer hfluxstartdate1
      integer hfluxstartdate2
      _RL     hfluxstartdate
      _RL     hfluxperiod
      _RL     hfluxconst
      _RL     hflux_exfremo_intercept 
      _RL     hflux_exfremo_slope
      character*1 hfluxmask
      parameter(  hfluxmask = 's' )

      integer atempstartdate1
      integer atempstartdate2
      _RL     atempstartdate
      _RL     atempperiod
      _RL     atempconst
      _RL     atemp_exfremo_intercept 
      _RL     atemp_exfremo_slope
      character*1 atempmask
      parameter(  atempmask = 's' )

      integer aqhstartdate1
      integer aqhstartdate2
      _RL     aqhstartdate
      _RL     aqhperiod
      _RL     aqhconst
      _RL     aqh_exfremo_intercept 
      _RL     aqh_exfremo_slope
      character*1 aqhmask
      parameter(  aqhmask = 's' )

      integer sfluxstartdate1
      integer sfluxstartdate2
      _RL     sfluxstartdate
      _RL     sfluxperiod
      _RL     sfluxconst
      _RL     sflux_exfremo_intercept 
      _RL     sflux_exfremo_slope
      character*1 sfluxmask
      parameter(  sfluxmask = 's' )

      integer evapstartdate1
      integer evapstartdate2
      _RL     evapstartdate
      _RL     evapperiod
      _RL     evapconst
      _RL     evap_exfremo_intercept 
      _RL     evap_exfremo_slope
      character*1 evapmask
      parameter(  evapmask = 's' )

      integer precipstartdate1
      integer precipstartdate2
      _RL     precipstartdate
      _RL     precipperiod
      _RL     precipconst
      _RL     precip_exfremo_intercept 
      _RL     precip_exfremo_slope
      character*1 precipmask
      parameter(  precipmask = 's' )

      integer snowprecipstartdate1
      integer snowprecipstartdate2
      _RL     snowprecipstartdate
      _RL     snowprecipperiod
      _RL     snowprecipconst
      _RL     snowprecip_exfremo_intercept 
      _RL     snowprecip_exfremo_slope
      character*1 snowprecipmask
      parameter(  snowprecipmask = 's' )

      integer runoffstartdate1
      integer runoffstartdate2
      _RL     runoffstartdate
      _RL     runoffperiod
      _RL     runoffconst
      _RL     runoff_exfremo_intercept 
      _RL     runoff_exfremo_slope
      character*1 runoffmask
      parameter(  runoffmask = 's' )

      integer ustressstartdate1
      integer ustressstartdate2
      _RL     ustressstartdate
      _RL     ustressperiod
      _RL     ustressconst
      _RL     ustress_exfremo_intercept 
      _RL     ustress_exfremo_slope
      character*1 ustressmask
      parameter(  ustressmask = 'u' )

      integer vstressstartdate1
      integer vstressstartdate2
      _RL     vstressstartdate
      _RL     vstressperiod
      _RL     vstressconst
      _RL     vstress_exfremo_intercept 
      _RL     vstress_exfremo_slope
      character*1 vstressmask
      parameter(  vstressmask = 'v' )

      integer uwindstartdate1
      integer uwindstartdate2
      _RL     uwindstartdate
      _RL     uwindperiod
      _RL     uwindconst
      _RL     uwind_exfremo_intercept 
      _RL     uwind_exfremo_slope
      character*1 uwindmask
      parameter(  uwindmask = 's' )

      integer vwindstartdate1
      integer vwindstartdate2
      _RL     vwindstartdate
      _RL     vwindperiod
      _RL     vwindconst
      _RL     vwind_exfremo_intercept 
      _RL     vwind_exfremo_slope
      character*1 vwindmask
      parameter(  vwindmask = 's' )

      integer wspeedstartdate1
      integer wspeedstartdate2
      _RL     wspeedstartdate
      _RL     wspeedperiod
      _RL     wspeedconst
      _RL     wspeed_exfremo_intercept 
      _RL     wspeed_exfremo_slope
      character*1 wspeedmask
      parameter(  wspeedmask = 's' )

      integer swfluxstartdate1
      integer swfluxstartdate2
      _RL     swfluxstartdate
      _RL     swfluxperiod
      _RL     swfluxconst
      _RL     swflux_exfremo_intercept 
      _RL     swflux_exfremo_slope
      character*1 swfluxmask
      parameter(  swfluxmask = 's' )

      integer lwfluxstartdate1
      integer lwfluxstartdate2
      _RL     lwfluxstartdate
      _RL     lwfluxperiod
      _RL     lwfluxconst
      _RL     lwflux_exfremo_intercept 
      _RL     lwflux_exfremo_slope
      character*1 lwfluxmask
      parameter(  lwfluxmask = 's' )

      integer swdownstartdate1
      integer swdownstartdate2
      _RL     swdownstartdate
      _RL     swdownperiod
      _RL     swdownconst
      _RL     swdown_exfremo_intercept 
      _RL     swdown_exfremo_slope
      character*1 swdownmask
      parameter(  swdownmask = 's' )

      integer lwdownstartdate1
      integer lwdownstartdate2
      _RL     lwdownstartdate
      _RL     lwdownperiod
      _RL     lwdownconst
      _RL     lwdown_exfremo_intercept 
      _RL     lwdown_exfremo_slope
      character*1 lwdownmask
      parameter(  lwdownmask = 's' )

      integer apressurestartdate1
      integer apressurestartdate2
      _RL     apressurestartdate
      _RL     apressureperiod
      _RL     apressureconst
      _RL     apressure_exfremo_intercept 
      _RL     apressure_exfremo_slope
      character*1 apressuremask
      parameter(  apressuremask = 's' )

c     Calendar data.
      integer climsststartdate1
      integer climsststartdate2
      _RL     climsststartdate
      _RL     climsstperiod
      _RL     climsstconst
      _RL     climsst_exfremo_intercept 
      _RL     climsst_exfremo_slope
      character*1 climsstmask
      parameter(  climsstmask = 's' )

      integer climsssstartdate1
      integer climsssstartdate2
      _RL     climsssstartdate
      _RL     climsssperiod
      _RL     climsssconst
      _RL     climsss_exfremo_intercept 
      _RL     climsss_exfremo_slope
      character*1 climsssmask
      parameter(  climsssmask = 's' )

c     freezing temperature is the minimum temperature allowed, used
c     to reset climatological temperatures fields where they have
c     values below climtempfreeze
      _RL climtempfreeze

      integer selectStressGridPosition

      integer obcsNstartdate1
      integer obcsNstartdate2
      _RL     obcsNstartdate
      _RL     obcsNperiod

      integer obcsSstartdate1
      integer obcsSstartdate2
      _RL     obcsSstartdate
      _RL     obcsSperiod

      integer obcsEstartdate1
      integer obcsEstartdate2
      _RL     obcsEstartdate
      _RL     obcsEperiod

      integer obcsWstartdate1
      integer obcsWstartdate2
      _RL     obcsWstartdate
      _RL     obcsWperiod

c     File names.
      character*(128) hfluxfile
      character*(128) atempfile
      character*(128) aqhfile
      character*(128) evapfile
      character*(128) precipfile
      character*(128) snowprecipfile
      character*(128) sfluxfile
      character*(128) runofffile
      character*(128) ustressfile
      character*(128) vstressfile
      character*(128) uwindfile
      character*(128) vwindfile
      character*(128) wspeedfile
      character*(128) swfluxfile
      character*(128) lwfluxfile
      character*(128) swdownfile
      character*(128) lwdownfile
      character*(128) apressurefile
      character*(128) climsstfile
      character*(128) climsssfile

C     useExfYearlyFields :: when set, automatically add extension
C                           _YEAR to input file names
C     twoDigitYear       :: when set, use 2-digit year extension YR
C                           instead of _YEAR for useExfYearlyFields
      logical useExfYearlyFields, twoDigitYear
      logical useExfCheckRange

      common /exf_param_l/
     &                     useExfYearlyFields, twoDigitYear,
     &                     useExfCheckRange
      common /exf_param_i/ selectStressGridPosition,
     &                     hfluxstartdate1,   hfluxstartdate2,
     &                     atempstartdate1,   atempstartdate2,
     &                     aqhstartdate1,     aqhstartdate2,
     &                     sfluxstartdate1,   sfluxstartdate2,
     &                     evapstartdate1,    evapstartdate2,
     &                     runoffstartdate1,  runoffstartdate2,
     &                     precipstartdate1,  precipstartdate2,
     &                     snowprecipstartdate1, snowprecipstartdate2,
     &                     ustressstartdate1, ustressstartdate2,
     &                     vstressstartdate1, vstressstartdate2,
     &                     uwindstartdate1,   uwindstartdate2,
     &                     vwindstartdate1,   vwindstartdate2,
     &                     wspeedstartdate1,  wspeedstartdate2,
     &                     swfluxstartdate1,  swfluxstartdate2,
     &                     lwfluxstartdate1,  lwfluxstartdate2,
     &                     swdownstartdate1,  swdownstartdate2,
     &                     lwdownstartdate1,  lwdownstartdate2,
     &                     obcsNstartdate1,   obcsNstartdate2,
     &                     obcsSstartdate1,   obcsSstartdate2,
     &                     obcsEstartdate1,   obcsEstartdate2,
     &                     obcsWstartdate1,   obcsWstartdate2,
     &                     apressurestartdate1,apressurestartdate2

      common /exf_param_r/
     &                     year2sec,          windstressmax,
     &                     repeatPeriod,      exf_monFreq,
     &                     exf_albedo,        ocean_emissivity,
     &                     ice_emissivity,    snow_emissivity,
     &                     exf_scal_BulkCdn,
     &                     hfluxperiod,       hfluxstartdate,
     &                     atempperiod,       atempstartdate,
     &                     aqhperiod,         aqhstartdate,
     &                     sfluxperiod,       sfluxstartdate,
     &                     evapperiod,        evapstartdate,
     &                     precipperiod,      precipstartdate,
     &                     snowprecipperiod,  snowprecipstartdate,
     &                     runoffperiod,      runoffstartdate,
     &                     ustressperiod,     ustressstartdate,
     &                     vstressperiod,     vstressstartdate,
     &                     uwindperiod,       uwindstartdate,
     &                     vwindperiod,       vwindstartdate,
     &                     wspeedperiod,      wspeedstartdate,
     &                     swfluxperiod,      swfluxstartdate,
     &                     lwfluxperiod,      lwfluxstartdate,
     &                     swdownperiod,      swdownstartdate,
     &                     lwdownperiod,      lwdownstartdate,
     &                     obcsNperiod,       obcsNstartdate,
     &                     obcsSperiod,       obcsSstartdate,
     &                     obcsEperiod,       obcsEstartdate,
     &                     obcsWperiod,       obcsWstartdate,
     &                     apressureperiod,   apressurestartdate,
     &                     hfluxconst,
     &                     atempconst,
     &                     aqhconst,
     &                     sfluxconst,
     &                     evapconst,
     &                     precipconst,
     &                     snowprecipconst,
     &                     runoffconst,
     &                     ustressconst,
     &                     vstressconst,
     &                     uwindconst,
     &                     vwindconst,
     &                     wspeedconst,
     &                     swfluxconst,
     &                     lwfluxconst,
     &                     swdownconst,
     &                     lwdownconst,
     &                     apressureconst

      common /exf_param_trend_removal/
     &                     hflux_exfremo_intercept,
     &                     atemp_exfremo_intercept,
     &                     aqh_exfremo_intercept,
     &                     sflux_exfremo_intercept,
     &                     evap_exfremo_intercept,
     &                     precip_exfremo_intercept,
     &                     snowprecip_exfremo_intercept,
     &                     runoff_exfremo_intercept,
     &                     ustress_exfremo_intercept,
     &                     vstress_exfremo_intercept,
     &                     uwind_exfremo_intercept,
     &                     vwind_exfremo_intercept,
     &                     wspeed_exfremo_intercept,
     &                     swflux_exfremo_intercept,
     &                     lwflux_exfremo_intercept,
     &                     swdown_exfremo_intercept,
     &                     lwdown_exfremo_intercept,
     &                     apressure_exfremo_intercept,
     &                     hflux_exfremo_slope,
     &                     atemp_exfremo_slope,
     &                     aqh_exfremo_slope,
     &                     sflux_exfremo_slope,
     &                     evap_exfremo_slope,
     &                     precip_exfremo_slope,
     &                     snowprecip_exfremo_slope,
     &                     runoff_exfremo_slope,
     &                     ustress_exfremo_slope,
     &                     vstress_exfremo_slope,
     &                     uwind_exfremo_slope,
     &                     vwind_exfremo_slope,
     &                     wspeed_exfremo_slope,
     &                     swflux_exfremo_slope,
     &                     lwflux_exfremo_slope,
     &                     swdown_exfremo_slope,
     &                     lwdown_exfremo_slope,
     &                     apressure_exfremo_slope

      common /exf_param_c/
     &                     hfluxfile,
     &                     atempfile,
     &                     aqhfile,
     &                     sfluxfile,
     &                     evapfile,
     &                     precipfile,
     &                     snowprecipfile,
     &                     runofffile,
     &                     ustressfile,
     &                     vstressfile,
     &                     uwindfile,
     &                     vwindfile,
     &                     wspeedfile,
     &                     swfluxfile,
     &                     lwfluxfile,
     &                     swdownfile,
     &                     lwdownfile,
     &                     apressurefile

      common /exf_clim_i/
     &                        climsststartdate1,  climsststartdate2,
     &                        climsssstartdate1,  climsssstartdate2

      common /exf_clim_c/
     &                        climsstfile,
     &                        climsssfile

      common /exf_clim_r/
     &                        climtempfreeze,
     &                        climsstperiod,      climsststartdate,
     &                        climsssperiod,      climsssstartdate,
     &                        climsstconst,       climsssconst,
     &     climsst_exfremo_intercept, climsst_exfremo_slope,
     &     climsss_exfremo_intercept, climsss_exfremo_slope,
     &     exf_inscal_climsst, exf_inscal_climsss

c     file precision and field type

      common /exf_param_type/ 
     &                     exf_iprec,
     &                     exf_yftype

      integer exf_iprec
      character*(2) exf_yftype

c     exf_inscal_*      input scaling factors
c     exf_offset_atemp  input air temperature offset
c                       (for conversion from C to K, if needed)
c     exf_outscale_*    output scaling factors

      _RL     exf_inscal_hflux
      _RL     exf_inscal_sflux
      _RL     exf_inscal_ustress
      _RL     exf_inscal_vstress
      _RL     exf_inscal_uwind
      _RL     exf_inscal_vwind
      _RL     exf_inscal_wspeed
      _RL     exf_inscal_swflux
      _RL     exf_inscal_lwflux
      _RL     exf_inscal_precip
      _RL     exf_inscal_snowprecip
      _RL     exf_inscal_sst
      _RL     exf_inscal_sss
      _RL     exf_inscal_atemp
      _RL     exf_offset_atemp
      _RL     exf_inscal_aqh
      _RL     exf_inscal_evap
      _RL     exf_inscal_apressure
      _RL     exf_inscal_runoff
      _RL     exf_inscal_swdown
      _RL     exf_inscal_lwdown
      _RL     exf_inscal_climsst
      _RL     exf_inscal_climsss

      _RL     exf_outscal_hflux
      _RL     exf_outscal_sflux
      _RL     exf_outscal_ustress
      _RL     exf_outscal_vstress
      _RL     exf_outscal_swflux
      _RL     exf_outscal_sst
      _RL     exf_outscal_sss
      _RL     exf_outscal_apressure

      common /exf_param_scal/
     &                      exf_inscal_hflux
     &                    , exf_inscal_sflux
     &                    , exf_inscal_ustress
     &                    , exf_inscal_vstress
     &                    , exf_inscal_uwind
     &                    , exf_inscal_vwind
     &                    , exf_inscal_wspeed
     &                    , exf_inscal_swflux
     &                    , exf_inscal_lwflux
     &                    , exf_inscal_precip
     &                    , exf_inscal_snowprecip
     &                    , exf_inscal_sst
     &                    , exf_inscal_sss
     &                    , exf_inscal_atemp
     &                    , exf_offset_atemp
     &                    , exf_inscal_aqh
     &                    , exf_inscal_evap
     &                    , exf_inscal_apressure
     &                    , exf_inscal_runoff
     &                    , exf_inscal_swdown
     &                    , exf_inscal_lwdown
     &                    , exf_outscal_hflux
     &                    , exf_outscal_sflux
     &                    , exf_outscal_ustress
     &                    , exf_outscal_vstress
     &                    , exf_outscal_swflux
     &                    , exf_outscal_sst
     &                    , exf_outscal_sss
     &                    , exf_outscal_apressure

#ifndef USE_EXF_INTERPOLATION
c-- set dummy dimension 1
       integer MAX_LAT_INC
       parameter(MAX_LAT_INC = 1)
#else
c for lat interpolation, arraysize currently set to 2176 max data values
       integer MAX_LAT_INC
       parameter(MAX_LAT_INC = 2176)
      _RL ustress_lon0, ustress_lon_inc
      _RL ustress_lat0, ustress_lat_inc(MAX_LAT_INC)
      INTEGER ustress_nlon, ustress_nlat
      _RL vstress_lon0, vstress_lon_inc
      _RL vstress_lat0, vstress_lat_inc(MAX_LAT_INC)
      INTEGER vstress_nlon, vstress_nlat
      _RL hflux_lon0, hflux_lon_inc
      _RL hflux_lat0, hflux_lat_inc(MAX_LAT_INC)
      INTEGER hflux_nlon, hflux_nlat
      _RL sflux_lon0, sflux_lon_inc
      _RL sflux_lat0, sflux_lat_inc(MAX_LAT_INC)
      INTEGER sflux_nlon, sflux_nlat
      _RL swflux_lon0, swflux_lon_inc
      _RL swflux_lat0, swflux_lat_inc(MAX_LAT_INC)
      INTEGER swflux_nlon, swflux_nlat
      _RL runoff_lon0, runoff_lon_inc
      _RL runoff_lat0, runoff_lat_inc(MAX_LAT_INC)
      INTEGER runoff_nlon, runoff_nlat
      _RL atemp_lon0, atemp_lon_inc
      _RL atemp_lat0, atemp_lat_inc(MAX_LAT_INC)
      INTEGER atemp_nlon, atemp_nlat
      _RL aqh_lon0, aqh_lon_inc
      _RL aqh_lat0, aqh_lat_inc(MAX_LAT_INC)
      INTEGER aqh_nlon, aqh_nlat
      _RL evap_lon0, evap_lon_inc
      _RL evap_lat0, evap_lat_inc(MAX_LAT_INC)
      INTEGER evap_nlon, evap_nlat
      _RL precip_lon0, precip_lon_inc
      _RL precip_lat0, precip_lat_inc(MAX_LAT_INC)
      INTEGER precip_nlon, precip_nlat
      _RL snowprecip_lon0, snowprecip_lon_inc
      _RL snowprecip_lat0, snowprecip_lat_inc(MAX_LAT_INC)
      INTEGER snowprecip_nlon, snowprecip_nlat
      _RL uwind_lon0, uwind_lon_inc
      _RL uwind_lat0, uwind_lat_inc(MAX_LAT_INC)
      INTEGER uwind_nlon, uwind_nlat
      _RL vwind_lon0, vwind_lon_inc
      _RL vwind_lat0, vwind_lat_inc(MAX_LAT_INC)
      INTEGER vwind_nlon, vwind_nlat
      _RL wspeed_lon0, wspeed_lon_inc
      _RL wspeed_lat0, wspeed_lat_inc(MAX_LAT_INC)
      INTEGER wspeed_nlon, wspeed_nlat
      _RL lwflux_lon0, lwflux_lon_inc
      _RL lwflux_lat0, lwflux_lat_inc(MAX_LAT_INC)
      INTEGER lwflux_nlon, lwflux_nlat
      _RL swdown_lon0, swdown_lon_inc
      _RL swdown_lat0, swdown_lat_inc(MAX_LAT_INC)
      INTEGER swdown_nlon, swdown_nlat
      _RL lwdown_lon0, lwdown_lon_inc
      _RL lwdown_lat0, lwdown_lat_inc(MAX_LAT_INC)
      INTEGER lwdown_nlon, lwdown_nlat
      _RL apressure_lon0,apressure_lon_inc
      _RL apressure_lat0,apressure_lat_inc(MAX_LAT_INC)
      INTEGER apressure_nlon,apressure_nlat

      common /exf_interpolation/
     & ustress_lon0, ustress_lon_inc,
     & ustress_lat0, ustress_lat_inc,
     & ustress_nlon, ustress_nlat,
     & vstress_lon0, vstress_lon_inc,
     & vstress_lat0, vstress_lat_inc,
     & vstress_nlon, vstress_nlat,
     & hflux_lon0, hflux_lon_inc,
     & hflux_lat0, hflux_lat_inc,
     & hflux_nlon, hflux_nlat,
     & sflux_lon0, sflux_lon_inc,
     & sflux_lat0, sflux_lat_inc,
     & sflux_nlon, sflux_nlat,
     & swflux_lon0, swflux_lon_inc,
     & swflux_lat0, swflux_lat_inc,
     & swflux_nlon, swflux_nlat,
     & runoff_lon0, runoff_lon_inc,
     & runoff_lat0, runoff_lat_inc,
     & runoff_nlon, runoff_nlat,
     & atemp_lon0, atemp_lon_inc,
     & atemp_lat0, atemp_lat_inc,
     & atemp_nlon, atemp_nlat,
     & aqh_lon0, aqh_lon_inc,
     & aqh_lat0, aqh_lat_inc,
     & aqh_nlon, aqh_nlat,
     & evap_lon0, evap_lon_inc,
     & evap_lat0, evap_lat_inc,
     & evap_nlon, evap_nlat,
     & precip_lon0, precip_lon_inc,
     & precip_lat0, precip_lat_inc,
     & precip_nlon, precip_nlat,
     & snowprecip_lon0, snowprecip_lon_inc,
     & snowprecip_lat0, snowprecip_lat_inc,
     & snowprecip_nlon, snowprecip_nlat,
     & uwind_lon0, uwind_lon_inc,
     & uwind_lat0, uwind_lat_inc,
     & uwind_nlon, uwind_nlat,
     & vwind_lon0, vwind_lon_inc,
     & vwind_lat0, vwind_lat_inc,
     & vwind_nlon, vwind_nlat,
     & wspeed_lon0, wspeed_lon_inc,
     & wspeed_lat0, wspeed_lat_inc,
     & wspeed_nlon, wspeed_nlat,
     & lwflux_lon0, lwflux_lon_inc,
     & lwflux_lat0, lwflux_lat_inc,
     & lwflux_nlon, lwflux_nlat,
     & swdown_lon0, swdown_lon_inc,
     & swdown_lat0, swdown_lat_inc,
     & swdown_nlon, swdown_nlat,
     & lwdown_lon0, lwdown_lon_inc,
     & lwdown_lat0, lwdown_lat_inc,
     & lwdown_nlon, lwdown_nlat,
     & apressure_lon0,apressure_lon_inc,
     & apressure_lat0,apressure_lat_inc,
     & apressure_nlon,apressure_nlat

      _RL climsst_lon0, climsst_lon_inc
      _RL climsst_lat0, climsst_lat_inc(MAX_LAT_INC)
      INTEGER climsst_nlon, climsst_nlat
      _RL climsss_lon0, climsss_lon_inc
      _RL climsss_lat0, climsss_lat_inc(MAX_LAT_INC)
      INTEGER climsss_nlon, climsss_nlat
      common /exf_clim_interpolation/
     & climsst_lon0, climsst_lon_inc,
     & climsst_lat0, climsst_lat_inc,
     & climsst_nlon, climsst_nlat,
     & climsss_lon0, climsss_lon_inc,
     & climsss_lat0, climsss_lat_inc,
     & climsss_nlon, climsss_nlat

#endif
