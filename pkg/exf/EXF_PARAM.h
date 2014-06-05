C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_PARAM.h,v 1.33 2014/06/05 19:33:08 jmc Exp $
C $Name:  $
C
C     ==================================================================
C     HEADER EXF_PARAM.h
C     ==================================================================
C
C     o Header file for the surface flux data. Used by the external
C       forcing package.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C
C     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
C              - Restructured the original version in order to have a
C                better interface to the MITgcmUV.
C
C              Christian Eckert eckert@mit.edu  12-Feb-2000
C              - Changed some variables names (package prefix: exf_)
C
C              Patrick Heimbach, heimbach@mit.edu  04-May-2000
C              - included exf_iprec to enable easy
C                switch between 32bit/64 bit data format
C
C              Patrick Heimbach, heimbach@mit.edu  01-May-2001
C              - added obcs parameters
C
C     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C
C     ==================================================================
C     HEADER EXF_PARAM.h
C     ==================================================================

C     Repeat period for forcing fields (s)
C     For example, for yearly repeat period: repeatPeriod=31556925.
C     Note: this option is not yet coded for sub-daily
C           forcing and for leap years but this limitation can be
C           circumvented by using a 4-year (1461-day) repeatPeriod
      _RL     repeatPeriod

C     useExfCheckRange   :: check range of input/output field values
C     useExfYearlyFields :: when set, automatically add extension
C                           _YEAR to input file names; the yearly files need
C                           to contain all the records that pertain to
C                           a particular year, including day 1, hour zero
C     twoDigitYear       :: when set, use 2-digit year extension YR
C                           instead of _YEAR for useExfYearlyFields
C    useOBCSYearlyFields :: when reading Open-Boundary values, assume yearly
C                           climatology (def=false)
C     readStressOnAgrid  :: read wind-streess located on model-grid, A-grid position
C     readStressOnCgrid  :: read wind-streess located on model-grid, C-grid position
C     stressIsOnCgrid    :: ustress & vstress are positioned on Arakawa C-grid
C     useStabilityFct_overIce :: over sea-ice, compute turbulent transfert
C                                coeff. function of stability (like over
C                                open ocean) rather than using fixed Coeff.
C     useAtmWind         :: use wind vector (uwind/vwind) to compute
C                           the wind stress (ustress/vstress)
C     useRelativeWind    :: Subtract U/VVEL or U/VICE from U/VWIND before
C                           computing U/VSTRESS
C     noNegativeEvap     :: prevent negative evap (= sea-surface condensation)
C     useExfZenAlbedo    :: ocean albedo (direct part) may vary
C                           with zenith angle (see select_ZenAlbedo)
C     select_ZenAlbedo   :: switch to different methods to compute albedo (direct part)
C                        :: 0 just use exf_albedo
C                        :: 1 use daily mean albedo from exf_zenithangle_table.F
C                        :: 2 use daily mean albedo computed as in pkg/aim_v23
C                        :: 3 use daily variable albedo
C     useExfZenIncoming  :: compute incoming solar radiation along with zenith angle
C     exf_debugLev       :: select message printing to STDOUT (e.g., when read rec)
C     exf_monFreq        :: Monitor Frequency (s) for EXF

      logical useExfCheckRange
      logical useExfYearlyFields, twoDigitYear
      logical useOBCSYearlyFields
      logical readStressOnAgrid
      logical readStressOnCgrid
      logical stressIsOnCgrid
      logical useStabilityFct_overIce
      logical useRelativeWind
      logical noNegativeEvap
      logical useAtmWind

      logical useExfZenAlbedo
      integer select_ZenAlbedo
      logical useExfZenIncoming

      INTEGER exf_debugLev
      _RL     exf_monFreq

C     Drag coefficient scaling factor
      _RL     exf_scal_BulkCdn

C     Maximum absolute windstress, used to reset unreastically high
C     data values
      _RL     windstressmax

C     freezing temperature is the minimum temperature allowed, used
C     to reset climatological temperatures fields where they have
C     values below climtempfreeze
      _RL climtempfreeze

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Description of contents of surface boundary condition files
C     Note: fieldperiod=0 means input file is one time-constant field
C           fieldperiod=-12 means input file contains 12 monthly means

      integer hfluxstartdate1
      integer hfluxstartdate2
      _RL     hfluxstartdate
      _RL     hfluxperiod
      _RL     hfluxconst
      _RL     hflux_exfremo_intercept
      _RL     hflux_exfremo_slope
      character*1 hfluxmask

      integer atempstartdate1
      integer atempstartdate2
      _RL     atempstartdate
      _RL     atempperiod
      _RL     atempconst
      _RL     atemp_exfremo_intercept
      _RL     atemp_exfremo_slope
      character*1 atempmask

      integer aqhstartdate1
      integer aqhstartdate2
      _RL     aqhstartdate
      _RL     aqhperiod
      _RL     aqhconst
      _RL     aqh_exfremo_intercept
      _RL     aqh_exfremo_slope
      character*1 aqhmask

      integer sfluxstartdate1
      integer sfluxstartdate2
      _RL     sfluxstartdate
      _RL     sfluxperiod
      _RL     sfluxconst
      _RL     sflux_exfremo_intercept
      _RL     sflux_exfremo_slope
      character*1 sfluxmask

      integer evapstartdate1
      integer evapstartdate2
      _RL     evapstartdate
      _RL     evapperiod
      _RL     evapconst
      _RL     evap_exfremo_intercept
      _RL     evap_exfremo_slope
      character*1 evapmask

      integer precipstartdate1
      integer precipstartdate2
      _RL     precipstartdate
      _RL     precipperiod
      _RL     precipconst
      _RL     precip_exfremo_intercept
      _RL     precip_exfremo_slope
      character*1 precipmask

      integer snowprecipstartdate1
      integer snowprecipstartdate2
      _RL     snowprecipstartdate
      _RL     snowprecipperiod
      _RL     snowprecipconst
      _RL     snowprecip_exfremo_intercept
      _RL     snowprecip_exfremo_slope
      character*1 snowprecipmask

      integer runoffstartdate1
      integer runoffstartdate2
      _RL     runoffstartdate
      _RL     runoffperiod
      _RL     runoffconst
      _RL     runoff_exfremo_intercept
      _RL     runoff_exfremo_slope
      character*1 runoffmask

      _RL     runoftempconst
      _RL     runoftemp_exfremo_intercept
      _RL     runoftemp_exfremo_slope

      integer ustressstartdate1
      integer ustressstartdate2
      _RL     ustressstartdate
      _RL     ustressperiod
      _RL     ustressconst
      _RL     ustress_exfremo_intercept
      _RL     ustress_exfremo_slope
      character*1 ustressmask

      integer vstressstartdate1
      integer vstressstartdate2
      _RL     vstressstartdate
      _RL     vstressperiod
      _RL     vstressconst
      _RL     vstress_exfremo_intercept
      _RL     vstress_exfremo_slope
      character*1 vstressmask

      integer uwindstartdate1
      integer uwindstartdate2
      _RL     uwindstartdate
      _RL     uwindperiod
      _RL     uwindconst
      _RL     uwind_exfremo_intercept
      _RL     uwind_exfremo_slope
      character*1 uwindmask

      integer vwindstartdate1
      integer vwindstartdate2
      _RL     vwindstartdate
      _RL     vwindperiod
      _RL     vwindconst
      _RL     vwind_exfremo_intercept
      _RL     vwind_exfremo_slope
      character*1 vwindmask

      integer wspeedstartdate1
      integer wspeedstartdate2
      _RL     wspeedstartdate
      _RL     wspeedperiod
      _RL     wspeedconst
      _RL     wspeed_exfremo_intercept
      _RL     wspeed_exfremo_slope
      character*1 wspeedmask

      integer swfluxstartdate1
      integer swfluxstartdate2
      _RL     swfluxstartdate
      _RL     swfluxperiod
      _RL     swfluxconst
      _RL     swflux_exfremo_intercept
      _RL     swflux_exfremo_slope
      character*1 swfluxmask

      integer lwfluxstartdate1
      integer lwfluxstartdate2
      _RL     lwfluxstartdate
      _RL     lwfluxperiod
      _RL     lwfluxconst
      _RL     lwflux_exfremo_intercept
      _RL     lwflux_exfremo_slope
      character*1 lwfluxmask

      integer swdownstartdate1
      integer swdownstartdate2
      _RL     swdownstartdate
      _RL     swdownperiod
      _RL     swdownconst
      _RL     swdown_exfremo_intercept
      _RL     swdown_exfremo_slope
      character*1 swdownmask

      integer lwdownstartdate1
      integer lwdownstartdate2
      _RL     lwdownstartdate
      _RL     lwdownperiod
      _RL     lwdownconst
      _RL     lwdown_exfremo_intercept
      _RL     lwdown_exfremo_slope
      character*1 lwdownmask

      integer apressurestartdate1
      integer apressurestartdate2
      _RL     apressurestartdate
      _RL     apressureperiod
      _RL     apressureconst
      _RL     apressure_exfremo_intercept
      _RL     apressure_exfremo_slope
      character*1 apressuremask

      integer areamaskstartdate1
      integer areamaskstartdate2
      _RL     areamaskstartdate
      _RL     areamaskperiod
      _RL     areamaskTauRelax
      _RL     areamaskconst
      _RL     areamask_exfremo_intercept
      _RL     areamask_exfremo_slope
      character*1 areamaskmask

c     Calendar data.
      integer climsststartdate1
      integer climsststartdate2
      _RL     climsststartdate
      _RL     climsstperiod
      _RL     climsstTauRelax
      _RL     climsstconst
      _RL     climsst_exfremo_intercept
      _RL     climsst_exfremo_slope
      character*1 climsstmask

      integer climsssstartdate1
      integer climsssstartdate2
      _RL     climsssstartdate
      _RL     climsssperiod
      _RL     climsssTauRelax
      _RL     climsssconst
      _RL     climsss_exfremo_intercept
      _RL     climsss_exfremo_slope
      character*1 climsssmask

      integer climustrstartdate1
      integer climustrstartdate2
      _RL     climustrstartdate
      _RL     climustrperiod
      _RL     climustrTauRelax
      _RL     climustrconst
      _RL     climustr_exfremo_intercept
      _RL     climustr_exfremo_slope
      character*1 climustrmask

      integer climvstrstartdate1
      integer climvstrstartdate2
      _RL     climvstrstartdate
      _RL     climvstrperiod
      _RL     climvstrTauRelax
      _RL     climvstrconst
      _RL     climvstr_exfremo_intercept
      _RL     climvstr_exfremo_slope
      character*1 climvstrmask

c     the following variables are used in conjunction
c     with pkg/icefront to specify sub-glacial runoff
      integer sgrunoffstartdate1
      integer sgrunoffstartdate2
      _RL     sgrunoffstartdate
      _RL     sgrunoffperiod
      _RL     sgrunoffconst
      _RL     sgrunoff_exfremo_intercept
      _RL     sgrunoff_exfremo_slope
      _RL     exf_inscal_sgrunoff
      character*1 sgrunoffmask

c     the following variables are used in conjunction with pkg/obcs
c     to describe S/T/U/V open boundary condition files
      integer obcsNstartdate1
      integer obcsNstartdate2
      integer obcsSstartdate1
      integer obcsSstartdate2
      integer obcsEstartdate1
      integer obcsEstartdate2
      integer obcsWstartdate1
      integer obcsWstartdate2
      _RL     obcsNstartdate
      _RL     obcsNperiod
      _RL     obcsSstartdate
      _RL     obcsSperiod
      _RL     obcsEstartdate
      _RL     obcsEperiod
      _RL     obcsWstartdate
      _RL     obcsWperiod

c     the following variables are used in conjunction with pkg/obcs
c     and pkg/seaice to describe area, heff, hsnow, hsalt, uice,
c     and vice open boundary condition files
      integer siobNstartdate1
      integer siobNstartdate2
      integer siobSstartdate1
      integer siobSstartdate2
      integer siobEstartdate1
      integer siobEstartdate2
      integer siobWstartdate1
      integer siobWstartdate2
      _RL     siobNstartdate
      _RL     siobNperiod
      _RL     siobSstartdate
      _RL     siobSperiod
      _RL     siobEstartdate
      _RL     siobEperiod
      _RL     siobWstartdate
      _RL     siobWperiod

c     File names.
      character*(128) hfluxfile
      character*(128) atempfile
      character*(128) aqhfile
      character*(128) evapfile
      character*(128) precipfile
      character*(128) snowprecipfile
      character*(128) sfluxfile
      character*(128) runofffile
      character*(128) runoftempfile
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
      character*(128) areamaskfile
      character*(128) climsstfile
      character*(128) climsssfile
      character*(128) climustrfile
      character*(128) climvstrfile

      COMMON /EXF_PARAM_L/
     &       useExfCheckRange,
     &       useExfYearlyFields, twoDigitYear,
     &       useOBCSYearlyFields,
     &       useExfZenAlbedo, useExfZenIncoming,
     &       readStressOnAgrid, readStressOnCgrid,
     &       stressIsOnCgrid, useStabilityFct_overIce,
     &       useAtmWind, useRelativeWind, noNegativeEvap
      COMMON /EXF_PARAM_I/
     &       select_ZenAlbedo,  exf_debugLev,
     &       hfluxstartdate1,   hfluxstartdate2,
     &       atempstartdate1,   atempstartdate2,
     &       aqhstartdate1,     aqhstartdate2,
     &       sfluxstartdate1,   sfluxstartdate2,
     &       evapstartdate1,    evapstartdate2,
     &       runoffstartdate1,  runoffstartdate2,
     &       sgrunoffstartdate1,sgrunoffstartdate2,
     &       precipstartdate1,  precipstartdate2,
     &       snowprecipstartdate1, snowprecipstartdate2,
     &       ustressstartdate1, ustressstartdate2,
     &       vstressstartdate1, vstressstartdate2,
     &       uwindstartdate1,   uwindstartdate2,
     &       vwindstartdate1,   vwindstartdate2,
     &       wspeedstartdate1,  wspeedstartdate2,
     &       swfluxstartdate1,  swfluxstartdate2,
     &       lwfluxstartdate1,  lwfluxstartdate2,
     &       swdownstartdate1,  swdownstartdate2,
     &       lwdownstartdate1,  lwdownstartdate2,
     &       obcsNstartdate1,   obcsNstartdate2,
     &       obcsSstartdate1,   obcsSstartdate2,
     &       obcsEstartdate1,   obcsEstartdate2,
     &       obcsWstartdate1,   obcsWstartdate2,
     &       siobNstartdate1,   siobNstartdate2,
     &       siobSstartdate1,   siobSstartdate2,
     &       siobEstartdate1,   siobEstartdate2,
     &       siobWstartdate1,   siobWstartdate2,
     &       apressurestartdate1,apressurestartdate2,
     &       areamaskstartdate1,areamaskstartdate2

      COMMON /EXF_PARAM_R/
     &       repeatPeriod,      exf_monFreq,
     &       exf_scal_BulkCdn,  windstressmax,
     &       hfluxperiod,       hfluxstartdate,
     &       atempperiod,       atempstartdate,
     &       aqhperiod,         aqhstartdate,
     &       sfluxperiod,       sfluxstartdate,
     &       evapperiod,        evapstartdate,
     &       precipperiod,      precipstartdate,
     &       snowprecipperiod,  snowprecipstartdate,
     &       runoffperiod,      runoffstartdate,
     &       sgrunoffperiod,    sgrunoffstartdate,
     &       ustressperiod,     ustressstartdate,
     &       vstressperiod,     vstressstartdate,
     &       uwindperiod,       uwindstartdate,
     &       vwindperiod,       vwindstartdate,
     &       wspeedperiod,      wspeedstartdate,
     &       swfluxperiod,      swfluxstartdate,
     &       lwfluxperiod,      lwfluxstartdate,
     &       swdownperiod,      swdownstartdate,
     &       lwdownperiod,      lwdownstartdate,
     &       obcsNperiod,       obcsNstartdate,
     &       obcsSperiod,       obcsSstartdate,
     &       obcsEperiod,       obcsEstartdate,
     &       obcsWperiod,       obcsWstartdate,
     &       siobNperiod,       siobNstartdate,
     &       siobSperiod,       siobSstartdate,
     &       siobEperiod,       siobEstartdate,
     &       siobWperiod,       siobWstartdate,
     &       apressureperiod,   apressurestartdate,
     &       areamaskperiod,   areamaskstartdate,
     &       hfluxconst,
     &       atempconst,
     &       aqhconst,
     &       sfluxconst,
     &       evapconst,
     &       precipconst,
     &       snowprecipconst,
     &       runoffconst,
     &       runoftempconst,
     &       sgrunoffconst,
     &       ustressconst,
     &       vstressconst,
     &       uwindconst,
     &       vwindconst,
     &       wspeedconst,
     &       swfluxconst,
     &       lwfluxconst,
     &       swdownconst,
     &       lwdownconst,
     &       apressureconst,
     &       areamaskTauRelax,
     &       areamaskconst

      COMMON /EXF_PARAM_TREND_REMOVAL/
     &       hflux_exfremo_intercept,
     &       atemp_exfremo_intercept,
     &       aqh_exfremo_intercept,
     &       sflux_exfremo_intercept,
     &       evap_exfremo_intercept,
     &       precip_exfremo_intercept,
     &       snowprecip_exfremo_intercept,
     &       runoff_exfremo_intercept,
     &       runoftemp_exfremo_intercept,
     &       sgrunoff_exfremo_intercept,
     &       ustress_exfremo_intercept,
     &       vstress_exfremo_intercept,
     &       uwind_exfremo_intercept,
     &       vwind_exfremo_intercept,
     &       wspeed_exfremo_intercept,
     &       swflux_exfremo_intercept,
     &       lwflux_exfremo_intercept,
     &       swdown_exfremo_intercept,
     &       lwdown_exfremo_intercept,
     &       apressure_exfremo_intercept,
     &       areamask_exfremo_intercept,
     &       hflux_exfremo_slope,
     &       atemp_exfremo_slope,
     &       aqh_exfremo_slope,
     &       sflux_exfremo_slope,
     &       evap_exfremo_slope,
     &       precip_exfremo_slope,
     &       snowprecip_exfremo_slope,
     &       runoff_exfremo_slope,
     &       runoftemp_exfremo_slope,
     &       sgrunoff_exfremo_slope,
     &       ustress_exfremo_slope,
     &       vstress_exfremo_slope,
     &       uwind_exfremo_slope,
     &       vwind_exfremo_slope,
     &       wspeed_exfremo_slope,
     &       swflux_exfremo_slope,
     &       lwflux_exfremo_slope,
     &       swdown_exfremo_slope,
     &       lwdown_exfremo_slope,
     &       apressure_exfremo_slope,
     &       areamask_exfremo_slope

      COMMON /EXF_PARAM_C/
     &       hfluxfile,     hfluxmask,
     &       atempfile,     atempmask,
     &       aqhfile,       aqhmask,
     &       sfluxfile,     sfluxmask,
     &       evapfile,      evapmask,
     &       precipfile,    precipmask,
     &       snowprecipfile,snowprecipmask,
     &       runofffile,    runoffmask,
     &       runoftempfile,
     &       ustressfile,   ustressmask,
     &       vstressfile,   vstressmask,
     &       uwindfile,     uwindmask,
     &       vwindfile,     vwindmask,
     &       wspeedfile,    wspeedmask,
     &       swfluxfile,    swfluxmask,
     &       lwfluxfile,    lwfluxmask,
     &       swdownfile,    swdownmask,
     &       lwdownfile,    lwdownmask,
     &       apressurefile, apressuremask,
     &       areamaskfile,  areamaskmask,
     &                      sgrunoffmask

      COMMON /EXF_CLIM_I/
     &       climsststartdate1,  climsststartdate2,
     &       climsssstartdate1,  climsssstartdate2,
     &       climustrstartdate1,  climustrstartdate2,
     &       climvstrstartdate1,  climvstrstartdate2

      COMMON /EXF_CLIM_C/
     &       climsstfile,  climsstmask,
     &       climsssfile,  climsssmask,
     &       climustrfile, climustrmask,
     &       climvstrfile, climvstrmask

      COMMON /EXF_CLIM_R/
     &       climtempfreeze,
     &       climsstperiod,      climsststartdate,
     &       climsssperiod,      climsssstartdate,
     &       climustrperiod,     climustrstartdate,
     &       climvstrperiod,     climvstrstartdate,
     &       climsstTauRelax,    climsssTauRelax,
     &       climustrTauRelax,   climvstrTauRelax,
     &       climsstconst,       climsssconst,
     &       climustrconst,      climvstrconst,
     &       climsst_exfremo_intercept, climsst_exfremo_slope,
     &       climsss_exfremo_intercept, climsss_exfremo_slope,
     &       climustr_exfremo_intercept, climustr_exfremo_slope,
     &       climvstr_exfremo_intercept, climvstr_exfremo_slope,
     &       exf_inscal_climsst, exf_inscal_climsss,
     &       exf_inscal_climustr, exf_inscal_climvstr

c     file precision and field type

      COMMON /EXF_PARAM_TYPE/
     &       exf_iprec,
     &       exf_iprec_obcs

      integer exf_iprec
      integer exf_iprec_obcs

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
      _RL     exf_inscal_runoftemp
      _RL     exf_inscal_swdown
      _RL     exf_inscal_lwdown
      _RL     exf_inscal_climsst
      _RL     exf_inscal_climsss
      _RL     exf_inscal_climustr
      _RL     exf_inscal_climvstr
      _RL     exf_inscal_areamask

      _RL     exf_outscal_hflux
      _RL     exf_outscal_sflux
      _RL     exf_outscal_ustress
      _RL     exf_outscal_vstress
      _RL     exf_outscal_swflux
      _RL     exf_outscal_sst
      _RL     exf_outscal_sss
      _RL     exf_outscal_apressure
      _RL     exf_outscal_areamask

      COMMON /EXF_PARAM_SCAL/
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
     &                    , exf_inscal_runoftemp
     &                    , exf_inscal_sgrunoff
     &                    , exf_inscal_swdown
     &                    , exf_inscal_lwdown
     &                    , exf_inscal_areamask
     &                    , exf_outscal_hflux
     &                    , exf_outscal_sflux
     &                    , exf_outscal_ustress
     &                    , exf_outscal_vstress
     &                    , exf_outscal_swflux
     &                    , exf_outscal_sst
     &                    , exf_outscal_sss
     &                    , exf_outscal_apressure
     &                    , exf_outscal_areamask

#ifndef USE_EXF_INTERPOLATION
c-- set dummy dimension 1
      INTEGER    exf_interp_bufferSize
      PARAMETER( exf_interp_bufferSize = 1 )
      INTEGER MAX_LAT_INC
      PARAMETER(MAX_LAT_INC = 1)
#else /* USE_EXF_INTERPOLATION */
C  To read input data without dynamical allocation (EXF_INTERP_USE_DYNALLOC undef),
C  buffer size currently set to 65000 (allows to read-in a 1x1 global data set)
C  Increase to 140000 to accommodate for ECMWF-INTERIM
      INTEGER    exf_interp_bufferSize
      PARAMETER( exf_interp_bufferSize = 140000 )
c for lat interpolation, arraysize currently set to 1279 max data values
c to accomodate ECMWF operational analysis
      INTEGER MAX_LAT_INC
      PARAMETER(MAX_LAT_INC = 1279)

C-- Interpolation parameters (for each input field):
C  {inputField}_lon0    :: longitude of the 1rst point (South-East corner)
C  {inputField}_lon_inc :: longitude increment (uniform)
C  {inputField}_lat0    :: latitude  of the 1rst point (South-East corner)
C  {inputField}_lat_inc :: latitude  increment (vector, fct of latitude only)
C  {inputField}_nlon    :: input filed 1rst dim, longitudinal direction
C  {inputField}_nlat    :: input filed 2nd  dim, latitudinal  direction
C  {inputField}_interpMethod :: interpolation method: =0 : no interpolation ;
C                            :: =1,11,21 : bilinear ; =2,12,22 : bicubic ;
C                            :: =1,2 for tracer ; =11,12 for U ; =21,22 for V.
C-  For 2 components vector field:
C    uvInterp_stress    :: interpolate wind-stress u & v components together
C    uvInterp_wind      :: interpolate wind        u & v components together
C    uvInterp_climstr   :: interpolate clim stress u & v components together
      _RL ustress_lon0, ustress_lon_inc
      _RL ustress_lat0, ustress_lat_inc(MAX_LAT_INC)
      INTEGER ustress_nlon, ustress_nlat, ustress_interpMethod
      _RL vstress_lon0, vstress_lon_inc
      _RL vstress_lat0, vstress_lat_inc(MAX_LAT_INC)
      INTEGER vstress_nlon, vstress_nlat, vstress_interpMethod
      _RL hflux_lon0, hflux_lon_inc
      _RL hflux_lat0, hflux_lat_inc(MAX_LAT_INC)
      INTEGER hflux_nlon, hflux_nlat, hflux_interpMethod
      _RL sflux_lon0, sflux_lon_inc
      _RL sflux_lat0, sflux_lat_inc(MAX_LAT_INC)
      INTEGER sflux_nlon, sflux_nlat, sflux_interpMethod
      _RL swflux_lon0, swflux_lon_inc
      _RL swflux_lat0, swflux_lat_inc(MAX_LAT_INC)
      INTEGER swflux_nlon, swflux_nlat, swflux_interpMethod
      _RL runoff_lon0, runoff_lon_inc
      _RL runoff_lat0, runoff_lat_inc(MAX_LAT_INC)
      INTEGER runoff_nlon, runoff_nlat, runoff_interpMethod
      _RL atemp_lon0, atemp_lon_inc
      _RL atemp_lat0, atemp_lat_inc(MAX_LAT_INC)
      INTEGER atemp_nlon, atemp_nlat, atemp_interpMethod
      _RL aqh_lon0, aqh_lon_inc
      _RL aqh_lat0, aqh_lat_inc(MAX_LAT_INC)
      INTEGER aqh_nlon, aqh_nlat, aqh_interpMethod
      _RL evap_lon0, evap_lon_inc
      _RL evap_lat0, evap_lat_inc(MAX_LAT_INC)
      INTEGER evap_nlon, evap_nlat, evap_interpMethod
      _RL precip_lon0, precip_lon_inc
      _RL precip_lat0, precip_lat_inc(MAX_LAT_INC)
      INTEGER precip_nlon, precip_nlat, precip_interpMethod
      _RL snowprecip_lon0, snowprecip_lon_inc
      _RL snowprecip_lat0, snowprecip_lat_inc(MAX_LAT_INC)
      INTEGER snowprecip_nlon, snowprecip_nlat, snowprecip_interpMethod
      _RL uwind_lon0, uwind_lon_inc
      _RL uwind_lat0, uwind_lat_inc(MAX_LAT_INC)
      INTEGER uwind_nlon, uwind_nlat, uwind_interpMethod
      _RL vwind_lon0, vwind_lon_inc
      _RL vwind_lat0, vwind_lat_inc(MAX_LAT_INC)
      INTEGER vwind_nlon, vwind_nlat, vwind_interpMethod
      _RL wspeed_lon0, wspeed_lon_inc
      _RL wspeed_lat0, wspeed_lat_inc(MAX_LAT_INC)
      INTEGER wspeed_nlon, wspeed_nlat, wspeed_interpMethod
      _RL lwflux_lon0, lwflux_lon_inc
      _RL lwflux_lat0, lwflux_lat_inc(MAX_LAT_INC)
      INTEGER lwflux_nlon, lwflux_nlat, lwflux_interpMethod
      _RL swdown_lon0, swdown_lon_inc
      _RL swdown_lat0, swdown_lat_inc(MAX_LAT_INC)
      INTEGER swdown_nlon, swdown_nlat, swdown_interpMethod
      _RL lwdown_lon0, lwdown_lon_inc
      _RL lwdown_lat0, lwdown_lat_inc(MAX_LAT_INC)
      INTEGER lwdown_nlon, lwdown_nlat, lwdown_interpMethod
      _RL apressure_lon0,apressure_lon_inc
      _RL apressure_lat0,apressure_lat_inc(MAX_LAT_INC)
      INTEGER apressure_nlon,apressure_nlat,apressure_interpMethod
      _RL areamask_lon0,areamask_lon_inc
      _RL areamask_lat0,areamask_lat_inc(MAX_LAT_INC)
      INTEGER areamask_nlon,areamask_nlat,areamask_interpMethod

      LOGICAL uvInterp_stress
      LOGICAL uvInterp_wind
      LOGICAL uvInterp_climstr
      COMMON /EXF_INTERPOLATION_L/
     & uvInterp_stress, uvInterp_wind, uvInterp_climstr

      COMMON /EXF_INTERPOLATION_RL/
     & ustress_lon0, ustress_lon_inc,
     & ustress_lat0, ustress_lat_inc,
     & vstress_lon0, vstress_lon_inc,
     & vstress_lat0, vstress_lat_inc,
     & hflux_lon0, hflux_lon_inc,
     & hflux_lat0, hflux_lat_inc,
     & sflux_lon0, sflux_lon_inc,
     & sflux_lat0, sflux_lat_inc,
     & swflux_lon0, swflux_lon_inc,
     & swflux_lat0, swflux_lat_inc,
     & runoff_lon0, runoff_lon_inc,
     & runoff_lat0, runoff_lat_inc,
     & atemp_lon0, atemp_lon_inc,
     & atemp_lat0, atemp_lat_inc,
     & aqh_lon0, aqh_lon_inc,
     & aqh_lat0, aqh_lat_inc,
     & evap_lon0, evap_lon_inc,
     & evap_lat0, evap_lat_inc,
     & precip_lon0, precip_lon_inc,
     & precip_lat0, precip_lat_inc,
     & snowprecip_lon0, snowprecip_lon_inc,
     & snowprecip_lat0, snowprecip_lat_inc,
     & uwind_lon0, uwind_lon_inc,
     & uwind_lat0, uwind_lat_inc,
     & vwind_lon0, vwind_lon_inc,
     & vwind_lat0, vwind_lat_inc,
     & wspeed_lon0, wspeed_lon_inc,
     & wspeed_lat0, wspeed_lat_inc,
     & lwflux_lon0, lwflux_lon_inc,
     & lwflux_lat0, lwflux_lat_inc,
     & swdown_lon0, swdown_lon_inc,
     & swdown_lat0, swdown_lat_inc,
     & lwdown_lon0, lwdown_lon_inc,
     & lwdown_lat0, lwdown_lat_inc,
     & apressure_lon0,apressure_lon_inc,
     & apressure_lat0,apressure_lat_inc,
     & areamask_lon0,areamask_lon_inc,
     & areamask_lat0,areamask_lat_inc

      COMMON /EXF_INTERPOLATION_I/
     & ustress_nlon, ustress_nlat, ustress_interpMethod,
     & vstress_nlon, vstress_nlat, vstress_interpMethod,
     & hflux_nlon, hflux_nlat, hflux_interpMethod,
     & sflux_nlon, sflux_nlat, sflux_interpMethod,
     & swflux_nlon, swflux_nlat, swflux_interpMethod,
     & runoff_nlon, runoff_nlat, runoff_interpMethod,
     & atemp_nlon, atemp_nlat, atemp_interpMethod,
     & aqh_nlon, aqh_nlat, aqh_interpMethod,
     & evap_nlon, evap_nlat, evap_interpMethod,
     & precip_nlon, precip_nlat, precip_interpMethod,
     & snowprecip_nlon, snowprecip_nlat, snowprecip_interpMethod,
     & uwind_nlon, uwind_nlat, uwind_interpMethod,
     & vwind_nlon, vwind_nlat, vwind_interpMethod,
     & wspeed_nlon, wspeed_nlat, wspeed_interpMethod,
     & lwflux_nlon, lwflux_nlat, lwflux_interpMethod,
     & swdown_nlon, swdown_nlat, swdown_interpMethod,
     & lwdown_nlon, lwdown_nlat, lwdown_interpMethod,
     & apressure_nlon,apressure_nlat,apressure_interpMethod,
     & areamask_nlon,areamask_nlat,areamask_interpMethod

      _RL climsst_lon0, climsst_lon_inc
      _RL climsst_lat0, climsst_lat_inc(MAX_LAT_INC)
      INTEGER climsst_nlon, climsst_nlat, climsst_interpMethod
      _RL climsss_lon0, climsss_lon_inc
      _RL climsss_lat0, climsss_lat_inc(MAX_LAT_INC)
      INTEGER climsss_nlon, climsss_nlat, climsss_interpMethod
      _RL climustr_lon0, climustr_lon_inc
      _RL climustr_lat0, climustr_lat_inc(MAX_LAT_INC)
      INTEGER climustr_nlon, climustr_nlat, climustr_interpMethod
      _RL climvstr_lon0, climvstr_lon_inc
      _RL climvstr_lat0, climvstr_lat_inc(MAX_LAT_INC)
      INTEGER climvstr_nlon, climvstr_nlat, climvstr_interpMethod

      COMMON /EXF_CLIM_INTERPOLATION/
     & climsst_lon0, climsst_lon_inc,
     & climsst_lat0, climsst_lat_inc,
     & climsss_lon0, climsss_lon_inc,
     & climsss_lat0, climsss_lat_inc,
     & climustr_lon0, climustr_lon_inc,
     & climustr_lat0, climustr_lat_inc,
     & climvstr_lon0, climvstr_lon_inc,
     & climvstr_lat0, climvstr_lat_inc,
     & climsst_nlon, climsst_nlat, climsst_interpMethod,
     & climsss_nlon, climsss_nlat, climsss_interpMethod,
     & climustr_nlon, climustr_nlat, climustr_interpMethod,
     & climvstr_nlon, climvstr_nlat, climvstr_interpMethod

#endif /* USE_EXF_INTERPOLATION */
