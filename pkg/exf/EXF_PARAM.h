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
C     rotateStressOnAgrid  :: rotate from zonal/meridional components to U/V components
C     readStressOnCgrid  :: read wind-streess located on model-grid, C-grid position
C     stressIsOnCgrid    :: ustress & vstress are positioned on Arakawa C-grid
C     useAtmWind         :: use wind vector (uwind/vwind) to compute
C                           the wind stress (ustress/vstress)
C     useRelativeWind    :: Subtract U/VVEL or U/VICE from U/VWIND before
C                           computing U/VSTRESS
C     noNegativeEvap     :: prevent negative evap (= sea-surface condensation)
C    useStabilityFct_overIce :: over sea-ice, compute turbulent transfert
C                               coeff. function of stability (like over
C                               open ocean) rather than using fixed Coeff.
C    diags_opOceWeighted :: weight surface flux diagnostics with open-ocean fraction
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

      LOGICAL useExfCheckRange
      LOGICAL useExfYearlyFields, twoDigitYear
      LOGICAL useOBCSYearlyFields
      LOGICAL readStressOnAgrid
      LOGICAL rotateStressOnAgrid
      LOGICAL readStressOnCgrid
      LOGICAL stressIsOnCgrid
      LOGICAL useAtmWind
      LOGICAL useRelativeWind
      LOGICAL noNegativeEvap
      LOGICAL useStabilityFct_overIce
      LOGICAL diags_opOceWeighted

      LOGICAL useExfZenAlbedo
      INTEGER select_ZenAlbedo
      LOGICAL useExfZenIncoming

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
C-    for each field:
C     {fld}file       :: file-name for this field
C     {fld}startdate1 :: field starting date (YYYYMMDD)
C     {fld}startdate1 :: field starting date (YYYYMMDD)
C     {fld}startdate2 :: field starting date (HHMMSS)
C     {fld}StartTime  :: corresponding starting time (in sec) for this field
C     {fld}period     :: time period (in sec) between 2 reccords
C     {fld}RepCycle   :: time duration of a repeating cycle
C     {fld}const      :: uniform default field value

      INTEGER hfluxstartdate1
      INTEGER hfluxstartdate2
      _RL     hfluxStartTime
      _RL     hfluxperiod
      _RL     hfluxRepCycle
      _RL     hfluxconst
      _RL     hflux_exfremo_intercept
      _RL     hflux_exfremo_slope
      CHARACTER*1 hfluxmask

      INTEGER atempstartdate1
      INTEGER atempstartdate2
      _RL     atempStartTime
      _RL     atempperiod
      _RL     atempRepCycle
      _RL     atempconst
      _RL     atemp_exfremo_intercept
      _RL     atemp_exfremo_slope
      CHARACTER*1 atempmask

      INTEGER aqhstartdate1
      INTEGER aqhstartdate2
      _RL     aqhStartTime
      _RL     aqhperiod
      _RL     aqhRepCycle
      _RL     aqhconst
      _RL     aqh_exfremo_intercept
      _RL     aqh_exfremo_slope
      CHARACTER*1 aqhmask

      INTEGER hs_startdate1
      INTEGER hs_startdate2
      _RL     hs_StartTime
      _RL     hs_period
      _RL     hs_RepCycle
      _RL     hs_const
      _RL     hs_exfremo_intercept
      _RL     hs_exfremo_slope
      CHARACTER*1 hs_mask

      INTEGER hl_startdate1
      INTEGER hl_startdate2
      _RL     hl_StartTime
      _RL     hl_period
      _RL     hl_RepCycle
      _RL     hl_const
      _RL     hl_exfremo_intercept
      _RL     hl_exfremo_slope
      CHARACTER*1 hl_mask

      INTEGER sfluxstartdate1
      INTEGER sfluxstartdate2
      _RL     sfluxStartTime
      _RL     sfluxperiod
      _RL     sfluxRepCycle
      _RL     sfluxconst
      _RL     sflux_exfremo_intercept
      _RL     sflux_exfremo_slope
      CHARACTER*1 sfluxmask

      INTEGER evapstartdate1
      INTEGER evapstartdate2
      _RL     evapStartTime
      _RL     evapperiod
      _RL     evapRepCycle
      _RL     evapconst
      _RL     evap_exfremo_intercept
      _RL     evap_exfremo_slope
      CHARACTER*1 evapmask

      INTEGER precipstartdate1
      INTEGER precipstartdate2
      _RL     precipStartTime
      _RL     precipperiod
      _RL     precipRepCycle
      _RL     precipconst
      _RL     precip_exfremo_intercept
      _RL     precip_exfremo_slope
      CHARACTER*1 precipmask

      INTEGER snowprecipstartdate1
      INTEGER snowprecipstartdate2
      _RL     snowprecipStartTime
      _RL     snowprecipperiod
      _RL     snowprecipRepCycle
      _RL     snowprecipconst
      _RL     snowprecip_exfremo_intercept
      _RL     snowprecip_exfremo_slope
      CHARACTER*1 snowprecipmask

      INTEGER runoffstartdate1
      INTEGER runoffstartdate2
      _RL     runoffStartTime
      _RL     runoffperiod
      _RL     runoffRepCycle
      _RL     runoffconst
      _RL     runoff_exfremo_intercept
      _RL     runoff_exfremo_slope
      CHARACTER*1 runoffmask

      _RL     runoftempconst
      _RL     runoftemp_exfremo_intercept
      _RL     runoftemp_exfremo_slope

      INTEGER saltflxstartdate1
      INTEGER saltflxstartdate2
      _RL     saltflxStartTime
      _RL     saltflxperiod
      _RL     saltflxRepCycle
      _RL     saltflxconst
      _RL     saltflx_exfremo_intercept
      _RL     saltflx_exfremo_slope
      CHARACTER*1 saltflxmask

      INTEGER ustressstartdate1
      INTEGER ustressstartdate2
      _RL     ustressStartTime
      _RL     ustressperiod
      _RL     ustressRepCycle
      _RL     ustressconst
      _RL     ustress_exfremo_intercept
      _RL     ustress_exfremo_slope
      CHARACTER*1 ustressmask

      INTEGER vstressstartdate1
      INTEGER vstressstartdate2
      _RL     vstressStartTime
      _RL     vstressperiod
      _RL     vstressRepCycle
      _RL     vstressconst
      _RL     vstress_exfremo_intercept
      _RL     vstress_exfremo_slope
      CHARACTER*1 vstressmask

      INTEGER uwindstartdate1
      INTEGER uwindstartdate2
      _RL     uwindStartTime
      _RL     uwindperiod
      _RL     uwindRepCycle
      _RL     uwindconst
      _RL     uwind_exfremo_intercept
      _RL     uwind_exfremo_slope
      CHARACTER*1 uwindmask

      INTEGER vwindstartdate1
      INTEGER vwindstartdate2
      _RL     vwindStartTime
      _RL     vwindperiod
      _RL     vwindRepCycle
      _RL     vwindconst
      _RL     vwind_exfremo_intercept
      _RL     vwind_exfremo_slope
      CHARACTER*1 vwindmask

      INTEGER wspeedstartdate1
      INTEGER wspeedstartdate2
      _RL     wspeedStartTime
      _RL     wspeedperiod
      _RL     wspeedRepCycle
      _RL     wspeedconst
      _RL     wspeed_exfremo_intercept
      _RL     wspeed_exfremo_slope
      CHARACTER*1 wspeedmask

      INTEGER swfluxstartdate1
      INTEGER swfluxstartdate2
      _RL     swfluxStartTime
      _RL     swfluxperiod
      _RL     swfluxRepCycle
      _RL     swfluxconst
      _RL     swflux_exfremo_intercept
      _RL     swflux_exfremo_slope
      CHARACTER*1 swfluxmask

      INTEGER lwfluxstartdate1
      INTEGER lwfluxstartdate2
      _RL     lwfluxStartTime
      _RL     lwfluxperiod
      _RL     lwfluxRepCycle
      _RL     lwfluxconst
      _RL     lwflux_exfremo_intercept
      _RL     lwflux_exfremo_slope
      CHARACTER*1 lwfluxmask

      INTEGER swdownstartdate1
      INTEGER swdownstartdate2
      _RL     swdownStartTime
      _RL     swdownperiod
      _RL     swdownRepCycle
      _RL     swdownconst
      _RL     swdown_exfremo_intercept
      _RL     swdown_exfremo_slope
      CHARACTER*1 swdownmask

      INTEGER lwdownstartdate1
      INTEGER lwdownstartdate2
      _RL     lwdownStartTime
      _RL     lwdownperiod
      _RL     lwdownRepCycle
      _RL     lwdownconst
      _RL     lwdown_exfremo_intercept
      _RL     lwdown_exfremo_slope
      CHARACTER*1 lwdownmask

      INTEGER apressurestartdate1
      INTEGER apressurestartdate2
      _RL     apressureStartTime
      _RL     apressureperiod
      _RL     apressureRepCycle
      _RL     apressureconst
      _RL     apressure_exfremo_intercept
      _RL     apressure_exfremo_slope
      CHARACTER*1 apressuremask

      INTEGER tidePotStartdate1
      INTEGER tidePotStartdate2
      _RL     tidePotStartTime
      _RL     tidePotPeriod
      _RL     tidePotRepCycle
      _RL     tidePotConst
      _RL     tidePot_exfremo_intercept
      _RL     tidePot_exfremo_slope
      CHARACTER*1 tidePotMask

      INTEGER areamaskstartdate1
      INTEGER areamaskstartdate2
      _RL     areamaskStartTime
      _RL     areamaskperiod
      _RL     areamaskRepCycle
      _RL     areamaskTauRelax
      _RL     areamaskconst
      _RL     areamask_exfremo_intercept
      _RL     areamask_exfremo_slope
      CHARACTER*1 areamaskmask

C     Calendar data.
      INTEGER climsststartdate1
      INTEGER climsststartdate2
      _RL     climsstStartTime
      _RL     climsstperiod
      _RL     climsstRepCycle
      _RL     climsstTauRelax
      _RL     climsstconst
      _RL     climsst_exfremo_intercept
      _RL     climsst_exfremo_slope
      CHARACTER*1 climsstmask

      INTEGER climsssstartdate1
      INTEGER climsssstartdate2
      _RL     climsssStartTime
      _RL     climsssperiod
      _RL     climsssRepCycle
      _RL     climsssTauRelax
      _RL     climsssconst
      _RL     climsss_exfremo_intercept
      _RL     climsss_exfremo_slope
      CHARACTER*1 climsssmask

      INTEGER climustrstartdate1
      INTEGER climustrstartdate2
      _RL     climustrStartTime
      _RL     climustrperiod
      _RL     climustrRepCycle
      _RL     climustrTauRelax
      _RL     climustrconst
      _RL     climustr_exfremo_intercept
      _RL     climustr_exfremo_slope
      CHARACTER*1 climustrmask

      INTEGER climvstrstartdate1
      INTEGER climvstrstartdate2
      _RL     climvstrStartTime
      _RL     climvstrperiod
      _RL     climvstrRepCycle
      _RL     climvstrTauRelax
      _RL     climvstrconst
      _RL     climvstr_exfremo_intercept
      _RL     climvstr_exfremo_slope
      CHARACTER*1 climvstrmask

C-    The following variables are used in conjunction with pkg/obcs
C     to describe S/T/U/V open boundary condition files
      INTEGER obcsNstartdate1
      INTEGER obcsNstartdate2
      INTEGER obcsSstartdate1
      INTEGER obcsSstartdate2
      INTEGER obcsEstartdate1
      INTEGER obcsEstartdate2
      INTEGER obcsWstartdate1
      INTEGER obcsWstartdate2
      _RL     obcsNstartTime
      _RL     obcsNperiod
      _RL     obcsNrepCycle
      _RL     obcsSstartTime
      _RL     obcsSperiod
      _RL     obcsSrepCycle
      _RL     obcsEstartTime
      _RL     obcsEperiod
      _RL     obcsErepCycle
      _RL     obcsWstartTime
      _RL     obcsWperiod
      _RL     obcsWrepCycle

C-    The following variables are used in conjunction with pkg/obcs
C     and pkg/seaice to describe area, heff, hsnow, hsalt, uice,
C     and vice open boundary condition files
      INTEGER siobNstartdate1
      INTEGER siobNstartdate2
      INTEGER siobSstartdate1
      INTEGER siobSstartdate2
      INTEGER siobEstartdate1
      INTEGER siobEstartdate2
      INTEGER siobWstartdate1
      INTEGER siobWstartdate2
      _RL     siobNstartTime
      _RL     siobNperiod
      _RL     siobNrepCycle
      _RL     siobSstartTime
      _RL     siobSperiod
      _RL     siobSrepCycle
      _RL     siobEstartTime
      _RL     siobEperiod
      _RL     siobErepCycle
      _RL     siobWstartTime
      _RL     siobWperiod
      _RL     siobWrepCycle

C-    File names.
      CHARACTER*(128) hfluxfile
      CHARACTER*(128) atempfile
      CHARACTER*(128) aqhfile
      CHARACTER*(128) hs_file
      CHARACTER*(128) hl_file
      CHARACTER*(128) evapfile
      CHARACTER*(128) precipfile
      CHARACTER*(128) snowprecipfile
      CHARACTER*(128) sfluxfile
      CHARACTER*(128) runofffile
      CHARACTER*(128) runoftempfile
      CHARACTER*(128) saltflxfile
      CHARACTER*(128) ustressfile
      CHARACTER*(128) vstressfile
      CHARACTER*(128) uwindfile
      CHARACTER*(128) vwindfile
      CHARACTER*(128) wspeedfile
      CHARACTER*(128) swfluxfile
      CHARACTER*(128) lwfluxfile
      CHARACTER*(128) swdownfile
      CHARACTER*(128) lwdownfile
      CHARACTER*(128) apressurefile
      CHARACTER*(128) tidePotFile
      CHARACTER*(128) areamaskfile
      CHARACTER*(128) climsstfile
      CHARACTER*(128) climsssfile
      CHARACTER*(128) climustrfile
      CHARACTER*(128) climvstrfile

      COMMON /EXF_PARAM_L/
     &       useExfCheckRange,
     &       useExfYearlyFields, twoDigitYear,
     &       useOBCSYearlyFields,
     &       useExfZenAlbedo, useExfZenIncoming,
     &       readStressOnAgrid, readStressOnCgrid,
     &       stressIsOnCgrid, rotateStressOnAgrid,
     &       useAtmWind, useRelativeWind, noNegativeEvap,
     &       useStabilityFct_overIce, diags_opOceWeighted

      COMMON /EXF_PARAM_I/
     &       select_ZenAlbedo,  exf_debugLev,
     &       hfluxstartdate1,   hfluxstartdate2,
     &       atempstartdate1,   atempstartdate2,
     &       aqhstartdate1,     aqhstartdate2,
     &       hs_startdate1,     hs_startdate2,
     &       hl_startdate1,     hl_startdate2,
     &       sfluxstartdate1,   sfluxstartdate2,
     &       evapstartdate1,    evapstartdate2,
     &       runoffstartdate1,  runoffstartdate2,
     &       saltflxstartdate1, saltflxstartdate2,
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
     &       apressurestartdate1, apressurestartdate2,
     &       tidePotStartdate1, tidePotStartdate2,
     &       areamaskstartdate1,  areamaskstartdate2,
     &       obcsNstartdate1,   obcsNstartdate2,
     &       obcsSstartdate1,   obcsSstartdate2,
     &       obcsEstartdate1,   obcsEstartdate2,
     &       obcsWstartdate1,   obcsWstartdate2,
     &       siobNstartdate1,   siobNstartdate2,
     &       siobSstartdate1,   siobSstartdate2,
     &       siobEstartdate1,   siobEstartdate2,
     &       siobWstartdate1,   siobWstartdate2

      COMMON /EXF_PARAM_R/
     &       repeatPeriod,      exf_monFreq,
     &       exf_scal_BulkCdn,  windstressmax,
     &       hfluxconst,        hfluxRepCycle,
     &       hfluxperiod,       hfluxStartTime,
     &       atempconst,        atempRepCycle,
     &       atempperiod,       atempStartTime,
     &       aqhconst,          aqhRepCycle,
     &       aqhperiod,         aqhStartTime,
     &       hs_const,          hs_RepCycle,
     &       hs_period,         hs_StartTime,
     &       hl_const,          hl_RepCycle,
     &       hl_period,         hl_StartTime,
     &       sfluxconst,        sfluxRepCycle,
     &       sfluxperiod,       sfluxStartTime,
     &       evapconst,         evapRepCycle,
     &       evapperiod,        evapStartTime,
     &       precipconst,       precipRepCycle,
     &       precipperiod,      precipStartTime,
     &       snowprecipconst,   snowprecipRepCycle,
     &       snowprecipperiod,  snowprecipStartTime,
     &       runoffconst,       runoffRepCycle,
     &       runoffperiod,      runoffStartTime,
     &       runoftempconst,
     &       saltflxconst,      saltflxRepCycle,
     &       saltflxperiod,     saltflxStartTime,
     &       ustressconst,      ustressRepCycle,
     &       ustressperiod,     ustressStartTime,
     &       vstressconst,      vstressRepCycle,
     &       vstressperiod,     vstressStartTime,
     &       uwindconst,        uwindRepCycle,
     &       uwindperiod,       uwindStartTime,
     &       vwindconst,        vwindRepCycle,
     &       vwindperiod,       vwindStartTime,
     &       wspeedconst,       wspeedRepCycle,
     &       wspeedperiod,      wspeedStartTime,
     &       swfluxconst,       swfluxRepCycle,
     &       swfluxperiod,      swfluxStartTime,
     &       lwfluxconst,       lwfluxRepCycle,
     &       lwfluxperiod,      lwfluxStartTime,
     &       swdownconst,       swdownRepCycle,
     &       swdownperiod,      swdownStartTime,
     &       lwdownconst,       lwdownRepCycle,
     &       lwdownperiod,      lwdownStartTime,
     &       apressureconst,    apressureRepCycle,
     &       apressureperiod,   apressureStartTime,
     &       tidePotConst,      tidePotRepCycle,
     &       tidePotPeriod,     tidePotStartTime,
     &       areamaskconst,     areamaskRepCycle,
     &       areamaskperiod,    areamaskStartTime,
     &       obcsNrepCycle,     obcsNperiod,     obcsNstartTime,
     &       obcsSrepCycle,     obcsSperiod,     obcsSstartTime,
     &       obcsErepCycle,     obcsEperiod,     obcsEstartTime,
     &       obcsWrepCycle,     obcsWperiod,     obcsWstartTime,
     &       siobNrepCycle,     siobNperiod,     siobNstartTime,
     &       siobSrepCycle,     siobSperiod,     siobSstartTime,
     &       siobErepCycle,     siobEperiod,     siobEstartTime,
     &       siobWrepCycle,     siobWperiod,     siobWstartTime

      COMMON /EXF_PARAM_TREND_REMOVAL/
     &       hflux_exfremo_intercept,
     &       atemp_exfremo_intercept,
     &       aqh_exfremo_intercept,
     &       hs_exfremo_intercept,
     &       hl_exfremo_intercept,
     &       sflux_exfremo_intercept,
     &       evap_exfremo_intercept,
     &       precip_exfremo_intercept,
     &       snowprecip_exfremo_intercept,
     &       runoff_exfremo_intercept,
     &       runoftemp_exfremo_intercept,
     &       saltflx_exfremo_intercept,
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
     &       tidePot_exfremo_intercept,
     &       areamask_exfremo_intercept,
     &       hflux_exfremo_slope,
     &       atemp_exfremo_slope,
     &       aqh_exfremo_slope,
     &       hs_exfremo_slope,
     &       hl_exfremo_slope,
     &       sflux_exfremo_slope,
     &       evap_exfremo_slope,
     &       precip_exfremo_slope,
     &       snowprecip_exfremo_slope,
     &       runoff_exfremo_slope,
     &       runoftemp_exfremo_slope,
     &       saltflx_exfremo_slope,
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
     &       tidePot_exfremo_slope,
     &       areamask_exfremo_slope

      COMMON /EXF_PARAM_C/
     &       hfluxfile,     hfluxmask,
     &       atempfile,     atempmask,
     &       aqhfile,       aqhmask,
     &       hs_file,       hs_mask,
     &       hl_file,       hl_mask,
     &       sfluxfile,     sfluxmask,
     &       evapfile,      evapmask,
     &       precipfile,    precipmask,
     &       snowprecipfile,snowprecipmask,
     &       runofffile,    runoffmask,
     &       runoftempfile,
     &       saltflxfile,   saltflxmask,
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
     &       tidePotFile,   tidePotMask,
     &       areamaskfile,  areamaskmask

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
     &       climsstconst,       climsstRepCycle,
     &       climsstperiod,      climsstStartTime,
     &       climsssconst,       climsssRepCycle,
     &       climsssperiod,      climsssStartTime,
     &       climustrconst,      climustrRepCycle,
     &       climustrperiod,     climustrStartTime,
     &       climvstrconst,      climvstrRepCycle,
     &       climvstrperiod,     climvstrStartTime,
     &       climsstTauRelax,    climsssTauRelax,
     &       climustrTauRelax,   climvstrTauRelax,
     &       areamaskTauRelax,
     &       climsst_exfremo_intercept, climsst_exfremo_slope,
     &       climsss_exfremo_intercept, climsss_exfremo_slope,
     &       climustr_exfremo_intercept, climustr_exfremo_slope,
     &       climvstr_exfremo_intercept, climvstr_exfremo_slope,
     &       exf_inscal_climsst, exf_inscal_climsss,
     &       exf_inscal_climustr, exf_inscal_climvstr

C     file precision and field type

      COMMON /EXF_PARAM_TYPE/
     &       exf_iprec,
     &       exf_iprec_obcs

      INTEGER exf_iprec
      INTEGER exf_iprec_obcs

C-    Scaling factors:
C     exf_inscal_{fld}   :: input scaling factors
C     exf_offset_atemp   :: input air temperature offset
C                        :: (for conversion from C to K, if needed)
C     exf_outscale_{fld} :: output scaling factors

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
c     _RL     exf_inscal_sst
c     _RL     exf_inscal_sss
      _RL     exf_inscal_atemp, exf_offset_atemp
      _RL     exf_inscal_aqh
      _RL     exf_inscal_hs
      _RL     exf_inscal_hl
      _RL     exf_inscal_evap
      _RL     exf_inscal_apressure
      _RL     exf_inscal_runoff
      _RL     exf_inscal_runoftemp
      _RL     exf_inscal_saltflx
      _RL     exf_inscal_swdown
      _RL     exf_inscal_lwdown
      _RL     exf_inscal_tidePot
      _RL     exf_inscal_areamask
      _RL     exf_inscal_climsst
      _RL     exf_inscal_climsss
      _RL     exf_inscal_climustr
      _RL     exf_inscal_climvstr

      _RL     exf_outscal_hflux
      _RL     exf_outscal_sflux
      _RL     exf_outscal_ustress
      _RL     exf_outscal_vstress
      _RL     exf_outscal_swflux
      _RL     exf_outscal_sst
      _RL     exf_outscal_sss
      _RL     exf_outscal_apressure
      _RL     exf_outscal_tidePot
      _RL     exf_outscal_areamask

      COMMON /EXF_PARAM_SCAL/
     &                      exf_inscal_hflux,
     &                      exf_inscal_sflux,
     &                      exf_inscal_ustress,
     &                      exf_inscal_vstress,
     &                      exf_inscal_uwind,
     &                      exf_inscal_vwind,
     &                      exf_inscal_wspeed,
     &                      exf_inscal_swflux,
     &                      exf_inscal_lwflux,
     &                      exf_inscal_precip,
     &                      exf_inscal_snowprecip,
c    &                      exf_inscal_sst,
c    &                      exf_inscal_sss,
     &                      exf_inscal_atemp, exf_offset_atemp,
     &                      exf_inscal_aqh,
     &                      exf_inscal_hs,
     &                      exf_inscal_hl,
     &                      exf_inscal_evap,
     &                      exf_inscal_apressure,
     &                      exf_inscal_runoff,
     &                      exf_inscal_runoftemp,
     &                      exf_inscal_saltflx,
     &                      exf_inscal_swdown,
     &                      exf_inscal_lwdown,
     &                      exf_inscal_tidePot,
     &                      exf_inscal_areamask,
     &                      exf_outscal_hflux,
     &                      exf_outscal_sflux,
     &                      exf_outscal_ustress,
     &                      exf_outscal_vstress,
     &                      exf_outscal_swflux,
     &                      exf_outscal_sst,
     &                      exf_outscal_sss,
     &                      exf_outscal_apressure,
     &                      exf_outscal_tidePot,
     &                      exf_outscal_areamask

#ifndef USE_EXF_INTERPOLATION
C-  Set dummy dimension to 1
      INTEGER MAX_LAT_INC
      PARAMETER(MAX_LAT_INC = 1)
#else /* USE_EXF_INTERPOLATION */
C-  To read input data without dynamical allocation (INTERP_USE_DYNALLOC undef):
C   Note: exf_interp_bufferSize has been moved to EXF_INTERP_SIZE.h
c     INTEGER    exf_interp_bufferSize
c     PARAMETER( exf_interp_bufferSize = 140000 )

C-- For lat interpolation, arraysize currently set to 1279 max data values
C   to accomodate ECMWF operational analysis
      INTEGER MAX_LAT_INC
      PARAMETER(MAX_LAT_INC = 1279)

C-- Interpolation parameters (for each input field):
C   {inputField}_lon0    :: longitude of the 1rst point (South-East corner)
C   {inputField}_lon_inc :: longitude increment (uniform)
C   {inputField}_lat0    :: latitude  of the 1rst point (South-East corner)
C   {inputField}_lat_inc :: latitude  increment (vector, fct of latitude only)
C   {inputField}_nlon    :: input filed 1rst dim, longitudinal direction
C   {inputField}_nlat    :: input filed 2nd  dim, latitudinal  direction
C   {inputField}_interpMethod :: interpolation method: =0 : no interpolation ;
C                             :: =1,11,21 : bilinear ; =2,12,22 : bicubic ;
C                             :: =1,2 for tracer ; =11,12 for U ; =21,22 for V.
C-  Global parameters (for all fields):
C    exf_output_interp   :: output directly interpolation result (before
C                           rescaling, rotation or time-interp)
C-  Internal parameters, for 2 components vector field:
C    uvInterp_stress     :: interpolate wind-stress u & v components together
C    uvInterp_wind       :: interpolate wind        u & v components together
C    uvInterp_climstr    :: interpolate clim stress u & v components together
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
      _RL saltflx_lon0, saltflx_lon_inc
      _RL saltflx_lat0, saltflx_lat_inc(MAX_LAT_INC)
      INTEGER saltflx_nlon, saltflx_nlat, saltflx_interpMethod
      _RL atemp_lon0, atemp_lon_inc
      _RL atemp_lat0, atemp_lat_inc(MAX_LAT_INC)
      INTEGER atemp_nlon, atemp_nlat, atemp_interpMethod
      _RL aqh_lon0, aqh_lon_inc
      _RL aqh_lat0, aqh_lat_inc(MAX_LAT_INC)
      INTEGER aqh_nlon, aqh_nlat, aqh_interpMethod
      _RL hs_lon0, hs_lon_inc
      _RL hs_lat0, hs_lat_inc(MAX_LAT_INC)
      INTEGER hs_nlon, hs_nlat, hs_interpMethod
      _RL hl_lon0, hl_lon_inc
      _RL hl_lat0, hl_lat_inc(MAX_LAT_INC)
      INTEGER hl_nlon, hl_nlat, hl_interpMethod
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
      _RL tidePot_lon0,tidePot_lon_inc
      _RL tidePot_lat0,tidePot_lat_inc(MAX_LAT_INC)
      INTEGER tidePot_nlon,tidePot_nlat,tidePot_interpMethod
      _RL areamask_lon0,areamask_lon_inc
      _RL areamask_lat0,areamask_lat_inc(MAX_LAT_INC)
      INTEGER areamask_nlon,areamask_nlat,areamask_interpMethod

      LOGICAL exf_output_interp
      LOGICAL uvInterp_stress
      LOGICAL uvInterp_wind
      LOGICAL uvInterp_climstr
      COMMON /EXF_INTERPOLATION_L/
     & exf_output_interp,
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
     & saltflx_lon0, saltflx_lon_inc,
     & saltflx_lat0, saltflx_lat_inc,
     & atemp_lon0, atemp_lon_inc,
     & atemp_lat0, atemp_lat_inc,
     & aqh_lon0, aqh_lon_inc,
     & aqh_lat0, aqh_lat_inc,
     & hs_lon0, hs_lon_inc,
     & hs_lat0, hs_lat_inc,
     & hl_lon0, hl_lon_inc,
     & hl_lat0, hl_lat_inc,
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
     & apressure_lon0, apressure_lon_inc,
     & apressure_lat0, apressure_lat_inc,
     & tidePot_lon0, tidePot_lon_inc,
     & tidePot_lat0, tidePot_lat_inc,
     & areamask_lon0, areamask_lon_inc,
     & areamask_lat0, areamask_lat_inc

      COMMON /EXF_INTERPOLATION_I/
     & ustress_nlon, ustress_nlat, ustress_interpMethod,
     & vstress_nlon, vstress_nlat, vstress_interpMethod,
     & hflux_nlon, hflux_nlat, hflux_interpMethod,
     & sflux_nlon, sflux_nlat, sflux_interpMethod,
     & swflux_nlon, swflux_nlat, swflux_interpMethod,
     & runoff_nlon, runoff_nlat, runoff_interpMethod,
     & saltflx_nlon, saltflx_nlat, saltflx_interpMethod,
     & atemp_nlon, atemp_nlat, atemp_interpMethod,
     & aqh_nlon, aqh_nlat, aqh_interpMethod,
     & hs_nlon, hs_nlat, hs_interpMethod,
     & hl_nlon, hl_nlat, hl_interpMethod,
     & evap_nlon, evap_nlat, evap_interpMethod,
     & precip_nlon, precip_nlat, precip_interpMethod,
     & snowprecip_nlon, snowprecip_nlat, snowprecip_interpMethod,
     & uwind_nlon, uwind_nlat, uwind_interpMethod,
     & vwind_nlon, vwind_nlat, vwind_interpMethod,
     & wspeed_nlon, wspeed_nlat, wspeed_interpMethod,
     & lwflux_nlon, lwflux_nlat, lwflux_interpMethod,
     & swdown_nlon, swdown_nlat, swdown_interpMethod,
     & lwdown_nlon, lwdown_nlat, lwdown_interpMethod,
     & apressure_nlon, apressure_nlat, apressure_interpMethod,
     & tidePot_nlon, tidePot_nlat, tidePot_interpMethod,
     & areamask_nlon, areamask_nlat, areamask_interpMethod

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
