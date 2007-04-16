C $Header: /u/gcmpack/MITgcm/pkg/exf/EXF_FIELDS.h,v 1.1 2007/04/16 23:27:20 jmc Exp $
C $Name:  $
c
c
c     ==================================================================
c     HEADER exf_fields
c     ==================================================================
c
c     o Header file for the surface flux data.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: field swap in adj. mode; heimbach@mit.edu 10-Jan-2002
c     included runoff D. Stammer, Nov. 25, 2001
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     HEADER exf_fields
c     ==================================================================
c
c
c     Field definitions, units, and sign conventions:
c     ===============================================
c
c     ustress   :: Zonal surface wind stress in N/m^2
c                  > 0 for increase in uVel, which is west to
c                      east for cartesian and spherical polar grids
c                  Typical range: -0.5 < ustress < 0.5
c                  Input field
c
c     vstress   :: Meridional surface wind stress in N/m^2
c                  > 0 for increase in vVel, which is south to
c                      north for cartesian and spherical polar grids
c                  Typical range: -0.5 < vstress < 0.5
c                  Input field
c
c     hflux     :: Net upward surface heat flux excluding shortwave in W/m^2
c                  hflux = latent + sensible + lwflux
c                  > 0 for decrease in theta (ocean cooling)
c                  Typical range: -250 < hflux < 600
c                  Input field
c
c     sflux     :: Net upward freshwater flux in m/s
c                  sflux = evap - precip - runoff
c                  > 0 for increase in salt (ocean salinity)
c                  Typical range: -1e-7 < sflux < 1e-7
c                  Input field
c
c     swflux    :: Net upward shortwave radiation in W/m^2
c                  swflux = - ( swdown - ice and snow absorption - reflected )
c                  > 0 for decrease in theta (ocean cooling)
c                  Typical range: -350 < swflux < 0
c                  Input field
c
c     uwind     :: Surface (10-m) zonal wind velocity in m/s
c                  > 0 for increase in uVel, which is west to
c                      east for cartesian and spherical polar grids
c                  Typical range: -10 < uwind < 10
c                  Input or input/output field
c
c     vwind     :: Surface (10-m) meridional wind velocity in m/s
c                  > 0 for increase in vVel, which is south to
c                      north for cartesian and spherical polar grids
c                  Typical range: -10 < vwind < 10
c                  Input or input/output field
c
c     wspeed    :: Surface (10-m) wind speed in m/s
c                  >= 0 sqrt(u^2+v^2)
c                  Typical range: 0 < wspeed < 10
c                  Input or input/output field
c
c     atemp     :: Surface (2-m) air temperature in deg K
c                  Typical range: 200 < atemp < 300
c                  Input or input/output field
c
c     aqh       :: Surface (2m) specific humidity in kg/kg
c                  Typical range: 0 < aqh < 0.02
c                  Input or input/output field
c
c     lwflux    :: Net upward longwave radiation in W/m^2
c                  lwflux = - ( lwdown - ice and snow absorption - emitted )
c                  > 0 for decrease in theta (ocean cooling)
c                  Typical range: -20 < lwflux < 170
c                  Input field
c
c     evap      :: Evaporation in m/s
c                  > 0 for increase in salt (ocean salinity)
c                  Typical range: 0 < evap < 2.5e-7
c                  Input, input/output, or output field
c
c     precip    :: Precipitation in m/s
c                  > 0 for decrease in salt (ocean salinity)
c                  Typical range: 0 < precip < 5e-7
c                  Input or input/output field
c
c     snowprecip :: snow in m/s
c                  > 0 for decrease in salt (ocean salinity)
c                  Typical range: 0 < precip < 5e-7
c                  Input or input/output field
c
c     runoff    :: River and glacier runoff in m/s
c                  > 0 for decrease in salt (ocean salinity)
c                  Typical range: 0 < runoff < ????
c                  Input or input/output field
c
c     swdown    :: Downward shortwave radiation in W/m^2
c                  > 0 for increase in theta (ocean warming)
c                  Typical range: 0 < swdown < 450
c                  Input/output field
c
c     lwdown    :: Downward longwave radiation in W/m^2
c                  > 0 for increase in theta (ocean warming)
c                  Typical range: 50 < lwdown < 450
c                  Input/output field
c
c     apressure :: Atmospheric pressure field in N/m^2
c                  > 0 for ????
c                  Typical range: ???? < apressure < ????
c                  Input field
c
c     hs        :: sensible heat flux into ocean in W/m^2
c                  > 0 for increase in theta (ocean warming)
c
c     hl        :: latent   heat flux into ocean in W/m^2
c                  > 0 for increase in theta (ocean warming)
c
c
c     NOTES:
c     ======
c
c     All surface forcing fields are defined at the center of
c     each grid (the rVel location in model/inc/GRID.h) with
c     one exception.  When both ALLOW_BULKFORMULAE and
c     USE_EXF_INTERPOLATION are undefined, ustress and vstress are
c     defined at the Southwest C-grid U and V points, respectively.
c
c     Input and output units and sign conventions can be customized
c     using variables exf_inscal_* and exf_outscal_*, which are set
c     by exf_readparms.F
c
c     Output fields fu, fv, Qnet, Qsw, and EmPmR are
c     defined in FFIELDS.h
c
c     #ifndef SHORTWAVE_HEATING, hflux includes shortwave,
c     that is, hflux = latent + sensible + lwflux +swflux
c
c     Arrays *0 and *1 below are used for temporal interpolation.
c

      common /exf_stress_r/ ustress, vstress
      _RL ustress   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_ustress_r/ ustress0, ustress1
      _RL ustress0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL ustress1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_vstress_r/ vstress0, vstress1
      _RL vstress0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_wspeed_r/ wspeed
      _RL wspeed   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_wspeed_r/ wspeed0, wspeed1
      _RL wspeed0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wspeed1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_atm_wind_r/ uwind, vwind
      _RL uwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_uwind_r/ uwind0, uwind1
      _RL uwind0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL uwind1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_vwind_r/ vwind0, vwind1
      _RL vwind0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vwind1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_hsflux_r/ hflux, sflux
      _RL hflux     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sflux     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_hflux_r/ hflux0, hflux1
      _RL hflux0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL hflux1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_sflux_r/ sflux0, sflux1
      _RL sflux0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sflux1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_ATM_TEMP
      common /exf_atm_temp_r/ atemp, aqh, lwflux, precip, snowprecip
      _RL atemp     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL aqh       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lwflux    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL precip    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL snowprecip (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_atemp_r/ atemp0, atemp1
      _RL atemp0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL atemp1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_aqh_r/ aqh0, aqh1
      _RL aqh0      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL aqh1      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_lwflux_r/ lwflux0, lwflux1
      _RL lwflux0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lwflux1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_precip_r/ precip0, precip1
      _RL precip0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL precip1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_snowprecip_r/ snowprecip0, snowprecip1
      _RL snowprecip0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL snowprecip1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_turb_r/ hs, hl
      _RL hs        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL hl        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

      common /exfl_wind_r/ us, cw, sw, sh
      _RL us        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL cw        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sw        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sh        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      common /exf_swflux_r/ swflux
      _RL swflux    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_swflux_r/ swflux0, swflux1
      _RL swflux0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL swflux1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
      common /exf_evap/ evap
      _RL evap      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_evap_r/ evap0, evap1
      _RL evap0     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL evap1     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      common /exf_rad_down_r/
     &     swdown, lwdown, swdown0, swdown1, lwdown0, lwdown1
      _RL swdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lwdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL swdown0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL swdown1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lwdown0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lwdown1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ATMOSPHERIC_LOADING
      common /exf_apressure_r/ apressure, apressure0, apressure1
      _RL apressure (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL apressure0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL apressure1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#if defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE)
      common /exfl_runoff_r/ runoff, runoff0, runoff1
      _RL runoff    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL runoff0   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL runoff1   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
