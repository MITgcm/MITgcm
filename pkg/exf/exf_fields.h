c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_fields.h,v 1.2 2002/01/11 19:24:24 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exf_fields
c     ==================================================================
c
c     o Header file for the surface flux data.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c
c     ==================================================================
c     HEADER exf_fields
c     ==================================================================

c     Model fields.

c     heat and salt flux.
      common /exf_hsflux_r/ hflux, sflux
      _RL hflux (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL sflux (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)

c     zonal and meridionalwind stress.
      common /exf_stress_r/ ustress, vstress
      _RL ustress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_ATM_TEMP
c     Use the atmospheric temperature and specific humidity for flux
c     estimates.

c     Atmospheric temperature.
      _RL atemp (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)

c     Atmospheric specific humidity.
      _RL aqh (1-olx:snx+olx,1-oly:sny+oly,    nsx,nsy)

c     Long wave radiative flux.
      _RL lwflux (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

c     Precipitation.
      _RL precip (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

      common /exf_atm_temp_r/ atemp, aqh, lwflux, precip

c     Short wave radiative flux.
      common /exf_swflux_r/ swflux
      _RL swflux (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

#else
c     Do not use the atmospheric temperature and specific humidity for
c     flux estimates but given fluxes.

#ifdef ALLOW_KPP
c     Short wave radiative flux.
      common /exf_swflux_r/ swflux
      _RL swflux (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

#endif

#endif /* ALLOW_ATM_TEMP */

#ifdef ALLOW_ATM_WIND
c     Use the atmospheric winds for flux estimates.

c     Atmospheric zonal and meridional wind
      common /exf_atm_wind_r/ uwind, vwind
      _RL uwind (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL vwind (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)

#else
c     Do not use the atmospheric winds for flux estimates but given 
c     wind stresses.

#endif  /* ALLOW_ATM_WIND */

c--   define auxiliary fields for temporal interpolation

#ifdef ALLOW_ATM_TEMP

      common /exfl_aqh_r/ aqh0, aqh1
      _RL aqh0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL aqh1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_atemp_r/ atemp0, atemp1
      _RL atemp0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL atemp1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_precip_r/ precip0, precip1
      _RL precip0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL precip1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_lwflux_r/ lwflux0, lwflux1
      _RL lwflux0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lwflux1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_swflux_r/ swflux0, swflux1
      _RL swflux0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL swflux1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#else
      common /exfl_hflux_r/ hflux0, hflux1
      _RL hflux0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL hflux1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_sflux_r/ sflux0, sflux1
      _RL sflux0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sflux1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_KPP
      common /exfl_swflux_r/ swflux0, swflux1
      _RL swflux0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL swflux1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
#endif

#ifdef ALLOW_ATM_WIND

      common /exfl_uwind_r/ uwind0, uwind1
      _RL uwind0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL uwind1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_vwind_r/ vwind0, vwind1
      _RL vwind0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vwind1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#else
      common /exfl_ustress_r/ ustress0, ustress1
      _RL ustress0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL ustress1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfl_vstress_r/ vstress0, vstress1
      _RL vstress0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
