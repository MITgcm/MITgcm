c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_fields.h,v 1.1 2001/05/14 22:08:40 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exf_fields
c     ==================================================================
c
c     o Header file for the surface flux data.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     HEADER exf_fields
c     ==================================================================

c     Model fields.

c     heat and salt flux.
      _RL hflux (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL sflux (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)

      common /exf_hsflux_r/ hflux, sflux

c     zonal and meridionalwind stress.
      _RL ustress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_stress_r/ ustress, vstress

#ifdef ALLOW_BULKFORMULAE
c     Use bulk formulae estimates of the surface fluxes.

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
      _RL swflux (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

      common /exf_swflux_r/ swflux

#else
c     Do not use the atmospheric temperature and specific humidity for
c     flux estimates but given fluxes.

#ifdef ALLOW_KPP
c     Short wave radiative flux.
      _RL swflux (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

      common /exf_swflux_r/ swflux

#endif
#endif

#ifdef ALLOW_ATM_WIND
c     Use the atmospheric winds for flux estimates.

c     Atmospheric zonal and meridional wind
      _RL uwind (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL vwind (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)

      common /exf_atm_wind_r/ uwind, vwind
#else
c     Do not use the atmospheric winds for flux estimates but given 
c     wind stresses.

#endif

#else
c     Use given surface fluxes (heat flux and wind stress)

#ifdef ALLOW_KPP
c     Allow for the use of the KPP mixed layer scheme.
c     Short wave radiative flux.
      _RL swflux (1-olx:snx+olx,1-oly:sny+oly, nsx,nsy)

      common /exf_swflux_r/ swflux

#endif

#endif


