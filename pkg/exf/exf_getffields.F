c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffields.F,v 1.13 2003/05/23 18:37:31 heimbach Exp $

#include "EXF_CPPOPTIONS.h"

      subroutine exf_getffields( mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_getffields
c     ==================================================================
c
c     o Read-in atmospheric state and/or surface fluxes from files.
c
c       heimbach@mit.edu, 23-May-2003 totally re-structured
c
c     ==================================================================
c     SUBROUTINE exf_getffields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

c     == routine arguments ==

      integer mythid
      integer mycurrentiter
      _RL     mycurrenttime

c     == local variables ==

c     == end of interface ==

c--   read forcing fields from files and temporal interpolation

#ifdef ALLOW_ATM_WIND

c     Zonal wind.
cgen      call exf_set_uwind( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen( 
     &     uwindfile, uwindstartdate, uwindperiod, 
     &     exf_inscal_uwind,
     &     uwind, uwind0, uwind1, uwindmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Meridional wind.
cgen      call exf_set_vwind( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen( 
     &     vwindfile, vwindstartdate, vwindperiod, 
     &     exf_inscal_vwind,
     &     vwind, vwind0, vwind1, vwindmask, 
     &     mycurrenttime, mycurrentiter, mythid )

#ifdef ALLOW_UWIND_CONTROL
      call ctrl_getuwind  ( mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_VWIND_CONTROL
      call ctrl_getvwind  ( mycurrenttime, mycurrentiter, mythid )
#endif

#else  /* ifndef ALLOW_ATM_WIND */

c     Zonal wind stress.
      call exf_set_gen( 
     &     ustressfile, ustressstartdate, ustressperiod, 
     &     exf_inscal_ustress,
     &     ustress, ustress0, ustress1, ustressmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Meridional wind stress.
      call exf_set_gen( 
     &     vstressfile, vstressstartdate, vstressperiod, 
     &     exf_inscal_vstress,
     &     vstress, vstress0, vstress1, vstressmask, 
     &     mycurrenttime, mycurrentiter, mythid )

#endif /* ifndef ALLOW_ATM_WIND */

#ifdef ALLOW_ATM_TEMP

c     Atmospheric temperature.
cgen      call exf_set_atemp ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen( 
     &     atempfile, atempstartdate, atempperiod, 
     &     exf_inscal_atemp,
     &     atemp, atemp0, atemp1, atempmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Atmospheric humidity.
cgen      call exf_set_aqh    ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen( 
     &     aqhfile, aqhstartdate, aqhperiod, 
     &     exf_inscal_aqh,
     &     aqh, aqh0, aqh1, aqhmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Net long wave radiative flux.
cgen      call exf_set_lwflux ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen( 
     &     lwfluxfile, lwfluxstartdate, lwfluxperiod, 
     &     exf_inscal_lwflux,
     &     lwflux, lwflux0, lwflux1, lwfluxmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Precipitation.
cgen      call exf_set_precip ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen( 
     &     precipfile, precipstartdate, precipperiod, 
     &     exf_inscal_precip,
     &     precip, precip0, precip1, precipmask, 
     &     mycurrenttime, mycurrentiter, mythid )

#ifdef ALLOW_ATEMP_CONTROL
      call ctrl_getatemp  ( mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_AQH_CONTROL
      call ctrl_getaqh    ( mycurrenttime, mycurrentiter, mythid )
#endif

#else  /* ifndef ALLOW_ATM_TEMP */

c     Atmospheric heat flux.
      call exf_set_gen  ( 
     &     hfluxfile, hfluxstartdate, hfluxperiod, exf_inscal_hflux,
     &     hflux, hflux0, hflux1, hfluxmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Salt flux.
      call exf_set_gen  ( 
     &     sfluxfile, sfluxstartdate, sfluxperiod, exf_inscal_sflux,
     &     sflux, sflux0, sflux1, sfluxmask, 
     &     mycurrenttime, mycurrentiter, mythid )

#endif /* ifndef ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
c     Net short wave radiative flux.
cgen      call exf_set_swflux ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen  ( 
     &     swfluxfile, swfluxstartdate, swfluxperiod, exf_inscal_swflux,
     &     swflux, swflux0, swflux1, swfluxmask, 
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef EXF_READ_EVAP
c     Evaporation
cgen      call exf_set_evap   ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen  ( 
     &     evapfile, evapstartdate, evapperiod, exf_inscal_evap,
     &     evap, evap0, evap1, evapmask, 
     &     mycurrenttime, mycurrentiter, mythid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION

c     Downward shortwave radiation.
cgen      call exf_set_swdown ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen  ( 
     &     swdownfile, swdownstartdate, swdownperiod, exf_inscal_swdown,
     &     swdown, swdown0, swdown1, swdownmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     Downward longwave radiation.
cgen      call exf_set_lwdown ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen  ( 
     &     lwdownfile, lwdownstartdate, lwdownperiod, exf_inscal_lwdown,
     &     lwdown, lwdown0, lwdown1, lwdownmask, 
     &     mycurrenttime, mycurrentiter, mythid )

#endif

#ifdef ATMOSPHERIC_LOADING
c     Atmos. pressure forcing
cgen      call exf_set_apressure ( mycurrenttime, mycurrentiter, mythid )
      call exf_set_gen  ( 
     &     apressurefile, apressurestartdate, apressureperiod, 
     &     exf_inscal_apressure,
     &     apressure, apressure0, apressure1, apressuremask, 
     &     mycurrenttime, mycurrentiter, mythid )
#endif


      end
