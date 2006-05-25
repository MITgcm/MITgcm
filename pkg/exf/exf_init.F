c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_init.F,v 1.10 2006/05/25 18:32:56 heimbach Exp $

#include "EXF_OPTIONS.h"


      subroutine exf_init( mythid )

c     ==================================================================
c     SUBROUTINE exf_init
c     ==================================================================
c
c     o This routine initialises the forcing
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-20000
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "exf.h"
#include "exf_param.h"
#include "exf_fields.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == end of interface ==

      call exf_init_gen (
     &     ustressconst, ustress, ustress0, ustress1, mythid )
      call exf_init_gen (
     &     vstressconst, vstress, vstress0, vstress1, mythid )

#ifdef ALLOW_ATM_WIND 
      call exf_init_gen (
     &     uwindconst, uwind, uwind0, uwind1, mythid )
      call exf_init_gen (
     &     vwindconst, vwind, vwind0, vwind1, mythid )
#endif

      call exf_init_gen (
     &     wspeedconst, wspeed, wspeed0, wspeed1, mythid )

      call exf_init_gen (
     &     hfluxconst, hflux, hflux0, hflux1, mythid )
      call exf_init_gen (
     &     sfluxconst, sflux, sflux0, sflux1, mythid )

#ifdef ALLOW_ATM_TEMP
      call exf_init_gen (
     &     atempconst, atemp, atemp0, atemp1, mythid )
      call exf_init_gen (
     &     aqhconst, aqh, aqh0, aqh1, mythid )
      call exf_init_gen (
     &     lwfluxconst, lwflux, lwflux0, lwflux1, mythid )
      call exf_init_gen (
     &     precipconst, precip, precip0, precip1, mythid )
      call exf_init_gen (
     &     snowprecipconst, snowprecip, snowprecip0, snowprecip1, 
     &     mythid )
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      call exf_init_gen (
     &     swfluxconst, swflux, swflux0, swflux1, mythid )
#endif

#if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
      call exf_init_gen (
     &     evapconst, evap, evap0, evap1, mythid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      call exf_init_gen (
     &     swdownconst, swdown, swdown0, swdown1, mythid )
      call exf_init_gen (
     &     lwdownconst, lwdown, lwdown0, lwdown1, mythid )
#endif

#ifdef ATMOSPHERIC_LOADING
      call exf_init_gen (
     &     apressureconst, apressure, apressure0, apressure1, mythid )
#endif

#if (defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE))
      call exf_init_runoff   ( mythid )
#endif

c     Initialize climatological fields
      call exf_clim_init     ( mythid )

      end
