c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_init.F,v 1.6 2003/03/06 00:47:33 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_init(
     I                     mythid
     &                   )

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

c     == routine arguments ==

      integer mythid

c     == local variables ==

c     == end of interface ==

      call exf_init_ustress  ( mythid )
      call exf_init_vstress  ( mythid )

#ifdef ALLOW_ATM_WIND 
      call exf_init_uwind    ( mythid )
      call exf_init_vwind    ( mythid )
#endif

      call exf_init_hflux    ( mythid )
      call exf_init_sflux    ( mythid )

#ifdef ALLOW_ATM_TEMP
      call exf_init_atemp    ( mythid )
      call exf_init_aqh      ( mythid )
      call exf_init_lwflux   ( mythid )
      call exf_init_precip   ( mythid )
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      call exf_init_swflux   ( mythid )
#endif

#if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
      call exf_init_evap     ( mythid )
#endif

#if (defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE))
      call exf_init_runoff   ( mythid )
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      call exf_init_swdown   ( mythid )
      call exf_init_lwdown   ( mythid )
#endif

#ifdef ATMOSPHERIC_LOADING
      call exf_init_apressure( mythid )
#endif

c     Initialize climatological fields
      call exf_clim_init     ( mythid )

      end
