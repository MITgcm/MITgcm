c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_init.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

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

      integer i

c     == end of interface ==

      call exf_init_hflux  ( mythid )
      call exf_init_sflux  ( mythid )

      call exf_init_ustress( mythid )
      call exf_init_vstress( mythid )

#ifdef ALLOW_BULKFORMULAE
c     Use bulk formulae estimates of the surface fluxes.

#ifdef ALLOW_ATM_TEMP

c     Use the atmospheric temperature and specific humidity for flux
c     estimates.
      call exf_init_atemp ( mythid )
      call exf_init_aqh   ( mythid )
      call exf_init_lwflux( mythid )
      call exf_init_swflux( mythid )
      call exf_init_precip( mythid )

#else /* ALLOW_ATM_TEMP undefined */   

#ifdef ALLOW_KPP
      call exf_init_swflux( mythid )
#endif /* ALLOW_KPP */

#endif /* ALLOW_ATM_TEMP */

#ifdef ALLOW_ATM_WIND 
      
c     Use atmospheric wind fields
      call exf_init_uwind( mythid )
      call exf_init_vwind( mythid )

#endif /* ALLOW_ATM_WIND */

#else /* ALLOW_BULKFORMULAE undefined */

#ifdef ALLOW_KPP
      call exf_init_swflux( mythid )
#endif /* ALLOW_KPP */

#endif /* ALLOW_BULKFORMULAE */

c     Check for climatological contributions
      call exf_clim_init( mythid )

      end
