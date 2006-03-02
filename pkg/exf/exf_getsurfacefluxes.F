C
C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getsurfacefluxes.F,v 1.11 2006/03/02 02:53:22 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_GetSurfaceFluxes(
     I                                 mytime,
     I                                 myiter,
     I                                 mythid
     &                               )

c     ==================================================================
c     SUBROUTINE exf_GetSurfaceFluxes
c     ==================================================================
c
c     o Mid-level routine for enabling the use of flux fields as control
c       variables.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_GetSurfaceFluxes
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "exf_fields.h"
#ifdef ALLOW_AUTODIFF
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      _RL mytime
      integer myiter
      integer mythid

c     == global variables ==

c     == end of interface ==

#ifdef ALLOW_HFLUX_CONTROL
      call ctrl_get_gen ( 
     &     xx_hflux_file, xx_hfluxstartdate, xx_hfluxperiod,
     &     maskc, hflux, xx_hflux0, xx_hflux1, xx_hflux_dummy,
     &     xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_SFLUX_CONTROL
      call ctrl_get_gen ( 
     &     xx_sflux_file, xx_sfluxstartdate, xx_sfluxperiod,
     &     maskc, sflux, xx_sflux0, xx_sflux1, xx_sflux_dummy,
     &     xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_USTRESS_CONTROL
      call ctrl_get_gen ( 
     &     xx_tauu_file, xx_tauustartdate, xx_tauuperiod,
     &     maskw, ustress, xx_tauu0, xx_tauu1, xx_tauu_dummy,
     &     xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &     mytime, myiter, mythid )
#endif

#ifdef ALLOW_VSTRESS_CONTROL
      call ctrl_get_gen ( 
     &     xx_tauv_file, xx_tauvstartdate, xx_tauvperiod,
     &     masks, vstress, xx_tauv0, xx_tauv1, xx_tauv_dummy,
     &     xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &     mytime, myiter, mythid )
#endif

      end
