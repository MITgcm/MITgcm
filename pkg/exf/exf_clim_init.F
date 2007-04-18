C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_init.F,v 1.11 2007/04/18 13:24:28 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_clim_init( mythid )

c     ==================================================================
c     SUBROUTINE exf_clim_init
c     ==================================================================
c
c     o This routine initialises the climatologic forcing
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-20000
c
c     ==================================================================
c     SUBROUTINE exf_clim_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "cal.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_CLIM_PARAM.h"
#include "EXF_CLIM_FIELDS.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer interp_method

c     == end of interface ==

c     initialize climatology fields

      interp_method=2

#ifdef ALLOW_CLIMSST_RELAXATION
      call exf_init_gen (
     &     climsstfile, climsstperiod, exf_inscal_climsst, climsstmask,
     &     climsstconst, climsst, climsst0, climsst1, 
#ifdef USE_EXF_INTERPOLATION
     &     climsst_lon0, climsst_lon_inc, 
     &     climsst_lat0, climsst_lat_inc,
     &     climsst_nlon, climsst_nlat, xC, yC, interp_method,
#endif
     &     mythid )
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      call exf_init_gen (
     &     climsssfile, climsssperiod, exf_inscal_climsss, climsssmask,
     &     climsssconst, climsss, climsss0, climsss1, 
#ifdef USE_EXF_INTERPOLATION
     &     climsss_lon0, climsss_lon_inc, 
     &     climsss_lat0, climsss_lat_inc,
     &     climsss_nlon, climsss_nlat, xC, yC, interp_method,
#endif
     &     mythid )
#endif

      end
