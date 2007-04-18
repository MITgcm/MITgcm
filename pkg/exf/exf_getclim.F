C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getclim.F,v 1.8 2007/04/18 13:24:28 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_getclim(
     I                        mytime,
     I                        myiter,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================
c
c     o Get the climatogy fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_OPTIONS.h .
c
c       A note on surface fluxes:
c
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_FIELDS.h"
#include "EXF_CLIM_PARAM.h"
#include "EXF_CLIM_FIELDS.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer interp_method

c     == end of interface ==

      interp_method=2

#ifdef ALLOW_CLIMSST_RELAXATION
c     Get values of climatological sst fields.
cph      call exf_set_gen  (
cph     &     climsstfile, climsststartdate, climsstperiod, 
cph     &     climsststartdate1, climsststartdate2,
cph     &     exf_inscal_climsst,
cph     &     climsst_exfremo_intercept, climsst_exfremo_slope,
cph     &     climsst, climsst0, climsst1, climsstmask, 
#ifdef USE_EXF_INTERPOLATION
cph     &     climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
cph     &     climsst_nlon, climsst_nlat, xC, yC, interp_method,
#endif
cph     &     mytime, myiter, mythid )
c
      call exf_set_climsst ( mytime, myiter, mythid )

c--   Update the tile edges.
      _EXCH_XY_R8(climsst, mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Get values of climatological sss fields.
cph      call exf_set_gen  (
cph     &     climsssfile, climsssstartdate, climsssperiod, 
cph     &     climsssstartdate1, climsssstartdate2,
cph     &     exf_inscal_climsss,
cph     &     climsss_exfremo_intercept, climsss_exfremo_slope,
cph     &     climsss, climsss0, climsss1, climsssmask, 
#ifdef USE_EXF_INTERPOLATION
cph     &     climsss_lon0, climsss_lon_inc, climsss_lat0, climsss_lat_inc,
cph     &     climsss_nlon, climsss_nlat, xC, yC, interp_method,
#endif
cph     &     mytime, myiter, mythid )
c
      call exf_set_climsss ( mytime, myiter, mythid )

c--   Update the tile edges.
      _EXCH_XY_R8(climsss, mythid)
#endif

      end
