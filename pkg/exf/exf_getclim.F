C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getclim.F,v 1.10 2007/04/18 19:55:34 heimbach Exp $
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

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer interp_method
      integer bi, bj, i, j

c     == end of interface ==

      interp_method=2

#ifdef ALLOW_CLIMSST_RELAXATION
c     Get values of climatological sst fields.
      call exf_set_gen  (
     &     climsstfile, climsststartdate, climsstperiod, 
     &     climsststartdate1, climsststartdate2,
     &     exf_inscal_climsst,
     &     climsst_exfremo_intercept, climsst_exfremo_slope,
     &     climsst, climsst0, climsst1, climsstmask, 
#ifdef USE_EXF_INTERPOLATION
     &     climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
     &     climsst_nlon, climsst_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )
c
      do bj = mybylo(mythid),mybyhi(mythid)
       do bi = mybxlo(mythid),mybxhi(mythid)
        do j = 1,sny
         do i = 1,snx
            if (climsst(i,j,bi,bj) .lt. climtempfreeze) then
               climsst(i,j,bi,bj) = climtempfreeze
            endif
         enddo
        enddo
       enddo
      enddo

c--   Update the tile edges.
      _EXCH_XY_R8(climsst, mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Get values of climatological sss fields.
      call exf_set_gen  (
     &     climsssfile, climsssstartdate, climsssperiod, 
     &     climsssstartdate1, climsssstartdate2,
     &     exf_inscal_climsss,
     &     climsss_exfremo_intercept, climsss_exfremo_slope,
     &     climsss, climsss0, climsss1, climsssmask, 
#ifdef USE_EXF_INTERPOLATION
     &     climsss_lon0, climsss_lon_inc, climsss_lat0, climsss_lat_inc,
     &     climsss_nlon, climsss_nlat, xC, yC, interp_method,
#endif
     &     mytime, myiter, mythid )
c

c--   Update the tile edges.
      _EXCH_XY_R8(climsss, mythid)
#endif

      end
