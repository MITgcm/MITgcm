C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getclim.F,v 1.25 2014/11/02 21:23:30 gforget Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

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
c       The MITgcm-UV vertical coordinate z is positive upward.
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
#ifdef ALLOW_BULK_OFFLINE
# include "PARAMS.h"
# include "DYNVARS.h"
#endif
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_FIELDS.h"
#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# ifdef ALLOW_ECCO
#  include "ecco_cost.h"
# endif
#endif

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      integer bi, bj, i, j

c     == end of interface ==

#ifdef ALLOW_CLIMSST_RELAXATION
c     Get values of climatological sst fields.
      call exf_set_gen  (
     &     climsstfile, climsststartdate, climsstperiod,
     &     exf_inscal_climsst,
     &     climsst_exfremo_intercept, climsst_exfremo_slope,
     &     climsst, climsst0, climsst1, climsstmask,
#ifdef USE_EXF_INTERPOLATION
     &     climsst_lon0, climsst_lon_inc, climsst_lat0, climsst_lat_inc,
     &     climsst_nlon, climsst_nlat, xC, yC, climsst_interpMethod,
#endif
     &     mytime, myiter, mythid )

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
      _EXCH_XY_RL(climsst, mythid)
#endif

#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# ifdef ALLOW_SST_CONTROL
      call CTRL_GET_GEN (
     &     xx_sst_file, xx_sststartdate, xx_sstperiod,
     &     maskc, climsst, xx_sst0, xx_sst1, xx_sst_dummy,
     &     xx_sst_remo_intercept, xx_sst_remo_slope,
     &     wsst, mytime, myiter, mythid )
# endif
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Get values of climatological sss fields.
      call exf_set_gen  (
     &     climsssfile, climsssstartdate, climsssperiod,
     &     exf_inscal_climsss,
     &     climsss_exfremo_intercept, climsss_exfremo_slope,
     &     climsss, climsss0, climsss1, climsssmask,
#ifdef USE_EXF_INTERPOLATION
     &     climsss_lon0, climsss_lon_inc, climsss_lat0, climsss_lat_inc,
     &     climsss_nlon, climsss_nlat, xC, yC, climsss_interpMethod,
#endif
     &     mytime, myiter, mythid )

c--   Update the tile edges.
      _EXCH_XY_RL(climsss, mythid)
#endif

#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# ifdef ALLOW_SSS_CONTROL
      call CTRL_GET_GEN (
     &     xx_sss_file, xx_sssstartdate, xx_sssperiod,
     &     maskc, climsss, xx_sss0, xx_sss1, xx_sss_dummy,
     &     xx_sss_remo_intercept, xx_sss_remo_slope,
     &     wsss, mytime, myiter, mythid )
# endif
#endif

#ifdef ALLOW_CLIMSTRESS_RELAXATION
c     Get values of climatological ustr fields.
      call exf_set_uv (
     I     climustrfile, climustrstartdate, climustrperiod,
     I     exf_inscal_climustr,
     I     climustr_exfremo_intercept, climustr_exfremo_slope,
     U     climustr, climustr0, climustr1, climustrmask,
     I     climvstrfile, climvstrstartdate, climvstrperiod,
     I     exf_inscal_climvstr,
     I     climvstr_exfremo_intercept, climvstr_exfremo_slope,
     U     climvstr, climvstr0, climvstr1, climvstrmask,
#ifdef USE_EXF_INTERPOLATION
     I     climustr_lon0, climustr_lon_inc,
     I     climustr_lat0, climustr_lat_inc,
     I     climustr_nlon, climustr_nlat, climustr_interpMethod,
     I     climvstr_lon0, climvstr_lon_inc,
     I     climvstr_lat0, climvstr_lat_inc,
     I     climvstr_nlon, climvstr_nlat, climvstr_interpMethod,
     I     uvInterp_climstr,
#endif
     I     mytime, myiter, mythid )

      CALL EXCH_UV_XY_RL( climustr, climvstr, .TRUE., myThid )
#endif /* CLIMSTRESS_RELAXATION */

#ifdef ALLOW_BULK_OFFLINE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-oLy,sNy+oLy
          DO i=1-oLx,sNx+oLx
# ifdef ALLOW_CLIMSST_RELAXATION
           if ( climsstfile .NE. ' ' .AND.
     &          climsstperiod .NE. 0. )
     &          theta(i,j,1,bi,bj) = climsst(i,j,bi,bj)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
           if ( climsssfile .NE. ' ' .AND.
     &          climsssperiod .NE. 0. )
     &          salt(i,j,1,bi,bj) = climsss(i,j,bi,bj)
# endif
# ifdef ALLOW_CLIMSTRESS_RELAXATION
           if ( climustrfile .NE. ' ' .AND.
     &          climustrperiod .NE. 0. )
     &          uvel(i,j,1,bi,bj) = climustr(i,j,bi,bj)
           if ( climvstrfile .NE. ' ' .AND.
     &          climvstrperiod .NE. 0. )
     &          vvel(i,j,1,bi,bj) = climvstr(i,j,bi,bj)
# endif
           if ( myiter .EQ. niter0 ) then
              if ( maskC(i,j,1,bi,bj) .NE. 0. .AND.
     &             theta(i,j,1,bi,bj) .EQ. 0. ) then
                print *, 'ph-warn-exf-clim ', i, j, theta(i,j,1,bi,bj)
cph                STOP 'in exf_getclim'
              endif
           endif
          ENDDO
         ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_BULK_OFFLINE */

      RETURN
      END
