#include "EXF_OPTIONS.h"

      subroutine exf_init_runoff( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_runoff
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_init_runoff
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

#ifdef ALLOW_AUTODIFF
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

      integer mythid

#if (defined (ALLOW_RUNOFF) || defined (ALLOW_SEAICE))

c     == local variables ==

      integer bi, bj, i, j, interp_method

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1-oly, sny+oly
            do i = 1-olx, snx+olx
              runoff(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      if ( runofffile .NE. ' ' ) then
#ifdef USE_EXF_INTERPOLATION
         interp_method = 1
         call exf_interp(
     &    runofffile, exf_iprec
     &  , runoff, 1, xC, yC
     &  ,runoff_lon0,runoff_lon_inc
     &  ,runoff_lat0,runoff_lat_inc
     &  ,runoff_nlon,runoff_nlat,interp_method,mythid )
#else
         call mdsreadfield( runofffile, exf_iprec, exf_yftype, 1
     &        , runoff, 1, mythid
     &        )
#endif
         if (exf_yftype .eq. 'RL') then
            call exf_filter_rl( runoff, runoffmask, mythid )
         else 
            call exf_filter_rs( runoff, runoffmask, mythid )
         end if
         
c--   scale runoff array
         do bj = mybylo(mythid), mybyhi(mythid)
           do bi = mybxlo(mythid), mybxhi(mythid)
             do j = 1, sny
               do i = 1, snx
                 if(ABS(runoff(i,j,bi,bj)).gt.998.) runoff(i,j,bi,bj)=0.
                 runoff(i,j,bi,bj)=exf_inscal_runoff*runoff(i,j,bi,bj)
              enddo
            enddo
          enddo
        enddo
      endif

#ifdef ALLOW_RUNOFF_CONTROL
      call ctrl_get_gen ( 
     &     xx_runoff_file, xx_runoffstartdate, xx_runoffperiod,
     &     maskc, runoff, xx_runoff0, xx_runoff1, xx_runoff_dummy,
     &     xx_runoff_remo_intercept, xx_runoff_remo_slope,
     &     0., 0., mythid )
#endif

      _EXCH_XY_R4(  runoff, mythid )

      CALL PLOT_FIELD_XYRL( runoff, 
     &     'Runoff after rescaling and exf_inscal_runoff' , 
     &     0, myThid )

#endif

      end
