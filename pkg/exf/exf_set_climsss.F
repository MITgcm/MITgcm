c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_set_climsss.F,v 1.14 2006/12/14 22:15:27 dimitri Exp $

#include "EXF_OPTIONS.h"


      subroutine exf_set_climsss( 
     O                            mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_set_climsss
c     ==================================================================
c
c     o  Get the current climatological sea surface salinity field.
c
c     started: Christian Eckert eckert@mit.edu 27-Aug-1999
c     changed: Christian Eckert eckert@mit.edu 11-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c              Christian Eckert eckert@mit.edu 12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c     changed: heimbach@mit.edu 08-Feb-2002
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_set_climsss
c     ==================================================================

      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"
#include "exf_clim_param.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_CLIMSSS_RELAXATION

c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac
      integer bi, bj, i, j, interp_method
      integer year0, year1

c     == end of interface ==

      if ( climsssfile .NE. ' ' ) then

         if ( climsssperiod .EQ. 0 ) then

c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
            call cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1,
     I           mycurrenttime, mycurrentiter, mythid
     &           )

         else

c     get record numbers and interpolation factor for climsss
            call exf_GetFFieldRec(
     I           climsssstartdate, climsssperiod
     I           , climsssstartdate1, climsssstartdate2
     I           , .false.
     O           , fac, first, changed
     O           , count0, count1, year0, year1
     I           , mycurrenttime, mycurrentiter, mythid
     &           )

         endif

         if ( first ) then
#ifdef USE_EXF_INTERPOLATION
          interp_method = 2
          call exf_interp(
     &    climsssfile, exf_clim_iprec
     &  , climsss1, count0, xC, yC
     &  ,climsss_lon0,climsss_lon_inc
     &  ,climsss_lat0,climsss_lat_inc
     &  ,climsss_nlon,climsss_nlat,interp_method,mythid )
#else
            call mdsreadfield( climsssfile, exf_clim_iprec
     &           , exf_clim_yftype, 1
     &           , climsss1, count0, mythid
     &           )
#endif
            if (exf_clim_yftype .eq. 'RL') then
               call exf_filter_rl( climsss1, climsssmask, mythid )
            else
               call exf_filter_rs( climsss1, climsssmask, mythid )
            end if
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields( climsss0, climsss1, mythid )
#ifdef USE_EXF_INTERPOLATION
          interp_method = 2
          call exf_interp(
     &    climsssfile, exf_clim_iprec
     &  , climsss1, count1, xC, yC
     &  ,climsss_lon0,climsss_lon_inc
     &  ,climsss_lat0,climsss_lat_inc
     &  ,climsss_nlon,climsss_nlat,interp_method,mythid )
#else
            call mdsreadfield( climsssfile, exf_clim_iprec
     &           , exf_clim_yftype, 1
     &           , climsss1, count1, mythid
     &           )
#endif
            if (exf_clim_yftype .eq. 'RL') then
               call exf_filter_rl( climsss1, climsssmask, mythid )
            else
               call exf_filter_rs( climsss1, climsssmask, mythid )
            end if
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do j = 1,sny
                  do i = 1,snx

c     Interpolate linearly onto the current time.
                     climsss(i,j,bi,bj) = exf_inscal_sss * (
     &                                fac * climsss0(i,j,bi,bj) +
     &                    (exf_one - fac) * climsss1(i,j,bi,bj) )

                  enddo
               enddo
            enddo
         enddo

      endif

#endif /* ALLOW_CLIMSSS_RELAXATION */

      end


      subroutine exf_init_climsss(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_climsss
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_climsss
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_fields.h"
#include "exf_param.h"
#include "exf_clim_param.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_CLIMSSS_RELAXATION

c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              climsss (i,j,bi,bj) = climsssconst
              climsss0(i,j,bi,bj) = 0. _d 0
              climsss1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_CLIMSSSS_RELAXATION */

      end
