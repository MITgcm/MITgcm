C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_set_climsst.F,v 1.18 2007/04/18 13:24:28 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_set_climsst( 
     O                            mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_set_climsst
c     ==================================================================
c
c     o  Get the current climatological sea surface salinity field.
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
c     SUBROUTINE exf_set_climsst
c     ==================================================================

      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_CLIM_PARAM.h"
#include "EXF_CLIM_FIELDS.h"

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_CLIMSST_RELAXATION

c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac
      integer bi, bj, i, j, interp_method
      integer year0, year1

c     == end of interface ==

      if ( climsstfile .NE. ' ' ) then

         if ( climsstperiod .EQ. 0 ) then

c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
            call cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1,
     I           mycurrenttime, mycurrentiter, mythid
     &           )

         else 

c     get record numbers and interpolation factor for climsst

            call exf_GetFFieldRec(
     I           climsststartdate, climsstperiod
     I           , climsststartdate1, climsststartdate2
     I           , .false.
     O           , fac, first, changed
     O           , count0, count1, year0, year1
     I           , mycurrenttime, mycurrentiter, mythid
     &           )

         endif

         if ( first ) then
#ifdef USE_EXF_INTERPOLATION
      _BARRIER
          interp_method = 2
          call exf_interp(
     &    climsstfile, exf_clim_iprec
     &  , climsst1, count0, xC, yC
     &  ,climsst_lon0,climsst_lon_inc
     &  ,climsst_lat0,climsst_lat_inc
     &  ,climsst_nlon,climsst_nlat,interp_method,mythid )
#else
            call mdsreadfield( climsstfile, exf_clim_iprec
     &           , exf_clim_yftype, 1
     &           , climsst1, count0, mythid
     &           )
#endif
            if (exf_clim_yftype .eq. 'RL') then
               call exf_filter_rl( climsst1, climsstmask, mythid )
            else
               call exf_filter_rs( climsst1, climsstmask, mythid )
            end if
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields( climsst0, climsst1, mythid )
            
#ifdef USE_EXF_INTERPOLATION
      _BARRIER
          interp_method = 2
          call exf_interp(
     &    climsstfile, exf_clim_iprec
     &  , climsst1, count1, xC, yC
     &  ,climsst_lon0,climsst_lon_inc
     &  ,climsst_lat0,climsst_lat_inc
     &  ,climsst_nlon,climsst_nlat,interp_method,mythid )
#else
            call mdsreadfield( climsstfile, exf_clim_iprec
     &           , exf_clim_yftype, 1
     &           , climsst1, count1, mythid
     &           )
#endif
            if (exf_clim_yftype .eq. 'RL') then
               call exf_filter_rl( climsst1, climsstmask, mythid )
            else
               call exf_filter_rs( climsst1, climsstmask, mythid )
            end if
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do j = 1,sny
                  do i = 1,snx

c     Set to freezing temperature if less
                     if (climsst0(i,j,bi,bj) .lt. climtempfreeze) then
                        climsst0(i,j,bi,bj) = climtempfreeze
                     endif
                     if (climsst1(i,j,bi,bj) .lt. climtempfreeze) then
                        climsst1(i,j,bi,bj) = climtempfreeze
                     endif

c     Interpolate linearly onto the current time.
                     climsst(i,j,bi,bj) = exf_inscal_sst * (
     &                                fac * climsst0(i,j,bi,bj) +
     &                    (exf_one - fac) * climsst1(i,j,bi,bj) )

                  enddo
               enddo
            enddo
         enddo

      endif

#endif /* ALLOW_CLIMSST_RELAXATION */

      end


      subroutine exf_init_climsst(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_climsst
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_climsst
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "EXF_FIELDS.h"
#include "EXF_PARAM.h"
#include "EXF_CLIM_PARAM.h"
#include "EXF_CLIM_FIELDS.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_CLIMSST_RELAXATION

c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              climsst (i,j,bi,bj) = climsstconst
              climsst0(i,j,bi,bj) = 0. _d 0
              climsst1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_CLIMSSST_RELAXATION */

      end
