#include "EXF_OPTIONS.h"

      subroutine exf_set_gen( 
     &     genfile, genstartdate, genperiod, exf_inscal_gen,
     &     genfld, gen0, gen1, genmask,
#ifdef USE_EXF_INTERPOLATION
     &     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     &     gen_nlon, gen_nlat, gen_xout, gen_yout,
#endif
     &     mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_gen
c     ==================================================================
c
c     o set external forcing gen
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c              20-Dec-2002: mods for pkg/seaice, menemenlis@jpl.nasa.gov
c              heimbach@mit.edu: totally re-organized exf_set_... 
c              replaced all routines by one generic routine
c              5-Aug-2003: added USE_EXF_INTERPOLATION for arbitrary
c                          input grid capability

c     ==================================================================
c     SUBROUTINE exf_set_gen
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"

c     == routine arguments ==

      _RL     genstartdate, genperiod
      _RL     exf_inscal_gen
      _RL     genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      character*1 genmask
      character*(128) genfile
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid
#ifdef USE_EXF_INTERPOLATION
c     gen_lon_0 ,gen_lat_0 :: longitude and latitude of SouthWest
c                             corner of global input grid
c     gen_nlon, gen_nlat   :: input x-grid and y-grid size
c     gen_lon_inc          :: scalar x-grid increment
c     gen_lat_inc          :: vector y-grid increments
c     gen_xout, gen_yout   :: coordinates for output grid
      _RL gen_lon0, gen_lon_inc
      _RL gen_lat0, gen_lat_inc(MAX_LAT_INC)
      INTEGER gen_nlon, gen_nlat
      _RS gen_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS gen_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j, interp_method

c     == end of interface ==

      if ( genfile .NE. ' ' ) then

c     get record numbers and interpolation factor for gen
         call exf_GetFFieldRec(
     I        genstartdate, genperiod
     O        , fac, first, changed
     O        , count0, count1
     I        , mycurrenttime, mycurrentiter, mythid
     &        )

         if ( first ) then
#ifdef USE_EXF_INTERPOLATION
            interp_method = 2
            call exf_interp( genfile, exf_iprec
     &           , gen1, count0, gen_xout, gen_yout
     &           , gen_lon0,gen_lon_inc
     &           , gen_lat0,gen_lat_inc
     &           , gen_nlon,gen_nlat,interp_method,mythid
     &           )
#else
            call mdsreadfield( genfile, exf_iprec, exf_yftype, 1
     &           , gen1, count0, mythid
     &           )
#endif

            if (exf_yftype .eq. 'RL') then
               call exf_filter_rl( gen1, genmask, mythid )
            else
               call exf_filter_rs( gen1, genmask, mythid )
            end if
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields( gen0, gen1, mythid )
            
#ifdef USE_EXF_INTERPOLATION
            interp_method = 2
            call exf_interp( genfile, exf_iprec
     &           , gen1, count1, gen_xout, gen_yout
     &           , gen_lon0,gen_lon_inc
     &           , gen_lat0,gen_lat_inc
     &           , gen_nlon,gen_nlat,interp_method,mythid
     &           )
#else
            call mdsreadfield( genfile, exf_iprec, exf_yftype, 1
     &           , gen1, count1, mythid
     &           )
#endif

            if (exf_yftype .eq. 'RL') then
               call exf_filter_rl( gen1, genmask, mythid )
            else
               call exf_filter_rs( gen1, genmask, mythid )
            end if
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do j = 1,sny
                  do i = 1,snx

c     Interpolate linearly onto the current time.

                     genfld(i,j,bi,bj) = exf_inscal_gen * (
     &                                fac * gen0(i,j,bi,bj) +
     &                    (exf_one - fac) * gen1(i,j,bi,bj) )

                  enddo
               enddo
            enddo
         enddo

      endif

      end



      subroutine exf_init_gen ( 
     &     genconst, genfld, gen0, gen1, mythid )

c     ==================================================================
c     SUBROUTINE exf_init_gen
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c              heimbach@mit.edu: totally re-organized exf_set_... 
c              replaced all routines by one generic routine
c
c     ==================================================================
c     SUBROUTINE exf_init_gen
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"

c     == routine arguments ==

      _RL genconst
      _RL genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer mythid

c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              genfld(i,j,bi,bj)  = genconst
              gen0(i,j,bi,bj)    = 0. _d 0
              gen1(i,j,bi,bj)    = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end
