#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_gen( 
     &     genfile, genstartdate, genperiod, exf_inscal_gen,
     &     genfld, gen0, gen1, genmask, 
     &     mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_gen
c     ==================================================================
c
c     o set external forcing gen
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c              mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c              heimbach@mit.edu: totally re-organized exf_set_... 
c              replaced all routines by one generic routine

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

      integer genstartdate(4)
      _RL     genperiod
      _RL     exf_inscal_gen
      _RL     genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      character*1 genmask
      character*(128) genfile
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j

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
            call mdsreadfield( genfile, exf_iprec, exf_yftype, 1
     &           , gen1, count0, mythid
     &           )

            if (exf_yftype .eq. 'RL') then
               call exf_filter_rl( gen1, genmask, mythid )
            else
               call exf_filter_rs( gen1, genmask, mythid )
            end if
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields( gen0, gen1, mythid )
            
            call mdsreadfield( genfile, exf_iprec, exf_yftype, 1
     &           , gen1, count1, mythid
     &           )

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
