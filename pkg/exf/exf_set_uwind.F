#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_uwind(
     O                            uwind
     I                          , mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exf_set_uwind
c     ==================================================================
c
c     o set external forcing uwind
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000

c     ==================================================================
c     SUBROUTINE exf_set_uwind
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"

      common /exfl_uwind_r/ uwind0, uwind1
      _RL uwind0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL uwind1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c     == routine arguments ==

      _RL     uwind(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
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

c     get record numbers and interpolation factor for uwind
      call exf_GetFFieldRec(
     I                       uwindstartdate, uwindperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

#ifndef ALLOW_AUTODIFF_TAMC
      if ( first ) then
#endif
        call mdsreadfield( uwindfile, exf_iprec, exf_yftype, 1
     &                   , uwind1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( uwind1, uwindmask, mythid )
        else
           call exf_filter_rs( uwind1, uwindmask, mythid )
        end if
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      if (( first ) .or. ( changed )) then
#endif
        call exf_SwapFFields( uwind0, uwind1, mythid )

        call mdsreadfield( uwindfile, exf_iprec, exf_yftype, 1
     &                   , uwind1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( uwind1, uwindmask, mythid )
        else
           call exf_filter_rs( uwind1, uwindmask, mythid )
        end if
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1,sny
            do i = 1,snx

c             Interpolate linearly onto the current time.

              uwind(i,j,bi,bj) = fac          *uwind0(i,j,bi,bj)+
     &                          (exf_one - fac) *uwind1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

      end



      subroutine exf_init_uwind(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_uwind
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_uwind
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"

      common /exfl_uwind_r/ uwind0, uwind1
      _RL uwind0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL uwind1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              uwind0(i,j,bi,bj) = 0. _d 0
              uwind1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end
