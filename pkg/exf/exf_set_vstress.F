#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_vstress(
     O                            vstress
     I                          , mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exf_set_vstress
c     ==================================================================
c
c     o set external forcing vstress
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000

c     ==================================================================
c     SUBROUTINE exf_set_vstress
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"

      common /exfl_vstress_r/ vstress0, vstress1
      _RL vstress0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c     == routine arguments ==

      _RL     vstress(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
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

c     get record numbers and interpolation factor for vstress
      call exf_GetFFieldRec(
     I                       vstressstartdate, vstressperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

#ifndef ALLOW_AUTODIFF_TAMC
      if ( first ) then
#endif
        call mdsreadfield( vstressfile, exf_iprec, exf_yftype, 1
     &                   , vstress1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( vstress1, vstressmask, mythid )
        else
           call exf_filter_rs( vstress1, vstressmask, mythid )
        end if
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      if (( first ) .or. ( changed )) then
#endif
        call exf_SwapFFields( vstress0, vstress1, mythid )

        call mdsreadfield( vstressfile, exf_iprec, exf_yftype, 1
     &                   , vstress1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( vstress1, vstressmask, mythid )
        else
           call exf_filter_rs( vstress1, vstressmask, mythid )
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

              vstress(i,j,bi,bj) = fac          *vstress0(i,j,bi,bj)+
     &                          (exf_one - fac) *vstress1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

      end



      subroutine exf_init_vstress(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_vstress
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_vstress
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"

      common /exfl_vstress_r/ vstress0, vstress1
      _RL vstress0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstress1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

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
              vstress0(i,j,bi,bj) = 0. _d 0
              vstress1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end
