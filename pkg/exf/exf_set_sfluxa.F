#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_sfluxa(
     O                            sfluxa
     I                          , mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exf_set_sfluxa
c     ==================================================================
c
c     o set external forcing sfluxa
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000

c     ==================================================================
c     SUBROUTINE exf_set_sfluxa
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exfa_param.h"
#include "exf_constants.h"

      common /exfl_sfluxa_r/ sfluxa0, sfluxa1
      _RL sfluxa0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sfluxa1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c     == routine arguments ==

      _RL     sfluxa(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
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

c     get record numbers and interpolation factor for sfluxa
      call exf_GetFFieldRec(
     I                       sfluxastartdate, sfluxaperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

#ifndef ALLOW_AUTODIFF_TAMC
      if ( first ) then
#endif
        call mdsreadfield( sfluxafile, exf_iprec, exf_yftype, 1
     &                   , sfluxa1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( sfluxa1, sfluxamask, mythid )
        else
           call exf_filter_rs( sfluxa1, sfluxamask, mythid )
        end if
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      if (( first ) .or. ( changed )) then
#endif
        call exf_SwapFFields( sfluxa0, sfluxa1, mythid )

        call mdsreadfield( sfluxafile, exf_iprec, exf_yftype, 1
     &                   , sfluxa1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( sfluxa1, sfluxamask, mythid )
        else
           call exf_filter_rs( sfluxa1, sfluxamask, mythid )
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

              sfluxa(i,j,bi,bj) = fac          *sfluxa0(i,j,bi,bj)+
     &                          (exf_one - fac) *sfluxa1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

      end



      subroutine exf_init_sfluxa(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_sfluxa
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_sfluxa
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exfa_param.h"

      common /exfl_sfluxa_r/ sfluxa0, sfluxa1
      _RL sfluxa0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL sfluxa1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

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
              sfluxa0(i,j,bi,bj) = 0. _d 0
              sfluxa1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end
