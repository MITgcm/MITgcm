#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_ustressa(
     O                            ustressa
     I                          , mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                          )

c     ==================================================================
c     SUBROUTINE exf_set_ustressa
c     ==================================================================
c
c     o set external forcing ustressa
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000

c     ==================================================================
c     SUBROUTINE exf_set_ustressa
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exfa_param.h"
#include "exf_constants.h"

      common /exfl_ustressa_r/ ustressa0, ustressa1
      _RL ustressa0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL ustressa1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

c     == routine arguments ==

      _RL     ustressa(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
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

c     get record numbers and interpolation factor for ustressa
      call exf_GetFFieldRec(
     I                       ustressastartdate, ustressaperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

#ifndef ALLOW_AUTODIFF_TAMC
      if ( first ) then
#endif
        call mdsreadfield( ustressafile, exf_iprec, exf_yftype, 1
     &                   , ustressa1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( ustressa1, ustressamask, mythid )
        else
           call exf_filter_rs( ustressa1, ustressamask, mythid )
        end if
#ifndef ALLOW_AUTODIFF_TAMC
      endif
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      if (( first ) .or. ( changed )) then
#endif
        call exf_SwapFFields( ustressa0, ustressa1, mythid )

        call mdsreadfield( ustressafile, exf_iprec, exf_yftype, 1
     &                   , ustressa1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( ustressa1, ustressamask, mythid )
        else
           call exf_filter_rs( ustressa1, ustressamask, mythid )
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

              ustressa(i,j,bi,bj) = fac          *ustressa0(i,j,bi,bj)+
     &                          (exf_one - fac) *ustressa1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

      end



      subroutine exf_init_ustressa(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_ustressa
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_ustressa
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exfa_param.h"

      common /exfl_ustressa_r/ ustressa0, ustressa1
      _RL ustressa0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL ustressa1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

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
              ustressa0(i,j,bi,bj) = 0. _d 0
              ustressa1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

      end
