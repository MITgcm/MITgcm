#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_evap( mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_evap
c     ==================================================================
c
c     o set external forcing evap
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002

c     ==================================================================
c     SUBROUTINE exf_set_evap
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"
#include "exf_fields.h"

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef EXF_READ_EVAP
c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j

c     == end of interface ==

c     get record numbers and interpolation factor for evap
      call exf_GetFFieldRec(
     I                       evapstartdate, evapperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      if ( first ) then
        if ( evapfile .NE. ' ' )
     &        call mdsreadfield( evapfile, exf_iprec, exf_yftype, 1
     &                   , evap1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( evap1, evapmask, mythid )
        else
           call exf_filter_rs( evap1, evapmask, mythid )
        end if
      endif

      if (( first ) .or. ( changed )) then
        call exf_SwapFFields( evap0, evap1, mythid )

        if ( evapfile .NE. ' ' )
     &       call mdsreadfield( evapfile, exf_iprec, exf_yftype, 1
     &                   , evap1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( evap1, evapmask, mythid )
        else
           call exf_filter_rs( evap1, evapmask, mythid )
        end if
      endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1,sny
            do i = 1,snx

c             Interpolate linearly onto the current time.

              evap(i,j,bi,bj) = fac          *evap0(i,j,bi,bj)+
     &                          (exf_one - fac) *evap1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

#endif EXF_READ_EVAP

      end



      subroutine exf_init_evap( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_evap
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c
c     ==================================================================
c     SUBROUTINE exf_init_evap
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"
#include "exf_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef EXF_READ_EVAP
c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              evap(i,j,bi,bj)  = 0. _d 0
              evap0(i,j,bi,bj) = 0. _d 0
              evap1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif EXF_READ_EVAP

      end
