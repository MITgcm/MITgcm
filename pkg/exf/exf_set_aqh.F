#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_aqh( mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_aqh
c     ==================================================================
c
c     o set external forcing aqh
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002

c     ==================================================================
c     SUBROUTINE exf_set_aqh
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

#ifdef ALLOW_ATM_TEMP
c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j

c     == end of interface ==

c     get record numbers and interpolation factor for aqh
      call exf_GetFFieldRec(
     I                       aqhstartdate, aqhperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      if ( first ) then
        call mdsreadfield( aqhfile, exf_iprec, exf_yftype, 1
     &                   , aqh1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( aqh1, aqhmask, mythid )
        else
           call exf_filter_rs( aqh1, aqhmask, mythid )
        end if
      endif

      if (( first ) .or. ( changed )) then
        call exf_SwapFFields( aqh0, aqh1, mythid )

        call mdsreadfield( aqhfile, exf_iprec, exf_yftype, 1
     &                   , aqh1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( aqh1, aqhmask, mythid )
        else
           call exf_filter_rs( aqh1, aqhmask, mythid )
        end if
      endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1,sny
            do i = 1,snx

c             Interpolate linearly onto the current time.

              aqh(i,j,bi,bj) = fac          *aqh0(i,j,bi,bj)+
     &                          (exf_one - fac) *aqh1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

#endif

      end



      subroutine exf_init_aqh( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_aqh
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c
c     ==================================================================
c     SUBROUTINE exf_init_aqh
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"
#include "exf_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_ATM_TEMP
c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              aqh0(i,j,bi,bj) = 0. _d 0
              aqh1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif

      end
