#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_atemp( mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_atemp
c     ==================================================================
c
c     o set external forcing atemp
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002

c     ==================================================================
c     SUBROUTINE exf_set_atemp
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

c     get record numbers and interpolation factor for atemp
      call exf_GetFFieldRec(
     I                       atempstartdate, atempperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      if ( first ) then
        call mdsreadfield( atempfile, exf_iprec, exf_yftype, 1
     &                   , atemp1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( atemp1, atempmask, mythid )
        else
           call exf_filter_rs( atemp1, atempmask, mythid )
        end if
      endif

      if (( first ) .or. ( changed )) then
        call exf_SwapFFields( atemp0, atemp1, mythid )

        call mdsreadfield( atempfile, exf_iprec, exf_yftype, 1
     &                   , atemp1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( atemp1, atempmask, mythid )
        else
           call exf_filter_rs( atemp1, atempmask, mythid )
        end if
      endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1,sny
            do i = 1,snx

c             Interpolate linearly onto the current time.

              atemp(i,j,bi,bj) = fac          *atemp0(i,j,bi,bj)+
     &                          (exf_one - fac) *atemp1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

#endif

      end



      subroutine exf_init_atemp( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_atemp
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c
c     ==================================================================
c     SUBROUTINE exf_init_atemp
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
              atemp0(i,j,bi,bj) = 0. _d 0
              atemp1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif

      end
