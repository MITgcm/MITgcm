#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_lwdown( mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_lwdown
c     ==================================================================
c
c     o set external forcing lwdown
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002

c     ==================================================================
c     SUBROUTINE exf_set_lwdown
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

#ifdef ALLOW_DOWNWARD_RADIATION
c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j

c     == end of interface ==

      if ( lwdownfile .NE. ' ' ) then

c     get record numbers and interpolation factor for lwdown
         call exf_GetFFieldRec(
     I        lwdownstartdate, lwdownperiod
     O        , fac, first, changed
     O        , count0, count1
     I        , mycurrenttime, mycurrentiter, mythid
     &        )

         if ( first ) then
            call mdsreadfield( lwdownfile, exf_iprec, exf_yftype, 1
     &           , lwdown1, count0, mythid
     &           )
            if (exf_yftype .eq. 'RL') then
               call exf_filter_rl( lwdown1, lwdownmask, mythid )
            else
               call exf_filter_rs( lwdown1, lwdownmask, mythid )
            end if
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields( lwdown0, lwdown1, mythid )

            call mdsreadfield( lwdownfile, exf_iprec, exf_yftype, 1
     &           , lwdown1, count1, mythid
     &           )
            if (exf_yftype .eq. 'RL') then
               call exf_filter_rl( lwdown1, lwdownmask, mythid )
            else
               call exf_filter_rs( lwdown1, lwdownmask, mythid )
            end if
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do j = 1,sny
                  do i = 1,snx

c     Interpolate linearly onto the current time.

                     lwdown(i,j,bi,bj) = fac * lwdown0(i,j,bi,bj) +
     &                    (exf_one - fac) * lwdown1(i,j,bi,bj)

                  enddo
               enddo
            enddo
         enddo

      endif

#endif

      end



      subroutine exf_init_lwdown( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_lwdown
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c
c     ==================================================================
c     SUBROUTINE exf_init_lwdown
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"
#include "exf_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_DOWNWARD_RADIATION
c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              lwdown(i,j,bi,bj)  = lwdownconst
              lwdown0(i,j,bi,bj) = 0. _d 0
              lwdown1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif

      end
