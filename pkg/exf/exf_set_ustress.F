#include "EXF_CPPOPTIONS.h"

      subroutine exf_set_ustress( mycurrenttime, mycurrentiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_ustress
c     ==================================================================
c
c     o set external forcing ustress
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002

c     ==================================================================
c     SUBROUTINE exf_set_ustress
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

#ifndef ALLOW_ATM_WIND
c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j

c     == end of interface ==

c     get record numbers and interpolation factor for ustress
      call exf_GetFFieldRec(
     I                       ustressstartdate, ustressperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      if ( first ) then
        call mdsreadfield( ustressfile, exf_iprec, exf_yftype, 1
     &                   , ustress1, count0, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( ustress1, ustressmask, mythid )
        else
           call exf_filter_rs( ustress1, ustressmask, mythid )
        end if
      endif

      if (( first ) .or. ( changed )) then
        call exf_SwapFFields( ustress0, ustress1, mythid )

        call mdsreadfield( ustressfile, exf_iprec, exf_yftype, 1
     &                   , ustress1, count1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( ustress1, ustressmask, mythid )
        else
           call exf_filter_rs( ustress1, ustressmask, mythid )
        end if
      endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1,sny
            do i = 1,snx

c             Interpolate linearly onto the current time.

              ustress(i,j,bi,bj) = fac          *ustress0(i,j,bi,bj)+
     &                          (exf_one - fac) *ustress1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

#endif

      end



      subroutine exf_init_ustress( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_ustress
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     changed: heimbach@mit.edu 10-Jan-2002
c
c     ==================================================================
c     SUBROUTINE exf_init_ustress
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_param.h"
#include "exf_fields.h"

c     == routine arguments ==

      integer mythid

#ifndef ALLOW_ATM_WIND
c     == local variables ==

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              ustress0(i,j,bi,bj) = 0. _d 0
              ustress1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif

      end
