#include "EXF_CPPOPTIONS.h"

      subroutine exf_init_runoff( mythid )

c     ==================================================================
c     SUBROUTINE exf_init_runoff
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_runoff
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"


c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer count0, count1
      integer bi, bj
      integer i, j

      _RL fac

c     == end of interface ==

#ifdef ALLOW_RUNOFF

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              runoff(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

        call mdsreadfield( runofffile, exf_iprec, exf_yftype, 1
     &                   , runoff, 1, mythid
     &                   )
        if (exf_yftype .eq. 'RL') then
           call exf_filter_rl( runoff, runoffmask, mythid )
        else 
           call exf_filter_rs( runoff, runoffmask, mythid )
        end if


cds   conversion from m/yr to m/s and changing sign.
      fac = -1.0/(86400.*365.0)
      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              if (runoff(i,j,bi,bj).gt.20.) runoff(i,j,bi,bj) = 0.
              runoff(i,j,bi,bj) = fac*runoff(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

#endif

      end
