c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_filter_rl.F,v 1.1 2001/05/14 22:08:40 heimbach Exp $

#include "EXF_CPPOPTIONS.h"

      subroutine exf_filter_rl(
     I                          arr,
     I                          ckind,
     I                          mythid
     &                        )

c     ==================================================================
c     SUBROUTINE exf_filter_rl
c     ==================================================================
c
c     o Read a flux record for external forcing.
c
c     started: Ralf.Giering@FastOpt.de 24-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_filter_rl
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "exf_constants.h"
#include "exfa.h"
#include "exfa_param.h"

c     == routine arguments ==

      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      character*1 ckind
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c     filter forcing field array
      do bj = jtlo,jthi
        do bi = itlo,ithi

c         Set undefined values to zero.
crg not necessary and
crg would require additional intermediate results in adjoint
crg          do j = 1,sny
crg            do i = 1,snx
crg              if (arr(i,j,bi,bj) .le. exf_undef) then
crg                arr(i,j,bi,bj) = 0. _d 0
crg              endif
crg            enddo
crg          enddo

c         Set land points to zero
          if      (ckind .eq. 's') then

            do j = 1,sny
              do i = 1,snx
                if ( _hFacC(i,j,1,bi,bj) .eq. 0. ) then
                  arr(i,j,bi,bj) = 0. _d 0
                endif
              enddo
            enddo

          else if (ckind .eq. 'u') then

            do j = 1,sny
              do i = 1,snx
                if ( maskw(i,j,1,bi,bj) .eq. 0. ) then
                  arr(i,j,bi,bj) = 0. _d 0
                endif
              enddo
            enddo

          else if (ckind .eq. 'v') then

            do j = 1,sny
              do i = 1,snx
                if ( masks(i,j,1,bi,bj) .eq. 0. ) then
                  arr(i,j,bi,bj) = 0. _d 0
                endif
              enddo
            enddo

          end if

        enddo
      enddo

      end
