#include "CTRL_OPTIONS.h"

      subroutine ctrl_mask_set_xz(
     &     jp1, jNone, OB_J, nwetobcs, ymaskobcs, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_mask_set_xz
c     ==================================================================
c
c     o count sliced (xz) wet points and set xz masks
c
c     heimbach@mit.edu, 30-Aug-2001
c     gebbie@mit.edu, corrected array bounds
c
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"
#include "CTRL_OBCS.h"

c     == routine arguments ==

      integer jp1, jNone
      integer OB_J     (1-olx:snx+olx,nsx,nsy)
      integer nwetobcs (nsx,nsy,nr,nobcs)
      character*(80)   ymaskobcs
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi

      integer iobcs
      integer il
      _RL     dummy
      _RL     maskxz   (1-olx:snx+olx,nr,nsx,nsy,nobcs)
      _RL     gg       (1-olx:snx+olx,nr,nsx,nsy)

      character*( 80)   fname

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      _BEGIN_MASTER( myThid )

c--   Count wet points at Northern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      do iobcs = 1,nobcs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do i = 1-olx,snx+olx
                maskxz(i,k,bi,bj,iobcs) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      do iobcs = 1,nobcs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do i = 1,snx
                j = OB_J(i,bi,bj)
                if ( j .NE. jNone ) then
c--               South mask for T, S, V
                  if (iobcs.eq.1 .or. iobcs .eq.2 .or. iobcs.eq.3) then
                    if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                      nwetobcs(bi,bj,k,iobcs) =nwetobcs(bi,bj,k,iobcs)+1
                      maskxz(i,k,bi,bj,iobcs) = 1
                    endif
                  endif
c--               West mask for U
                  if (iobcs .eq. 4) then
                    if (maskW(i,j,k,bi,bj) .eq. 1.) then
                      nwetobcs(bi,bj,k,iobcs) =nwetobcs(bi,bj,k,iobcs)+1
                      maskxz(i,k,bi,bj,iobcs) = 1
                    endif
                  endif
                endif
              enddo
            enddo
          enddo
        enddo
      enddo

#ifdef ALLOW_AUTODIFF
      il=ilnblnk( ymaskobcs )
      write(fname(1:80),'(80a)') ' '
      write(fname(1:80),'(a)') ymaskobcs

      do iobcs = 1,nobcs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do i = 1,snx
                 gg(i,k,bi,bj) = maskxz(i,k,bi,bj,iobcs)
              enddo
            enddo
          enddo
        enddo
        call active_write_xz( fname, gg, iobcs, 0, mythid, dummy )
      enddo
#endif

      _END_MASTER( mythid )

      return
      end
