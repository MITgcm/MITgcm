#include "CTRL_OPTIONS.h"

      subroutine ctrl_mask_set_yz(
     &     ip1, iNone, OB_I, nwetobcs, ymaskobcs, myThid )

c     ==================================================================
c     SUBROUTINE ctrl_mask_set_yz
c     ==================================================================
c
c     o count sliced (yz) wet points and set yz masks
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
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_OBCS.h"

c     == routine arguments ==

      integer ip1, iNone
      integer OB_I     (1-OLy:sNy+OLy,nSx,nSy)
      integer nwetobcs (nSx,nSy,Nr,nobcs)
      character*(MAX_LEN_FNAM)   ymaskobcs
      integer myThid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi

      integer iobcs
      integer il
      _RL     dummy
      _RL     maskyz   (1-OLy:sNy+OLy,Nr,nSx,nSy,nobcs)
      _RL     gg       (1-OLy:sNy+OLy,Nr,nSx,nSy)

      character*(MAX_LEN_FNAM)   fname

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

      _BEGIN_MASTER( myThid )

c--   Count wet points at Northern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv

      do iobcs = 1,nobcs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,Nr
              do j = 1-OLy,sNy+OLy
                maskyz(j,k,bi,bj,iobcs) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      do iobcs = 1,nobcs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,Nr
              do j = 1,sNy
                i = OB_I(j,bi,bj)
                if ( i .NE. iNone ) then
c--               West mask for T, S, U on East/West boundaries.
                  if(iobcs .eq.1 .or. iobcs .eq.2 .or. iobcs .eq.3) then
                    if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
                      nwetobcs(bi,bj,k,iobcs) =nwetobcs(bi,bj,k,iobcs)+1
                      maskyz(j,k,bi,bj,iobcs) = 1
                    endif
                  endif
c--               South mask for V
                  if (iobcs .eq. 4) then
                    if (maskS(i,j,k,bi,bj) .eq. 1.) then
                      nwetobcs(bi,bj,k,iobcs) =nwetobcs(bi,bj,k,iobcs)+1
                      maskyz(j,k,bi,bj,iobcs) = 1
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
      write(fname,'(a)') ymaskobcs

      do iobcs = 1,nobcs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,Nr
              do j = 1,sNy
                 gg(j,k,bi,bj) = maskyz(j,k,bi,bj,iobcs)
              enddo
            enddo
          enddo
        enddo
        call active_write_yz( fname, gg, iobcs, 0, myThid, dummy)
      enddo
#endif

      _END_MASTER( myThid )

      return
      end
