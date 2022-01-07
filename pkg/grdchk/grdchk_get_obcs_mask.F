#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine grdchk_get_obcs_mask( mythid )

c     ==================================================================
c     SUBROUTINE grdchk_get_obcs_mask
c     ==================================================================
c
c     o Get obcs masks from file
c
c     started: heimbach@mit.edu: 22-Apr-2003
c
c     ==================================================================
c     SUBROUTINE grdchk_get_obcs_mask
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "ctrl.h"
#include "CTRL_OBCS.h"
#include "grdchk.h"

c     == routine arguments ==
      integer mythid

#if (defined (ALLOW_GRDCHK) && defined (ALLOW_OBCS_CONTROL))
c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer iobcs
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      _RL dummy
#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-olx:snx+olx,nr,nsx,nsy)
#endif
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-oly:sny+oly,nr,nsx,nsy)
#endif
      character*( 80) fname

c     == end of interface ==

      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      _BEGIN_MASTER( mythid )

      if ( grdchkvarindex .EQ. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
      write(fname(1:80),'(80a)') ' '
      write(fname(1:80),'(a)') 'maskobcsn'
c
      do iobcs = 1,nobcs
         call active_read_xz(  fname, tmpfldxz, iobcs,
     &        .false., .false., 0, mythid, dummy)
c
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,nr
                  do i = imin,imax
                     grdchk_maskxz(i,k,bi,bj,iobcs) =
     &                    tmpfldxz(i,k,bi,bj)
                  enddo
               enddo
            enddo
         enddo
c
      enddo
#endif

      else if ( grdchkvarindex .EQ. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
      write(fname(1:80),'(80a)') ' '
      write(fname(1:80),'(a)') 'maskobcss'
c
      do iobcs = 1,nobcs
         call active_read_xz(  fname, tmpfldxz, iobcs,
     &        .false., .false., 0, mythid, dummy)
c
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,nr
                  do i = imin,imax
                     grdchk_maskxz(i,k,bi,bj,iobcs) =
     &                    tmpfldxz(i,k,bi,bj)
                  enddo
               enddo
            enddo
         enddo
c
      enddo
#endif

      else if ( grdchkvarindex .EQ. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
      write(fname(1:80),'(80a)') ' '
      write(fname(1:80),'(a)') 'maskobcsw'
c
      do iobcs = 1,nobcs
         call active_read_yz(  fname, tmpfldyz, iobcs,
     &        .false., .false., 0, mythid, dummy)
c
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,nr
                  do j = jmin,jmax
                     grdchk_maskyz(j,k,bi,bj,iobcs) =
     &                    tmpfldyz(j,k,bi,bj)
                  enddo
               enddo
            enddo
         enddo
c
      enddo
#endif

      else if ( grdchkvarindex .EQ. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
      write(fname(1:80),'(80a)') ' '
      write(fname(1:80),'(a)') 'maskobcse'
c
      do iobcs = 1,nobcs
         call active_read_yz(  fname, tmpfldyz, iobcs,
     &        .false., .false., 0, mythid, dummy)
c
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,nr
                  do j = jmin,jmax
                     grdchk_maskyz(j,k,bi,bj,iobcs) =
     &                    tmpfldyz(j,k,bi,bj)
                  enddo
               enddo
            enddo
         enddo
c
      enddo
#endif
      endif

      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      return
      end
