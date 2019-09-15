#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine grdchk_init( mythid )

c     ==================================================================
c     SUBROUTINE grdchk_init
c     ==================================================================
c
c     o Get the location of a given component of the control vector for
c       the current process.
c
c     started: Christian Eckert eckert@mit.edu 04-Apr-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_init
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

      integer       mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer irec
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer itest,iobcs
      integer icomptest

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

c--   initialise
      do bj = jtlo,jthi
         do bi = itlo,ithi
            do k = 1,ncvarnrmax(grdchkvarindex)
               do iobcs = 1, nobcs
                  nwettile(bi,bj,k,iobcs) = 0
               enddo
            enddo
         enddo
      enddo

c--   Determine the number of components of the given
c--   control variable on the current tile.
      if ( ncvargrd(grdchkvarindex) .eq. 'c' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k,1) = nwetctile(bi,bj,k)
               enddo
            enddo
         enddo
      else if ( ncvargrd(grdchkvarindex) .eq. 's' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k,1) = nwetstile(bi,bj,k)
               enddo
            enddo
         enddo
      else if ( ncvargrd(grdchkvarindex) .eq. 'w' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k,1) = nwetwtile(bi,bj,k)
               enddo
            enddo
         enddo
      else if ( ncvargrd(grdchkvarindex) .eq. 'v' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k,1) = nwetvtile(bi,bj,k)
               enddo
            enddo
         enddo
#if (defined ALLOW_SHIFWFLX_CONTROL) || (defined ALLOW_SHI2D_CONTROL)
      else if ( ncvargrd(grdchkvarindex) .eq. 'i' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k,1) = nwetitile(bi,bj,k)
               enddo
            enddo
         enddo
#endif /* ALLOW_SHIFWFLX_CONTROL || ALLOW_SHI2D_CONTROL */
      else if ( ncvargrd(grdchkvarindex) .eq. 'm' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  do iobcs = 1, nobcs
                     if ( grdchkvarindex .eq. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
                        nwettile(bi,bj,k,iobcs) =
     &                       nwetobcsn(bi,bj,k,iobcs)
#endif
                     else if ( grdchkvarindex .eq. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
                        nwettile(bi,bj,k,iobcs) =
     &                       nwetobcss(bi,bj,k,iobcs)
#endif
                     else if ( grdchkvarindex .eq. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
                        nwettile(bi,bj,k,iobcs) =
     &                       nwetobcsw(bi,bj,k,iobcs)
#endif
                     else if ( grdchkvarindex .eq. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
                        nwettile(bi,bj,k,iobcs) =
     &                       nwetobcse(bi,bj,k,iobcs)
#endif
                     endif
                  enddo
               enddo
            enddo
         enddo
      else
ce        --> wrong grid specification for the control variable.
      endif

c--   get mask file for obcs
#ifdef ALLOW_OBCS_CONTROL
      call grdchk_get_obcs_mask ( mythid )
#endif

c     ----------------------------------------------------------------

c--   Determine the actual and the maximum possible number of
c--   components of the given control variable.
      ncvarcomp     = 0
      maxncvarcomps = 0
      do bj = jtlo,jthi
         do bi = itlo,ithi
            do k = 1,ncvarnrmax(grdchkvarindex)
               do iobcs = 1, nobcs
                  ncvarcomp     = ncvarcomp + nwettile(bi,bj,k,iobcs)
                  maxncvarcomps = maxncvarcomps +
     &                 ncvarxmax(grdchkvarindex)*
     &                 ncvarymax(grdchkvarindex)
               enddo
            enddo
         enddo
      enddo
      ncvarcomp     = ncvarcomp*ncvarrecs(grdchkvarindex)
      maxncvarcomps = maxncvarcomps*ncvarrecs(grdchkvarindex)

      do bj = jtlo,jthi
         do bi = itlo,ithi
            iwetsum(bi,bj,0)    = 0
            do k = 1,ncvarnrmax(grdchkvarindex)
               iwetsum(bi,bj,k) = iwetsum(bi,bj,k-1) +
     &              nwettile(bi,bj,k,1)
            enddo
         enddo
      enddo

      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      return
      end
