C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_init.F,v 1.2 2001/07/13 14:50:46 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine grdchk_init(
     I                       mythid
     &                      )

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
#include "grdchk.h"

c     == routine arguments ==

      integer       mythid

#ifdef ALLOW_GRADIENT_CHECK
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer irec
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer itest
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

c--   Determine the number of components of the given
c--   control variable on the current tile.
      if ( ncvargrd(grdchkvarindex) .eq. 'c' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k) = nwetctile(bi,bj,k)
               enddo
            enddo
         enddo
      else if ( ncvargrd(grdchkvarindex) .eq. 's' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k) = nwetstile(bi,bj,k)
               enddo
            enddo
         enddo
      else if ( ncvargrd(grdchkvarindex) .eq. 'w' ) then
         do bj = jtlo,jthi
            do bi = itlo,ithi
               do k = 1,ncvarnrmax(grdchkvarindex)
                  nwettile(bi,bj,k) = nwetwtile(bi,bj,k)
               enddo
            enddo
         enddo
      else
ce        --> wrong grid specification for the control variable.
      endif

c     ----------------------------------------------------------------

c--   Determine the actual and the maximum possible number of
c--   components of the given control variable.
      ncvarcomp     = 0
      maxncvarcomps = 0
      do bj = jtlo,jthi
         do bi = itlo,ithi
            do k = 1,ncvarnrmax(grdchkvarindex)
               ncvarcomp     = ncvarcomp     + nwettile(bi,bj,k)
               maxncvarcomps = maxncvarcomps + snx*sny
            enddo
         enddo
      enddo
      ncvarcomp     = ncvarcomp*ncvarrecs(grdchkvarindex)
      maxncvarcomps = maxncvarcomps*ncvarrecs(grdchkvarindex)

      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRADIENT_CHECK */

      end

