
      subroutine hessupd( nn, mupd, dd, jmin, jmax, xdiff, lphprint )

c     ==================================================================
c     SUBROUTINE hessupd
c     ==================================================================
c
c     o controls update of descent vector using available
c       approximation of Hessian Matrix based on gradients of
c       previous iterations
c
c     o Reference: J.C. Gilbert & C. Lemarechal
c                  Some numerical experiments with variable-storage
c                  quasi-Newton algorithms
c                  Mathematical Programming 45 (1989), pp. 407-435
c
c     o started: ??? not reproducible
c
c     o changed: Patrick Heimbach, MIT/EAPS
c                24-Feb-2000: 
c                   - changed some variable names to be consistent
c                     with routine lsoptv, lsline;
c
c     o Version: 2.1.0, 02-Mar-2000: Patrick Heimbach, MIT/EAPS
c
c     ==================================================================
c     SUBROUTINE hessupd
c     ==================================================================

      implicit none

#include "blas1.h"

c------------------------------------
c declare arguments
c------------------------------------
      integer nn, mupd, jmin, jmax
      double precision dd(nn), alpha(100), xdiff(nn)
      logical lphprint

c------------------------------------
c declare local variables
c------------------------------------
      external DDOT
      double precision     DDOT

      integer jfin, i, j, jp
      double precision    r

c------------------------------------
c initialization
c------------------------------------
      jfin = jmax

      if (lphprint) 
     &     print *, 'pathei-lsopt: in hessupd; ', 
     &     'jmin, jmax, mupd:', jmin, jmax, mupd

      if (jfin.lt.jmin) jfin = jmax+mupd

c------------------------------------
c compute right hand side
c------------------------------------
      do j = jfin,jmin,-1

         if (lphprint) 
     &        print *, 'pathei-lsopt: in hessupd; loop ',
     &        'j,jfin,jmin = ', j,jfin,jmin

         jp = j
         if (jp.gt.mupd) jp = jp-mupd
         call dostore( nn, xdiff, .false., 2*jp+3 )
         r = DDOT( nn, dd, 1, xdiff,1 )
         call dostore( nn, xdiff, .false., 2*jp+2 )
         alpha(jp) = r
         do i = 1, nn
            dd(i) = dd(i) - r*xdiff(i)
         end do
      end do

c------------------------------------
c multiply precondition matrix
c------------------------------------
      if (mupd .ne. 0) then
         call dostore( nn, xdiff, .false., 3 )
         do i = 1, nn
            dd(i) = dd(i)*xdiff(i)
         end do
      end if

c------------------------------------
c compute left hand side
c------------------------------------
      do j = jmin,jfin
         jp = j
         if (jp .gt. mupd) jp = jp-mupd
         call dostore( nn, xdiff, .false., 2*jp+2 )
         r = alpha(jp) - DDOT( nn, dd,1 , xdiff, 1 )
         call dostore( nn, xdiff, .false., 2*jp+3 )
         do i = 1, nn
            dd(i) = dd(i) + r*xdiff(i)
         end do
      end do

      return

      end
