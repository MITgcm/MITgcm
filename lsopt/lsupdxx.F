
      subroutine lsupdxx(
     &     nn, ifail, lphprint
     &     , jmin, jmax, nupdate
     &     , ff, fmin, fold, gnorm0, dotdg
     &     , gg, dd, xx, xdiff
     &     , tmin, tmax, tact, epsx
     &     )

c     ==================================================================
c     SUBROUTINE lsupdxx
c     ==================================================================
c
c     o conceived for variable online/offline version
c       computes - new descent direction dd based on latest 
c                  available gradient
c                - new tact based on new dd
c                - new control vector xx needed for offline run
c
c     o started: Patrick Heimbach, MIT/EAPS
c                29-Feb-2000: 
c
c     o Version 2.1.0, 02-Mar-2000: Patrick Heimbach, MIT/EAPS
c
c     ==================================================================
c     SUBROUTINE lsupdxx
c     ==================================================================
c

#include "blas1.h"

      implicit none

c-----------------------------------------
c declare arguments
c-----------------------------------------
      integer nn, jmin, jmax, nupdate, ifail
      double precision    ff, fmin, fold, gnorm0, dotdg
      double precision    gg(nn), dd(nn), xx(nn), xdiff(nn)
      double precision    tmin, tmax, tact, epsx
      logical lphprint

c-----------------------------------------
C declare local variables
c-----------------------------------------
      integer i
      double precision    fdiff, preco

      double precision     DDOT
      external DDOT

c     ==================================================================

c-----------------------------------------
c use Fletchers scaling
c and initialize diagional to 1.
c-----------------------------------------
c
      if ( ( jmax .eq. 0 ) .or. (nupdate .eq. 0 ) ) then

         if (jmax .eq. 0) then
            fold = fmin
            if (lphprint) 
     &           print *, 'pathei-lsopt: using fold = fmin = ', fmin
         end if
         fdiff = fold - ff
         if (jmax .eq. 0) fdiff = ABS(fdiff)
         
         preco = 2. * fdiff / (gnorm0*gnorm0)
         do i = 1, nn
            dd(i)    = -gg(i)*preco
         end do

         if (lphprint) 
     &        print *, 'pathei-lsopt: first estimate of dd via ',
     &        'fold - ff'

c-----------------------------------------
c use the matrix stored in [diag]
c and the (y,s) pairs
c-----------------------------------------

         else

            do i = 1, nn
               dd(i) = -gg(i)
            end do

            if (jmax .gt. 0) then
               call hessupd( nn, nupdate, dd, jmin, jmax, xdiff, 
     &              lphprint )
            else
               if (lphprint) 
     &              print *, 'pathei-lsopt: no hessupd for first optim.'
            end if

         endif

c-----------------------------------------
c check whether new direction is a descent one
c-----------------------------------------
         dotdg = DDOT( nn, dd, 1, gg, 1 )
         if (dotdg .ge. 0.0) then
            ifail = 4
            goto 999
         end if

c----------------------------------
c declare arguments
c----------------------------------

      tmin = 0.
      do i = 1, nn
         tmin = max( tmin, abs(dd(i)) )
      end do
      tmin = epsx/tmin

c----------------------------------
c make sure that t is between
c tmin and tmax
c----------------------------------

      tact  = 1.0
      tmax = 1.0e+10
      if (tact.le.tmin) then
         tact = tmin
         if (tact.gt.tmax) then
            tmin = tmax
         endif
      endif

      if (tact .gt. tmax) then
          tact = tmax
          ifail = 7
      endif

c----------------------------------
c compute new x
c----------------------------------
      do i = 1, nn
         xdiff(i) = xx(i) + tact*dd(i)
      end do

c----------------------------------
c save new x to file for offline version
c----------------------------------

 999  continue

      return

      end
