
      subroutine dgscale( nn, gold, xdiff, diag, rmin )


c     ==================================================================
c     SUBROUTINE dgscale
c     ==================================================================
c
c     o computes new preconditioner and writes it to OPWARMD
c
c     o started: ??? not reproducible
c
c     o Version: 2.1.0, 02-Mar-2000: Patrick Heimbach, MIT/EAPS
c
c     ==================================================================
c     SUBROUTINE dgscale
c     ==================================================================

      implicit none

#include "blas1.h"

      integer nn
      double precision    gold(nn), xdiff(nn), diag(nn)

      integer i
      double precision    r1, rmin, den


c-----------------------------------------
c read diagonal
c-----------------------------------------
      call dostore( nn, diag, .false., 3 )

      r1 = 0.
      do i = 1, nn
        r1 = r1 + gold(i)*gold(i)*diag(i)
      end do
      r1 = 1.0 / r1

      call SSCAL( nn, r1, diag, 1 )

c-----------------------------------------
c update the diagonal
c (gg is used as an auxiliary vector)
c-----------------------------------------

      den = 0.0

      do i = 1, nn
cph(
         if (diag(i).LE.0) then
cph            print *, 'pathei-lsopt: in dgscale; diag = 0 for i=', i
            diag(i) = rmin
         end if
cph)
         den = den + xdiff(i)*xdiff(i) / diag(i)
      end do

      do i = 1, nn
         diag(i) = 1./
     $        (1./diag(i)+gold(i)**2-(xdiff(i)/diag(i))**2/den)
         if (diag(i).le.0.) then
            diag(i) = rmin
         endif
      end do

c-----------------------------------------
c write diagonal
c-----------------------------------------
      call dostore( nn, diag, .true., 3 )

      return
      end
