
      subroutine simul(
     I                  indic,
     I                  nn,
     I                  xx,
     O                  objf,
     O                  adxx
     &                )

c     ==================================================================
c     SUBROUTINE simul
c     ==================================================================
c
c     o This routine is called by the large-scale optimization lsopt.
c
c       Input  : indic - Parameter (not used here).
c                nn    - Number of control variables.
c                xx    - Array  of control variables.
c
c       Output : objf  - Value of objective function.
c                adxx  - Gradients of objective function with respect
c                        to the control variables.
c
c
c     started: Christian Eckert eckert@mit.edu 15-Feb-2000
c
c     changed: Christian Eckert eckert@mit.edu 10-Mar-2000
c
c              - Added ECCO layout.
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE simul
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ctrl.h"

c     == routine arguments ==

      integer indic
      integer nn
      _RL   xx(nn)
      _RL   objf
      _RL   adxx(nn)

c     == local variables ==

      integer i
      _RL   adobjf

      logical lheaderonly

c     == end of interface ==

      print *, 'pathei-lsopt in simul'

c--   Call the combined modified forward model and the adjoint model.
      do i = 1,nn
        adxx(i) = 0.
      enddo

      adobjf = 1.
c
      lheaderonly = .false.

      print *, 'pathei-lsopt vor optim_readdata'

      call optim_readdata( nn, ctrlname, lheaderonly, objf,   xx )
      call optim_readdata( nn, costname, lheaderonly, objf, adxx )

      indic = indic

cph(
      print *, ' leaving simul with nn, objf = ', nn, objf
      print *, ' leaving simul with xx, adxx = ', xx(1), adxx(1)
      do i=1,nn
c         if (xx(i).EQ.'NaN') then
c            print *, 'pathei - out: i = ', i
c         end if
      end do
cph)

      return
      end
