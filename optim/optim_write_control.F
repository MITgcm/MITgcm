
      subroutine optim_write_control(
     I                          nv,
     I                          xx
     &                        ) 

c     ==================================================================
c     SUBROUTINE optim_write_control
c     ==================================================================
c
c     o Interface routine for writing the next estimate of the control
c       vector that is to be used by the ocean state estimation program.
c
c     started: Christian Eckert eckert@mit.edu 12-Apr-2000
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE optim_write_control
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ctrl.h"

c     == routine arguments ==

      integer nv
      _RL     xx(nv)

c     == local variables ==

      _RL           ff

      logical lheaderonly

c     == end of interface ==

      ff       = -9999.

      print *,'pathei: writing ', nv,' sized control to file',
     &     ctrlname

      lheaderonly = .false.
      call optim_writedata( nv, ctrlname, lheaderonly, ff, xx )

      return
      end

