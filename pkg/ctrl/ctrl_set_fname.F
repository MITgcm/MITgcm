C
#include "CTRL_OPTIONS.h"

      subroutine ctrl_set_fname( xx_fname, fname, mythid )

c     ==================================================================
c     SUBROUTINE ctrl_set_fname
c     ==================================================================
c
c     o get filename for control variable and adjoint thereof
c     o initialise adjoint field
c
c     started: heimbach@mit.edu, 16-Aug-2001
c
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      character*(MAX_LEN_FNAM) xx_fname
      character*( 80)   fname(3)
      integer mythid

c     == local variables ==

      integer il

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      il=ilnblnk( xx_fname )

      write(fname(1)(1:80),'(80a)') ' '
      write(fname(2)(1:80),'(80a)') ' '
      write(fname(3)(1:80),'(80a)') ' '

      if ( il .GT. 0 .AND. il .LE. 80 ) then
         write(fname(1)(1:80),'(2a,i10.10)')
     &        xx_fname(1:il) ,'.', optimcycle
         write(fname(2)(1:80),'(3a,i10.10)')
     &        yadprefix, xx_fname(1:il), '.', optimcycle
         write(fname(3)(1:80),'(3a,i10.10)')
     &        'hn', xx_fname(1:il), '.', optimcycle
      endif

      _END_MASTER( mythid )

      end
