C
#include "CTRL_OPTIONS.h"

      SUBROUTINE ctrl_set_fname( xx_fname, fname, mythid )

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

      CHARACTER*(*) xx_fname
      CHARACTER*( 80)   fname(3)
      integer mythid

c     == local variables ==

      INTEGER il
      INTEGER ll,l

c     == external ==

      INTEGER  ilnblnk
      EXTERNAL ilnblnk

c     == end of interface ==

c--   Only the master thread will do I/O.
      _BEGIN_MASTER( mythid )

      il=ilnblnk( xx_fname )

      WRITE(fname(1)(1:80),'(80a)') ' '
      WRITE(fname(2)(1:80),'(80a)') ' '
      WRITE(fname(3)(1:80),'(80a)') ' '

      IF ( il .GT. 0 .AND. il .LE. 80 ) THEN
         ll = 0
         DO l = il, 1, -1
          IF ( xx_fname(l:l) .EQ. '/' ) THEN
           ll = l
           GO TO 11
          ENDIF
         ENDDO
 11      CONTINUE

         WRITE(fname(1)(1:80),'(2a,i10.10)')
     &        xx_fname(1:il) ,'.', optimcycle
         WRITE(fname(2)(1:80),'(3a,i10.10)')
     &        xx_fname(1:ll)//yadprefix,xx_fname(ll+1:il), '.', 
     &        optimcycle
         WRITE(fname(3)(1:80),'(3a,i10.10)')
     &        xx_fname(1:ll)//'hn', xx_fname(ll+1:il), '.', 
     &        optimcycle
      ELSE
         STOP 'ABNORMAL END: S/R CTRL_SET_FNAME'
      ENDIF

      _END_MASTER( mythid )

      END
