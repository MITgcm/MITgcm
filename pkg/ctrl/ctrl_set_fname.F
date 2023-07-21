#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_SET_FNAME
C     !INTERFACE:
      SUBROUTINE CTRL_SET_FNAME( xx_fname, fname, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE CTRL_SET_FNAME
C     ==================================================================
C     o get filename for control variable and adjoint thereof
C     started: heimbach@mit.edu, 16-Aug-2001
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS
      CHARACTER*(*) xx_fname
      CHARACTER*(MAX_LEN_FNAM) fname(3)
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER il, ic, l
CEOP

      il = ILNBLNK( xx_fname )

      IF ( il .GT. 0 .AND. (il+13) .LE. MAX_LEN_FNAM ) THEN
        ic = 0
        l = il
        DO WHILE ( ic .EQ. 0 .AND. l .GE. 1 )
          IF ( xx_fname(l:l) .EQ. '/' ) ic = l
          l = l -1
        ENDDO

        WRITE(fname(1),'(2A,I10.10)')
     &        xx_fname(1:il), '.', optimcycle
        WRITE(fname(2),'(4A,I10.10)')
     &        xx_fname(1:ic), yadprefix, xx_fname(ic+1:il), '.',
     &        optimcycle
        WRITE(fname(3),'(4A,I10.10)')
     &        xx_fname(1:ic), 'hn', xx_fname(ic+1:il), '.',
     &        optimcycle

      ELSE
        STOP 'ABNORMAL END: S/R CTRL_SET_FNAME'
      ENDIF

      RETURN
      END
