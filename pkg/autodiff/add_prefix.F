#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: ADD_PREFIX
C     !INTERFACE:
      SUBROUTINE ADD_PREFIX( prefix, actvarf, fname )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ADD_PREFIX
C     ==================================================================
C     o Helper S/R to add 2 character prefix to file-name
C     started: ou.wang@jpl.nasa.gov 24-Aug-2022
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     prefix  :: (input)  2 character prefix to add
C     actvarf :: (input)  active-variable file-name
C     fname   :: (output) file-name with added prefix
      CHARACTER*(2) prefix
      CHARACTER*(*) actvarf
      CHARACTER*(*) fname

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER il, ll, l
CEOP

      il = ILNBLNK( actvarf )
      ll = 0
      IF ( il .GT. 0 .AND. il .LE. 80 ) THEN
        l = il
        DO WHILE ( ll .EQ. 0 .AND. l .GE. 1 )
          IF ( actvarf(l:l) .EQ. '/' ) ll = l
          l = l -1
        ENDDO
      ELSE
        STOP 'ABNORMAL END: S/R ADD_PREFIX'
      ENDIF
      WRITE(fname,'(3A)') actvarf(1:ll), prefix, actvarf(ll+1:il)

      RETURN
      END
