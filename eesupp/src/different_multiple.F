C     $Header: /u/gcmpack/MITgcm/eesupp/src/different_multiple.F,v 1.1 1998/05/21 18:30:08 cnh Exp $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      LOGICAL FUNCTION DIFFERENT_MULTIPLE( freq, val1, val2 )
C     /==========================================================\
C     | LOGICAL FUNCTION DIFFERENT_MULTIPLE                      |
C     | o Checks two numbers multiple of a third number.         |
C     |==========================================================|
C     | This routine is used for diagnostic and other periodic   |
C     | operations. It is very sensitive to arithmetic precision.|
C     | For IEEE conforming arithmetic is works well but for     |
C     | cases where short cut arithmetic  is used it may not work|
C     | as expected. To overcome this issue compile this routine |
C     | separately with no optimisation.                         |
C     \==========================================================/
      IMPLICIT NONE
C     Returns TRUE if val1 and val2 are different multiples of freq.
      REAL freq, val1, val2
CEndofinterface
      DIFFERENT_MULTIPLE = .FALSE.
      IF ( freq .EQ. 0. ) THEN
        DIFFERENT_MULTIPLE = .FALSE.
      ELSE
        DIFFERENT_MULTIPLE = INT(val1/freq) .NE. INT(val2/freq)
      ENDIF
      END


