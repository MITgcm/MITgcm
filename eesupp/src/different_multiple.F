C     $Header: /u/gcmpack/MITgcm/eesupp/src/different_multiple.F,v 1.3 1999/05/24 14:57:55 adcroft Exp $
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
      _RL  freq, val1, val2
CEndofinterface
      _RL  f, v1, v2, v3, v4, d1, d2, d3, step

C     o Do easy cases first.
      DIFFERENT_MULTIPLE = .FALSE.

      IF ( freq .NE. 0. ) THEN
        IF ( ABS(val1-val2) .GT. freq ) THEN
         DIFFERENT_MULTIPLE = .TRUE.
        ELSE

C         o This case is more complex because of round-off error
          f = freq
          v1 = val1
          v2 = val2
          step = v1-v2

C         Test v1 to see if its a "closest multiple"
          v3 = v1 + step
          v4 = NINT(v1/f)*f
          d1 = v1-v4
          d2 = v2-v4
          d3 = v3-v4
          IF ( ABS(d1) .LE. ABS(d2) .AND. ABS(d1) .LE. ABS(d3) )
     &        DIFFERENT_MULTIPLE = .TRUE.

        ENDIF ! |val1-val2| > freq
      ENDIF ! freq != 0

      END
