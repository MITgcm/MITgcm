C     $Header: /u/gcmpack/MITgcm/eesupp/src/different_multiple.F,v 1.5 2001/09/21 03:54:34 cnh Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: DIFFERENT_MULTIPLE

C     !INTERFACE:
      LOGICAL FUNCTION DIFFERENT_MULTIPLE( freq, val1, val2 )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | LOGICAL FUNCTION DIFFERENT_MULTIPLE                       
C     | o Checks two numbers multiple of a third number.          
C     *==========================================================*
C     | This routine is used for diagnostic and other periodic    
C     | operations. It is very sensitive to arithmetic precision. 
C     | For IEEE conforming arithmetic it works well but for      
C     | cases where short cut arithmetic  is used it may not work 
C     | as expected. To overcome this issue compile this routine  
C     | separately with no optimisation.                          
C     *==========================================================*

C     !INPUT PARAMETERS:
C     == Routine arguments ==
C     val1, val2 :: Two times that are checked 
C     freq       :: Frequency by which times are divided.
      _RL  freq, val1, val2

C     !LOCAL VARIABLES:
C     == Local variables ==
C     f              :: Temp. for holding freq
C     v1, v2, v3, v4 :: Temp. for holding time
C     d1, d2, d3     :: Temp. for hold difference
C     step           :: Temp. for hold difference used as increment
      _RL  f, v1, v2, v3, v4, d1, d2, d3, step
CEOP

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
