C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/diff_base_multiple.F,v 1.1 2005/04/06 18:17:28 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: DIFF_BASE_MULTIPLE

C     !INTERFACE:
      LOGICAL FUNCTION DIFF_BASE_MULTIPLE( base, freq, val1, step )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | LOGICAL FUNCTION DIFF\_BASE\_MULTIPLE                       
C     | o Checks if a multiple of freq (+ base shift) exist
C     |   around val1 +/- step/2
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
C     base       :: shift base time
C     freq       :: Frequency by which time is divided.
C     val1       :: time that is checked 
C     step       :: length of time interval (around val1) that is checked 
      _RL  base, freq, val1, step

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     !LOCAL VARIABLES:
C     == Local variables ==
C     v1, v2, v3, v4 :: Temp. for holding time
C     d1, d2, d3     :: Temp. for hold difference
      _RL  v1, v2, v3, v4, d1, d2, d3
CEOP

C     o Do easy cases first.
      DIFF_BASE_MULTIPLE = .FALSE.

      IF ( freq .NE. 0. ) THEN
        IF ( ABS(step) .GT. freq ) THEN
         DIFF_BASE_MULTIPLE = .TRUE.
        ELSE

C         o This case is more complex because of round-off error
          v1 = val1
          v2 = val1 - step
          v3 = val1 + step

C         Test v1 to see if its a "closest multiple"
          v4 = base + NINT((v1-base)/freq)*freq
          d1 = v1-v4
          d2 = v2-v4
          d3 = v3-v4
          IF ( ABS(d1) .LE. ABS(d2) .AND. ABS(d1) .LE. ABS(d3) )
     &        DIFF_BASE_MULTIPLE = .TRUE.

        ENDIF
      ENDIF

      RETURN
      END
