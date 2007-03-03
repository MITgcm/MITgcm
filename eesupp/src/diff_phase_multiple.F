C $Header: /u/gcmpack/MITgcm/eesupp/src/diff_phase_multiple.F,v 1.4 2007/03/03 22:35:46 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: DIFF_PHASE_MULTIPLE

C     !INTERFACE:
      LOGICAL FUNCTION DIFF_PHASE_MULTIPLE( phase, freq, val1, step )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | LOGICAL FUNCTION DIFF\_PHASE\_MULTIPLE                       
C     | o Checks if a multiple of freq (+ phase shift) exist
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
C     phase      :: shift phase time
C     freq       :: Frequency by which time is divided.
C     val1       :: time that is checked 
C     step       :: length of time interval (around val1) that is checked 
      _RL  phase, freq, val1, step

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     !LOCAL VARIABLES:
C     == Local variables ==
C     v1, v2, v3, v4 :: Temp. for holding time
C     d1, d2, d3     :: Temp. for hold difference
      _RL  v1, v2, v3, v4, d1, d2, d3
CEOP

C     o Do easy cases first.
      DIFF_PHASE_MULTIPLE = .FALSE.

      IF ( freq .NE. 0. ) THEN
        IF ( ABS(step) .GT. ABS(freq) ) THEN
         DIFF_PHASE_MULTIPLE = .TRUE.
c       ELSEIF ( val1+step .GE. phase+baseTime ) THEN
C-     should compare to phase+baseTime (above), but would need PARAMS.h ;
C      choose to disable this condition for negative time:
        ELSEIF ( val1+step.GE.phase .OR. val1.LT.0. ) THEN

C         o This case is more complex because of round-off error
          v1 = val1
          v2 = val1 - step
          v3 = val1 + step

C         Test v1 to see if its a "closest multiple"
          v4 = phase + NINT((v1-phase)/freq)*freq
          d1 = v1-v4
          d2 = v2-v4
          d3 = v3-v4
          IF ( ABS(d1) .LT. ABS(d2) .AND. ABS(d1) .LE. ABS(d3) )
     &        DIFF_PHASE_MULTIPLE = .TRUE.

        ENDIF
      ENDIF

      RETURN
      END
