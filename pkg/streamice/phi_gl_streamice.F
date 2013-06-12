#include "CPP_OPTIONS.h"

      _RL FUNCTION PHI_GL_STREAMICE (X, W)

!      This function returns a "smoothed indicator function"
!      that is zero when x<0, 1 when x>w/2, and continuously diff.

       _RL X, W 
       _RL TMP1, PI

       IF (X<0.0) THEN
         PHI_GL_STREAMICE = 0.0
       ELSEIF(X>W/2.0) THEN
         PHI_GL_STREAMICE = 1.0
       ELSE
         PI = 3.14159265358979323844D0
         TMP1 = COS (2*PI*X/W)
         PHI_GL_STREAMICE = 0.5 * (1-TMP1)
       ENDIF

       RETURN
       END 