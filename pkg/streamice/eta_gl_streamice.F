C $Header: /u/gcmpack/MITgcm/pkg/streamice/eta_gl_streamice.F,v 1.2 2013/06/21 18:38:45 dgoldberg Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      _RL FUNCTION ETA_GL_STREAMICE (X, M1, M2, Y0, W)

!      This function smooths out slope discontinuities in a 1D function
!      y(x) is assumed piecewise linear with a slope discontinuity at 0
!      the results is smoothed only on -w/2 < x < w/2

!     X: input variable
!     M1: slope of y where x<0
!     M2: slope of y where x>0
!     Y0: value of y at 0
!     W: width of smoothing

       _RL X, M1, M2, Y0, W
       _RL TMP1, PI

       IF (X.le.-0.5*W) THEN
         ETA_GL_STREAMICE = Y0 + M1 * X
       ELSEIF(X.gt.0.5*W) THEN
         ETA_GL_STREAMICE = Y0 + M2 * X
       ELSE
         PI = 3.14159265358979323844D0
         TMP1 = W/PI * COS(PI*X/W)
         ETA_GL_STREAMICE = Y0 + 
     &     M1/2 * (X-0.5*W+TMP1) + 
     &     M2/2 * (X+0.5*W-TMP1)
       ENDIF

       RETURN
       END 
