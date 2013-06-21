C $Header: /u/gcmpack/MITgcm/pkg/streamice/eta_gl_streamice_prime.F,v 1.2 2013/06/21 18:38:45 dgoldberg Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      _RL FUNCTION ETA_GL_STREAMICE_PRIME (X, M1, M2, Y0, W)

!      This function returns the derivative of ETA_GL_STREAMICE
!      w.r.t. its first argument


!     X: input variable
!     M1: slope of y where x<0
!     M2: slope of y where x>0
!     Y0: value of y at 0
!     W: width of smoothing

       _RL X, M1, M2, Y0, W
       _RL TMP1, PI


       IF (X.le.-0.5*W) THEN
         ETA_GL_STREAMICE_PRIME = M1
       ELSEIF(X.gt.0.5*W) THEN
         ETA_GL_STREAMICE_PRIME = M2
       ELSE
         PI = 3.14159265358979323844D0
         TMP1 = SIN (PI*X/W)
         ETA_GL_STREAMICE_PRIME = 
     &     0.5*M1 * (1-TMP1) + 
     &     0.5*M2 * (1+TMP1)
       ENDIF

       RETURN
       END 
