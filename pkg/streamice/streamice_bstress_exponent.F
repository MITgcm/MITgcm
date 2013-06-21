#include "CPP_OPTIONS.h"

      _RL FUNCTION STREAMICE_BSTRESS_EXP (speed,m)
 
       _RL speed, m
!       _RL SLOPE_LIMITER

        STREAMICE_BSTRESS_EXP = speed**(m-1)

       RETURN
       END 
