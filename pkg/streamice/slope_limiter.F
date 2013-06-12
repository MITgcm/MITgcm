#include "CPP_OPTIONS.h"

!      _RL FUNCTION SLOPE_LIMITER (num, denom)

       _RL FUNCTION SLOPE_LIMITER (Cr)

!       _RL num, denom
!       _RL SLOPE_LIMITER
!       _RL r
       _RL Cr

! used for flux limiting in advective subroutines
! Van Leer limiter (source: Wikipedia!)

!       if (denom .eq. 0.0) then
!        SLOPE_LIMITER = 0.0
!       elseif (num*denom .le. 0.0) then
!        SLOPE_LIMITER = 0.0
!       else
!        r = num/denom
!        SLOPE_LIMITER = (r+abs(r))/(1+abs(r))
!       endif

       SLOPE_LIMITER = max(0.D0,max(min(1.D0,2.D0*Cr),
     &                 min(2.D0,Cr)))


       RETURN
       END 
