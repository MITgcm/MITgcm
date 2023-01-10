c$Id$
      BLOCK DATA validityTestBD
C Keeps the current bounds of the validity interval
C Initial value is ]-infinity, +infinity[
      REAL gmin, gmax
      LOGICAL infmin, infmax
      COMMON /validity_test_common/ 
     +     gmin, gmax, infmin, infmax
      DATA infmin/.TRUE./
      DATA infmax/.TRUE./
      DATA gmin/-999.99/
      DATA gmax/999.99/
      END

      SUBROUTINE validity_domain_real8(t, td)
C Updates the bounds of the validity interval
C with the new constraint that t keeps its sign.
      real*8 t, td
      real gmin, gmax, temp
      logical infmin, infmax
      COMMON /validity_test_common/ 
     +     gmin, gmax, infmin, infmax

      if(td .ne. 0.0) then
       temp = -(t/td)
       if ( temp .lt. 0.0 ) then
         if ( infmin ) then
            gmin = temp
            infmin = .FALSE.
         else
            gmin = max(gmin,temp)
         endif
       else
         if ( infmax ) then
            gmax = temp
            infmax = .FALSE.
         else
            gmax = min(gmax,temp)
         endif
       endif
      endif
      end

      SUBROUTINE validity_domain_real4(t, td)
C Updates the bounds of the validity interval
C with the new constraint that t keeps its sign.
      real*4 t, td
      real gmin, gmax, temp
      logical infmin, infmax
      COMMON /validity_test_common/ 
     +     gmin, gmax, infmin, infmax

      if(td .ne. 0.0) then
       temp = -(t/td)
       if ( temp .lt. 0.0 ) then
         if ( infmin ) then
            gmin = temp
            infmin = .FALSE.
         else
            gmin = max(gmin,temp)
         endif
       else
         if ( infmax ) then
            gmax = temp
            infmax = .FALSE.
         else
            gmax = min(gmax,temp)
         endif
       endif
      endif
      end
