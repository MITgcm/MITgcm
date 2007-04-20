
#include "CPP_OPTIONS.h"

      subroutine admtlm_init_fixed( myThid )
C     /==========================================================\
C     | subroutine admtlm_init_fixed                             |
C     | o This routine assigns final T,S to cost function        |
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_ADMTLM
# include "ctrl.h"
# include "ctrl_weights.h"
#endif

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_ADMTLM
C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj
      _RL dummy

      dummy = 0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c
c N.B. !!!
c Values here should be 1/(std.dev.)^2
c to be consistent with ctrl_set_*pack_xy* routines
c N.B. !!!
c
        DO k=1,Nr
         wtheta(k,bi,bj) = 1. / (1. _d 0)**2
         wsalt (k,bi,bj) = 1. / (0.1 _d 0)**2
         wuvvel(k,bi,bj) = 1. / (0.01 _d 0)**2
        END DO
c
        DO j=1,sNy
         DO i=1,sNx
          wetan(i,j,bi,bj) = 1. / (0.1 _d 0)**2
         END DO
        END DO
c
       END DO
      END DO

      call active_write_xy('wetan',wetan,1,0,mythid,dummy)

#endif
      
      end
