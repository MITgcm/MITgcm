
#include "CPP_OPTIONS.h"

      subroutine admtlm_metric( myThid )
C     /==========================================================\
C     | subroutine admtlm_metric                                 |
C     | o This routine assigns final T,S to cost function        |
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_ADMTLM
C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj
      _RL myMetric

      myMetric = 0. _d 0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        k=1
          DO j=1,sNy
          DO i=1,sNx
           myMetric = myMetric
     &            + theta(i,j,1,bi,bj)*theta(i,j,1,bi,bj)
     &            + salt(i,j,1,bi,bj)*salt(i,j,1,bi,bj)
          END DO
         END DO
       END DO
      END DO

      _GLOBAL_SUM_R8( myMetric , myThid )
      myMetric = myMetric/(nx*ny)

      print *, 'ph-myMetric = ', myMetric

#endif
      
      end
