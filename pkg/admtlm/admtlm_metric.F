
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
#ifdef ALLOW_ADMTLM
# include "cost.h"
# include "g_cost.h"
#endif

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_ADMTLM
C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj
      _RL myMetric
      _RL stdDevSST
      _RL stdDevSSS

      myMetric  = 0. _d 0
      stdDevSST = 0.52 _d 0
      stdDevSSS = 0.134 _d 0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          myMetric = myMetric
     &           + g_objf_state_final(i,j,bi,bj,1)**2 / stdDevSST**2
     &           + g_objf_state_final(i,j,bi,bj,2)**2 / stdDevSSS**2
          END DO
        END DO
       END DO
      END DO

      _GLOBAL_SUM_R8( myMetric , myThid )
      myMetric = myMetric/(nx*ny)

cph(
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
            print '(a,2I4,2(x,F15.9))', 'ph-metric T ', i, j, 
     &           g_objf_state_final(i,j,bi,bj,1),
     &           g_objf_state_final(i,j,bi,bj,1)**2 / stdDevSST**2
            print '(a,2I4,2(x,F15.9))', 'ph-metric S ', i, j, 
     &           g_objf_state_final(i,j,bi,bj,2),
     &           g_objf_state_final(i,j,bi,bj,2)**2 / stdDevSSS**2
          END DO
        END DO
       END DO
      END DO

      print *, 'ph-admtlm myMetric = ', myMetric

c      STOP 'ph in admtlm_metric'
cph)

#endif
      
      end
