
#include "CPP_OPTIONS.h"

      subroutine admtlm_map( myThid )
C     /==========================================================\
C     | subroutine admtlm_map                                    |
C     | o This routine assigns final T,S to cost function        |
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_ADMTLM
# include "adcost.h"
# include "g_cost.h"
#endif

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_ADMTLM
C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           adobjf_state_final(i,j,bi,bj,1) = 
     &      g_objf_state_final(i,j,bi,bj,1)
           adobjf_state_final(i,j,bi,bj,2) = 
     &      g_objf_state_final(i,j,bi,bj,2)
          END DO
         END DO
       END DO
      END DO

#endif
      
      end
