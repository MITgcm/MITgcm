C $Header: /u/gcmpack/MITgcm/pkg/admtlm/admtlm_map.F,v 1.5 2012/08/12 18:29:25 jmc Exp $
C $Name:  $

#include "ADMTLM_OPTIONS.h"

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
          DO k=1,4*Nr
            adobjf_state_final(i,j,bi,bj,k) =
     &       g_objf_state_final(i,j,bi,bj,k)
          END DO
           adobjf_state_final(i,j,bi,bj,4*Nr+1) =
     &      g_objf_state_final(i,j,bi,bj,4*Nr+1)
         END DO
        END DO
       END DO
      END DO

#endif

      end
