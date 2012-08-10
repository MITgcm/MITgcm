C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_state_final.F,v 1.7 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      subroutine cost_state_final( myThid )
C     /==========================================================\
C     | subroutine cost_state_final                              |
C     | o This routine assigns final T,S to cost function        |
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "cost.h"

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_STATE_FINAL
C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          DO k=1,nr
           objf_state_final(i,j,bi,bj,0*Nr+k) = theta(i,j,k,bi,bj)
           objf_state_final(i,j,bi,bj,1*Nr+k) = salt(i,j,k,bi,bj)
           objf_state_final(i,j,bi,bj,2*Nr+k) = uvel(i,j,k,bi,bj)
           objf_state_final(i,j,bi,bj,3*Nr+k) = vvel(i,j,k,bi,bj)
          END DO
          objf_state_final(i,j,bi,bj,4*Nr+1) = etan(i,j,bi,bj)
         END DO
        END DO
       END DO
      END DO

cph      fc = objf_state_final(45,4,1,1,1)
cph      _GLOBAL_SUM_RL( fc , myThid )
cph      print *, 'fc for admtlm test at 83,33 : ', fc

#endif

      end
