C $Header: /u/gcmpack/MITgcm/model/src/convective_adjustment.F,v 1.3 2001/02/04 14:38:46 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CONVECTIVE_ADJUSTMENT(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CONVECTIVE_ADJUSTMENT                         |
C     | o Calls vertical mixing or similar parameterization      |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K - Loop counters
C     myTime - Current time in simulation
C     myIter - Current iteration in simulation
C     myThid - Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef INCLUDE_CONVECT_CALL

C     == Local variables ==
      INTEGER K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C--   Check to see if should convect now
      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,myTime-deltaTClock) ) THEN

#ifdef  ALLOW_KPP
        IF ( 
     &      (.NOT.useKPP) ! CONVECT not needed with KPP mixing
     &     ) THEN
#endif  /* ALLOW_KPP */

C--       Loop over all *interior* layers
          DO K=2,Nr

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k-1,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Density of K-1 layer (above W(K)) reference to K-1 T-level
            CALL FIND_RHO(
     I           bi,bj,iMin,iMax,jMin,jMax,K-1,K-1,eosType,
     I           theta,salt,
     O           rhoKm1,
     I           myThid )

C-          Density of K layer (below W(K)) reference to K-1 T-level.
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FIND_RHO(
     I           bi,bj,iMin,iMax,jMin,jMax,K,K-1,eosType,
     I           theta,salt,
     O           rhoK,
     I           myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoKm1(:,:)  = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE rhoKp1(:,:)  = comlev1_bibj, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Check static stability with layer below and mix as needed.
            CALL CONVECT(
     I           bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,rhoK,
     U           ConvectCount,
     I           myTime,myIter,myThid)

C--       End DO K=1,Nr
          ENDDO
       
#ifdef  ALLOW_KPP
C--     End IF (.NOT.useKPP)
        ENDIF
#endif  /* ALLOW_KPP */

C--   End IF (DIFFERENT_MULTIPLE)
      ENDIF

#endif /* INCLUDE_CONVECT_CALL */

      RETURN
      END
