C $Header: /u/gcmpack/MITgcm/model/src/convective_adjustment_ini.F,v 1.3 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CONVECTIVE_ADJUSTMENT_INI
C     !INTERFACE:
      SUBROUTINE CONVECTIVE_ADJUSTMENT_INI(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CONVECTIVE_ADJUSTMENT_INI                      
C     | o Driver for vertical mixing or similar parameterization       
C     *==========================================================*
C     | Same prognostic code logic as S/R CONVECTIVE_ADJUSTMENT,  
C     | but different time history behavior in forward-reverse
C     | adjoint operation.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif 
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
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

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,k        :: Loop counters
C     rhoKm1, rhoK :: Density at adjacent levels (ref. to same level)
C     ConvectCount :: Convection freq. counter
      INTEGER i, j, K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
CEOP

C--   Check to see if should convect now
      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,myTime-deltaTClock) ) THEN

C-      Initialisation of Convection Counter
        DO K=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           ConvectCount(i,j,k) = 0.
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tapelev_ini_bibj_k   = USER

          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1

          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1

          act3 = myThid - 1
          max3 = nTx*nTy

          act4 = 0

          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  ALLOW_KPP
        IF ( 
     &      (.NOT.useKPP) ! CONVECT not needed with KPP mixing
     &     ) THEN
#endif  /* ALLOW_KPP */

C--       Loop over all *interior* layers
          DO K=2,Nr

#ifdef ALLOW_AUTODIFF_TAMC
            kkey = (ikey-1)*Nr + k
CADJ STORE theta(:,:,k-1,bi,bj) = tapelev_ini_bibj_k, 
CADJ &     key=kkey, byte=isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = tapelev_ini_bibj_k, 
CADJ &     key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Density of K-1 layer (above W(K)) reference to K-1 T-level
            CALL FIND_RHO(
     I           bi,bj,iMin,iMax,jMin,jMax,K-1,K-1,eosType,
     I           theta,salt,
     O           rhoKm1,
     I           myThid )

C-          Density of K layer (below W(K)) reference to K-1 T-level.
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj) = tapelev_ini_bibj_k, 
CADJ &     key = kkey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj) = tapelev_ini_bibj_k, 
CADJ &     key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FIND_RHO(
     I           bi,bj,iMin,iMax,jMin,jMax,K,K-1,eosType,
     I           theta,salt,
     O           rhoK,
     I           myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoKm1(:,:)  = tapelev_ini_bibj_k, key=kkey, byte=isbyte
CADJ STORE rhoK  (:,:)  = tapelev_ini_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Check static stability with layer below and mix as needed.
            CALL CONVECT(
     I           bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,rhoK,
     U           ConvectCount,
     I           myTime,myIter,myThid)

C--       End DO K=1,Nr
          ENDDO
       
#ifdef ALLOW_TIMEAVE 
          IF (myIter.ne.nIter0 .AND. taveFreq.GT.0.) THEN
            CALL TIMEAVE_CUMULATE(ConvectCountTave, ConvectCount, Nr,
     I                               deltaTclock, bi, bj, myThid)
          ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef  ALLOW_KPP
C--     End IF (.NOT.useKPP)
        ENDIF
#endif  /* ALLOW_KPP */

C--   End IF (DIFFERENT_MULTIPLE)
      ENDIF

#endif /* INCLUDE_CONVECT_CALL */

      RETURN
      END
