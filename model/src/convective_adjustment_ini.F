C $Header: /u/gcmpack/MITgcm/model/src/convective_adjustment_ini.F,v 1.23 2014/04/04 20:54:11 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CONVECTIVE_ADJUSTMENT_INI
C     !INTERFACE:
      SUBROUTINE CONVECTIVE_ADJUSTMENT_INI(
     I                      bi, bj, myTime, myIter, myThid )
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
#ifdef ALLOW_AUTODIFF
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  :: tile indices
C     myTime :: Current time in simulation
C     myIter :: Current iteration in simulation
C     myThid :: Thread number of this instance of S/R CONVECT
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef INCLUDE_CONVECT_CALL

C     !FUNCTIONS:
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iMin,iMax,jMin,jMax :: computation domain
C     i,j,k        :: Loop counters
C     rhoKm1, rhoK :: Density at adjacent levels (ref. to same level)
C     ConvectCount :: Convection freq. counter
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i, j, K, kTop, kBottom, kDir, deltaK
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL weightA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL weightB(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C--   Check to see if should convect now
      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,deltaTClock)
     &   ) THEN

C--   Define computation domain
        iMin=1-OLx
        iMax=sNx+OLx
        jMin=1-OLy
        jMax=sNy+OLy

C--   Initialise counters
        kTop    = 0
        kBottom = 0
        kDir    = 0
        deltaK  = 0

C-      Initialisation of Convection Counter
        DO K=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           ConvectCount(i,j,k) = 0.
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
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

         IF ( rkSign*gravitySign .GT. 0. ) THEN
C-          <=> usingZCoords:
          kTop    =  2
          kBottom = Nr
          kDir    =  1
          deltaK  = -1
         ELSE
C-          <=> usingPCoords:
          kTop    = Nr
          kBottom =  2
          kDir    = -1
          deltaK  =  0
         ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)  = tapelev_ini_bibj,
CADJ &     key=ikey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj)  = tapelev_ini_bibj,
CADJ &     key=ikey, byte=isbyte
CADJ STORE convectcount(:,:,:) = tapelev_ini_bibj,
CADJ &     key=ikey, byte=isbyte
#endif

C--       Loop over all *interior* layers
          DO K=kTop,kBottom,kDir

#ifdef ALLOW_AUTODIFF_TAMC
            kkey = (ikey-1)*Nr + k
CADJ STORE theta(:,:,k-1,bi,bj) = tapelev_ini_bibj_k,
CADJ &     key=kkey, byte=isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = tapelev_ini_bibj_k,
CADJ &     key=kkey, byte=isbyte
CADJ STORE convectcount(:,:,k-1) = tapelev_ini_bibj_k,
CADJ &     key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Density of K-1 layer (above W(K)) reference to K-1 T-level
            CALL FIND_RHO_2D(
     I           iMin, iMax, jMin, jMax, K+deltaK,
     I           theta(1-OLx,1-OLy,K-1,bi,bj),
     I           salt (1-OLx,1-OLy,K-1,bi,bj),
     O           rhoKm1,
     I           K-1, bi, bj, myThid )

C-          Density of K layer (below W(K)) reference to K-1 T-level.
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj) = tapelev_ini_bibj_k,
CADJ &     key = kkey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj) = tapelev_ini_bibj_k,
CADJ &     key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FIND_RHO_2D(
     I           iMin, iMax, jMin, jMax, K+deltaK,
     I           theta(1-OLx,1-OLy,K,bi,bj),
     I           salt (1-OLx,1-OLy,K,bi,bj),
     O           rhoK,
     I           K, bi, bj, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoKm1(:,:)  = tapelev_ini_bibj_k, key=kkey, byte=isbyte
CADJ STORE rhoK  (:,:)  = tapelev_ini_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Check static stability with layer below and mix as needed.
c           CALL CONVECT(
c    I           bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,rhoK,
c    U           ConvectCount,
c    I           myTime,myIter,myThid)

C-          Pre-calculate mixing weights for interface K
            CALL CONVECTIVE_WEIGHTS(
     I           bi,bj,K,rhoKm1,rhoK,
     O           weightA,weightB,ConvectCount,
     I           myThid)

C-          Convectively mix heat across interface K
            CALL CONVECTIVELY_MIXTRACER(
     I                              bi,bj,k,weightA,weightB,
     U                              theta,
     I                              myThid)

C-          Convectively mix salt across interface K
            CALL CONVECTIVELY_MIXTRACER(
     I                              bi,bj,k,weightA,weightB,
     U                              salt,
     I                              myThid)

#ifdef ALLOW_PTRACERS
C-          Convectively mix passive tracers across interface K
            IF ( usePTRACERS ) THEN
              CALL PTRACERS_CONVECT(
     I                              bi,bj,k,weightA,weightB,myThid)
            ENDIF
#endif /* ALLOW_PTRACERS */

C--       End DO K=1,Nr
          ENDDO

C--   End IF (DIFFERENT_MULTIPLE)
      ENDIF

#endif /* INCLUDE_CONVECT_CALL */

      RETURN
      END
