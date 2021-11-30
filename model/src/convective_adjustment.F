#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CONVECTIVE_ADJUSTMENT
C     !INTERFACE:
      SUBROUTINE CONVECTIVE_ADJUSTMENT(
     I                      bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CONVECTIVE_ADJUSTMENT
C     | o Driver for vertical mixing or similar parameterization
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
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  :: tile indices
C     myTime :: Current time in simulation
C     myIter :: Current iteration in simulation
C     myThid :: My Thread Id number
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
C     rhoKm1, rhoK :: Density at adjacent levels (common ref. level)
C     ConvectCount :: Convection mixing freq. counter.
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i, j, k, kTop, kBottom, kDir, deltaK
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey, kkey
#endif
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
        DO k=1,Nr
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
        act4 = ikey_dynamics - 1
        ikey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
     &                    + act4*max1*max2*max3
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

C--       Loop over all *interior* layers
        DO k=kTop,kBottom,kDir

#ifdef ALLOW_AUTODIFF_TAMC
         kkey = (ikey-1)*Nr + k
C     It is important that the two k-levels of these fields are stored
C     in one statement because otherwise taf will only store one, which
C     is wrong (i.e. was wrong in previous versions).
CADJ STORE theta(:,:,k-1,bi,bj), theta(:,:,k,bi,bj) =
CADJ &     comlev1_bibj_k, key = kkey, kind = isbyte
CADJ STORE salt(:,:,k-1,bi,bj), salt(:,:,k,bi,bj) =
CADJ &     comlev1_bibj_k, key = kkey, kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C-          Density of k-1 layer (above W(k)) reference to k-1 T-level
         CALL FIND_RHO_2D(
     I        iMin, iMax, jMin, jMax, k+deltaK,
     I        theta(1-OLx,1-OLy,k-1,bi,bj),
     I        salt (1-OLx,1-OLy,k-1,bi,bj),
     O        rhoKm1,
     I        k-1, bi, bj, myThid )

C-          Density of k layer (below W(k)) reference to k-1 T-level.
         CALL FIND_RHO_2D(
     I        iMin, iMax, jMin, jMax, k+deltaK,
     I        theta(1-OLx,1-OLy,k,bi,bj),
     I        salt (1-OLx,1-OLy,k,bi,bj),
     O        rhoK,
     I        k, bi, bj, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoKm1 = comlev1_bibj_k, key = kkey, kind = isbyte
CADJ STORE rhoK   = comlev1_bibj_k, key = kkey, kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C-          Pre-calculate mixing weights for interface k
         CALL CONVECTIVE_WEIGHTS(
     I        bi,bj,k,rhoKm1,rhoK,
     O        weightA,weightB,ConvectCount,
     I        myThid)

C-          Convectively mix heat across interface k
         CALL CONVECTIVELY_MIXTRACER(
     I                               bi,bj,k,weightA,weightB,
     U                               theta,
     I                               myThid)

C-          Convectively mix salt across interface k
         CALL CONVECTIVELY_MIXTRACER(
     I                               bi,bj,k,weightA,weightB,
     U                               salt,
     I                               myThid)

#ifdef ALLOW_PTRACERS
C-          Convectively mix passive tracers across interface k
         IF ( usePTRACERS ) THEN
          CALL PTRACERS_CONVECT(
     I                          bi,bj,k,weightA,weightB,myThid)
         ENDIF
#endif /* ALLOW_PTRACERS */

C--       End DO k=1,Nr
        ENDDO

#ifdef ALLOW_TIMEAVE
        IF (myIter.NE.nIter0 .AND. taveFreq.GT.0.) THEN
         CALL TIMEAVE_CUMUL_1T(ConvectCountTave, ConvectCount,
     I                         Nr, deltaTClock, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
        IF ( myIter.NE.nIter0 .AND. useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL( ConvectCount, 'CONVADJ ',
     I                          0, Nr, 2, bi, bj, myThid )
        ENDIF
#endif /*  ALLOW_DIAGNOSTICS */

C--   End IF (DIFFERENT_MULTIPLE)
      ENDIF

#endif /* INCLUDE_CONVECT_CALL */

      RETURN
      END
