C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_zonalmean.F,v 1.5 2011/08/28 21:46:45 jmc Exp $
C $Name:  $

#include "EBM_OPTIONS.h"

CBOP 0
C !ROUTINE: EBM_ZONALMEAN

C !INTERFACE:
      SUBROUTINE EBM_ZONALMEAN( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R CALCULATE ZONAL MEAN TEMPERATURE
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT PARAMETERS:
C     === Routine arguments ===
C     myThid   :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
C     i, j, k :: Loop counters
      INTEGER i, j, k, bi, bj
      _RL locSumMask(1-OLy:sNy+OLy,nSy)
      _RL locSumSST (1-OLy:sNy+OLy,nSy)

C- multi-threading note:
C     global-sum cannot be applied to shared var. (i.e. in common block)
C     (problem in earlier version if nTx > 1) and all threads should call
C     global-sum in the same order (problem in earlier version if nTy > 1).
C     For these reasons, we define local array (locSumMask,locSumSST) to perform
C     global sum. They need to be initialised not only for bj(myThid) but for all
C     bj and after applying global-sum (to all bj), all threads will get the right
C     results, which the master thread can copy to shared array.

C--   Top layer only
      k = 1

c--   Initialise
      DO bj=1,nSy
       DO j=1-OLy,sNy+OLy
         locSumMask(j,bj) = 0.
         locSumSST (j,bj) = 0.
       ENDDO
      ENDDO

C--   Calculate the zonal mean
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i=1,sNx
          locSumMask(j,bj) = locSumMask(j,bj)
     &                     + maskC(i,j,k,bi,bj)
          locSumSST(j,bj)  = locSumSST(j,bj)
     &                     + maskC(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=1,nSy
       DO j=1-OLy,sNy+OLy
        _GLOBAL_SUM_RL( locSumMask(j,bj), myThid )
        _GLOBAL_SUM_RL( locSumSST(j,bj) , myThid )
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE CountX = comlev1, key = ikey_dynamics
#endif
      _BEGIN_MASTER(myThid)
      DO bj=1,nSy
       DO j=1-OLy,sNy+OLy
        CountX(j,bj) = locSumMask(j,bj)
        ZonalMeanSST(j,bj) = locSumSST(j,bj)
        IF ( CountX(j,bj).GT.0. _d 0 ) THEN
          ZonalMeanSST(j,bj) = ZonalMeanSST(j,bj)/CountX(j,bj)
        ENDIF
       ENDDO
      ENDDO
      _END_MASTER(myThid)
      _BARRIER

      IF ( tauThetaZonRelax .NE. 0. _d 0 ) THEN
C-    replace SST with ZonalMeanSST for relaxation towards Zonal-Mean value
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            SST(i,j,bi,bj) = ZonalMeanSST(j,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_EBM */

      RETURN
      END
