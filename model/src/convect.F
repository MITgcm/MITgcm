C $Header: /u/gcmpack/MITgcm/model/src/Attic/convect.F,v 1.18 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CONVECT
C     !INTERFACE:
      SUBROUTINE CONVECT( bi, bj, iMin, iMax, jMin, jMax, K,
     &       rhoKm1, rhoKp1, ConvectCount,
     &       myTime,myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CONVECT                                        
C     | o Does vertical mixing of unstable water columns          
C     *==========================================================*
C     | Uses simple homgenisation scheme between unstable 
C     | layers.
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
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
#endif
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K - Loop counters
C     rhoKm1 - rho in layer above
C     rhoKp1 - rho in layer below
C     myTime - Current time in simulation
C     myIter - Current iteration in simulation
C     myThid - Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j    :: Loop counter
C     tMix   :: Temp. for calculation of mixed theta.
C     sMix   :: Temp. for calculation of mixed salt. 
C     tr1Mix :: Temp. for calculation of mixed tracer.
C     dSum   :: Temp. for layer thicknesses
      INTEGER i,j
      _RL tMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_PASSIVE_TRACER
      _RL tr1Mix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
      _RL dSum(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C--   Check to see if should convect now
      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,myTime-deltaTClock) ) THEN

C--    Calculate heat content of two level column
       DO j=jMin,jmax
        DO i=iMin,imax
         tMix(i,j) = theta(i,j,k-1,bi,bj)
     &               *_hFacC(i,j,k-1,bi,bj)*drF(k-1)
     &              +theta(i,j,k,bi,bj)
     &               *_hFacC(i,j,k,bi,bj)*drF(k)
         sMix(i,j) = salt (i,j,k-1,bi,bj)
     &               *_hFacC(i,j,k-1,bi,bj)*drF(k-1)
     &              +salt (i,j,k,bi,bj)
     &               *_hFacC(i,j,k,bi,bj)*drF(k)
#ifdef ALLOW_PASSIVE_TRACER
         tr1Mix(i,j) = Tr1 (i,j,k-1,bi,bj)
     &               *_hFacC(i,j,k-1,bi,bj)*drF(k-1)
     &              +Tr1  (i,j,k,bi,bj)
     &               *_hFacC(i,j,k,bi,bj)*drF(k)
#endif
         dSum(i,j) = _hFacC(i,j,k-1,bi,bj)*drF(k-1)
     &              +_hFacC(i,j,k,bi,bj)*drF(k)
        ENDDO
       ENDDO

C--    Where statically unstable, mix the heat and salt
       DO j=jMin,jmax
        DO i=iMin,imax
         IF ( _hFacC(i,j,k,bi,bj) .GT. 0. .AND.
     &        rhokm1(i,j) .GT. rhokp1(i,j) ) THEN
          theta(i,j,k-1,bi,bj) = tMix(i,j)/dSum(i,j)
          theta(i,j,k  ,bi,bj) = tMix(i,j)/dSum(i,j)
          salt(i,j,k-1,bi,bj)  = sMix(i,j)/dSum(i,j)
          salt(i,j,k  ,bi,bj)  = sMix(i,j)/dSum(i,j)
#ifdef ALLOW_PASSIVE_TRACER
          Tr1(i,j,k-1,bi,bj)   = Tr1Mix(i,j)/dSum(i,j)
          Tr1(i,j,k  ,bi,bj)   = Tr1Mix(i,j)/dSum(i,j)
#endif
          ConvectCount(i,j,k) = ConvectCount(i,j,k) + 1.
         ENDIF
        ENDDO
       ENDDO
       
      ENDIF

      RETURN
      END
