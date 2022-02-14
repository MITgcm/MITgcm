#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CALC( futureTime, futureIter,
     &                      uVel, vVel, wVel, theta, salt,
     &                      myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_CALC
C     | o Calculate future boundary data at open boundaries
C     |   at time = futureTime
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
#endif /* ALLOW_EXCH2 */
#include "SET_GRID.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "EOS.h"

C     == Routine arguments ==
      INTEGER futureIter
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER bi, bj
      INTEGER I, J ,K
      _RL obTimeScale,Uinflow,rampTime2
      _RL vertStructWst(Nr)
      _RL mz,strat,kx
      _RL tmpsum

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_CALC',myThid)
#endif

C Vertical mode number
      mz=1.0 _d 0
C Stratification
      strat = 1.0 _d -6 / (gravity*tAlpha)

C Create a vertical structure function with zero mean
      tmpsum=0.
      do K=1,Nr
       vertStructWst(K)=cos(mz*PI* (rC(K)/rF(Nr+1)) )
       tmpsum=tmpsum+vertStructWst(K)*drF(K)
      enddo
      tmpsum=tmpsum/rF(Nr+1)
      do K=1,Nr
       vertStructWst(K)=vertStructWst(K)-tmpsum
      enddo
c
      obTimeScale = 44567.0 _d 0
       kx=mz*2. _d 0*pi/400.0 _d 0
     &  *sqrt((2.0 _d 0*pi*2.0 _d 0*pi/(obTimeScale*obTimeScale)
     & - f0*f0)/(1.0 _d -6
     & - 2.0 _d 0*pi*2.0 _d 0*pi/(obTimeScale*obTimeScale)))
      Uinflow = 0.024 _d 0
C *NOTE* I have commented out the ramp function below
C just to speed things up. You will probably want to use it
C for smoother looking solutions.
      rampTime2 = 4. _d 0*44567.0 _d 0

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C     Eastern OB
      IF (useOrlanskiEast) THEN
        CALL ORLANSKI_EAST(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            OBEu(J,K,bi,bj)=0.
            OBEv(J,K,bi,bj)=0.
            OBEt(J,K,bi,bj)=tRef(K)
            OBEs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBEw(J,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C     Western OB
      IF (useOrlanskiWest) THEN
        CALL ORLANSKI_WEST(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
          OBWu(J,K,bi,bj)=0. _d 0
     &       +Uinflow
     &       *vertStructWst(K)
     &       *sin(2. _d 0*PI*futureTime/obTimeScale)
c    &       *(exp(futureTime/rampTime2)
c    &   - exp(-futureTime/rampTime2))
c    &   /(exp(futureTime/rampTime2)
c    &  + exp(-futureTime/rampTime2))
     &   *cos(kx*(3. _d 0-2. _d 0-0.5 _d 0)*delX(1))
          OBWv(J,K,bi,bj)=0. _d 0
     &       +Uinflow
     &       *f0/(2.0 _d 0*PI/obTimeScale)
     &       *vertStructWst(K)
     &       *cos(2. _d 0*PI*futureTime/obTimeScale )
     & * (exp(futureTime/rampTime2)
     &   - exp(-futureTime/rampTime2))
     &   /(exp(futureTime/rampTime2)
     &  + exp(-futureTime/rampTime2))
          OBWt(J,K,bi,bj)=tRef(K)
     & + Uinflow*sin(mz*PI*(float(k)-0.5 _d 0)/float(Nr))
     & * sin(2.0 _d 0*PI*futureTime/obTimeScale)
     & *sqrt(strat/(tAlpha*gravity))
     & *sqrt(2.0 _d 0*PI/obTimeScale*2.0*PI/obTimeScale - f0*f0)
     & /(2.0 _d 0*PI/obTimeScale)
c    & * (exp(futureTime/rampTime2)
c    &   - exp(-futureTime/rampTime2))
c    &   /(exp(futureTime/rampTime2)
c    &  + exp(-futureTime/rampTime2))
#ifdef ALLOW_NONHYDROSTATIC
          OBWw(J,K,bi,bj)=-Uinflow
     & *sqrt(2.0 _d 0*PI/obTimeScale*2.0 _d 0*PI/obTimeScale - f0*f0)
     & /sqrt(strat*strat -
     &          2.0 _d 0*PI/obTimeScale*2.0 _d 0*PI/obTimeScale)
     & *sin(mz*PI*(float(k)-0.5 _d 0)/float(Nr))
     &       *cos(2. _d 0*PI*futureTime/obTimeScale)
c    &       *(exp(futureTime/rampTime2)
c    &   - exp(-futureTime/rampTime2))
c    &   /(exp(futureTime/rampTime2)
c    &  + exp(-futureTime/rampTime2))
#endif
          ENDDO
        ENDDO
      ENDIF

C         Northern OB, template for forcing
      IF (useOrlanskiNorth) THEN
        CALL ORLANSKI_NORTH(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBNv(I,K,bi,bj)=0.
            OBNu(I,K,bi,bj)=0.
            OBNt(I,K,bi,bj)=tRef(K)
            OBNs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBNw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C         Southern OB, template for forcing
      IF (useOrlanskiSouth) THEN
        CALL ORLANSKI_SOUTH(
     &          bi, bj, futureTime,
     &          uVel, vVel, wVel, theta, salt,
     &          myThid )
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            OBSu(I,K,bi,bj)=0.
            OBSv(I,K,bi,bj)=0.
            OBSt(I,K,bi,bj)=tRef(K)
            OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
            OBSw(I,K,bi,bj)=0.
#endif
          ENDDO
        ENDDO
      ENDIF

C--   end bi,bj loops.
      ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_CALC',myThid)
#endif
#endif /* ALLOW_OBCS */

      RETURN
      END
