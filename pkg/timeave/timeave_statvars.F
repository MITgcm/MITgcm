C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_statvars.F,v 1.27 2012/08/06 16:56:59 jmc Exp $
C $Name:  $

#include "TIMEAVE_OPTIONS.h"

CBOP
C     !ROUTINE: TIMEAVE_STATV_WRITE

C     !INTERFACE:
      SUBROUTINE TIMEAVE_STATVARS(
     I                   myTime, myIter, bi, bj, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE TIMEAVE_STATVARS
C     | o Time averaging routine for eta, U, V, W, T, S, UT, VT
C     |   in model main time-stepping
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"

C     !INPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter, bi, bj
      INTEGER myThid
CEOP

#ifdef ALLOW_TIMEAVE
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      LOGICAL dumpFiles
      _RL DDTT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
        CALL TIMEAVE_RESET(uFluxtave, 1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(vFluxtave, 1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(tFluxtave, 1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(sFluxtave, 1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(etatave,   1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(thetatave, Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(salttave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(uVeltave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(vVeltave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(wVeltave,  Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(phiHydLowtave,1, bi, bj, myThid)
        CALL TIMEAVE_RESET(UTtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(VTtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(WTtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(UStave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(VStave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(WStave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(Eta2tave,  1,  bi, bj, myThid)
        CALL TIMEAVE_RESET(TTtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(UUtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(VVtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(UVtave,    Nr, bi, bj, myThid)
c       CALL TIMEAVE_RESET(KEtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(TdiffRtave,Nr, bi, bj, myThid)
#ifdef ALLOW_MOM_VECINV
        CALL TIMEAVE_RESET(uZetatave, Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(vZetatave, Nr, bi, bj, myThid)
#endif
        CALL TIMEAVE_RESET(phiHydtave,Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(phiHydLow2Tave,1, bi, bj, myThid)
        CALL TIMEAVE_RESET(ConvectCountTave,Nr,bi,bj,myThid)
#ifdef NONLIN_FRSURF
        CALL TIMEAVE_RESET(hUtave,    Nr, bi, bj, myThid)
        CALL TIMEAVE_RESET(hVtave,    Nr, bi, bj, myThid)
c       CALL TIMEAVE_RESET(hFacCtave, Nr, bi, bj, myThid)
c       CALL TIMEAVE_RESET(hFacWtave, Nr, bi, bj, myThid)
c       CALL TIMEAVE_RESET(hFacStave, Nr, bi, bj, myThid)
#endif /* NONLIN_FRSURF */
        timeAve_half(bi,bj) = 0. _d 0
        timeAve_full(bi,bj) = 0. _d 0
      ENDIF

C--   Cumulate state-variables with Half or Full time step :
      IF ( myIter .EQ. nIter0 ) THEN
        DDTT = deltaTclock*(1. _d 0 - tave_lastIter)
      ELSE
        DDTT = deltaTclock
        dumpFiles = DIFFERENT_MULTIPLE(taveFreq,myTime,deltaTClock)
#ifdef ALLOW_CAL
        IF ( useCAL ) THEN
          CALL CAL_TIME2DUMP( zeroRL, taveFreq, deltaTClock,
     U                        dumpFiles,
     I                        myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_CAL */
        IF ( dumpFiles ) DDTT = deltaTclock*tave_lastIter
      ENDIF

      IF ( DDTT .NE. 0. _d 0) THEN

C-    Time Averages of single fields (no hFactor)
      CALL TIMEAVE_CUMULATE(etatave,  etaN,  1 , DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(thetatave,theta, Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(salttave, salt,  Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(uVeltave, uVel,  Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(vVeltave, vVel,  Nr, DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMULATE(wVeltave, wVel,  Nr, DDTT, bi, bj, myThid)

C-    Time Averages of "double" fields (no hFactor)
      CALL TIMEAVE_CUMUL_2V(Eta2tave, etaN,etaN, 1,  0,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(TTtave, theta,theta, Nr, 0,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(UUtave, uVel,  uVel, Nr, 0,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(VVtave, vVel,  vVel, Nr, 0,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(UVtave, uVel,  vVel, Nr, 12,
     &     DDTT, bi, bj, myThid)
c     CALL TIMEAVE_CUMUL_KE(KEtave, uVel,  vVel, Nr,
c    &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(WTtave, theta, wVel, Nr, 3,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2V(WStave, salt,  wVel, Nr, 3,
     &     DDTT, bi, bj, myThid)

#ifdef NONLIN_FRSURF

c     CALL TIMEAVE_CUMUL_FC(hFacCtave,hFacC, Nr, DDTT, bi, bj, myThid)
c     CALL TIMEAVE_CUMUL_FC(hFacWtave,hFacW, Nr, DDTT, bi, bj, myThid)
c     CALL TIMEAVE_CUMUL_FC(hFacStave,hFacS, Nr, DDTT, bi, bj, myThid)

C-    Time Averages of single fields (* hFactor)
      CALL TIMEAVE_CUMUL_1VFC(hUtave,  uVel,  hFacW, Nr,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_1VFC(hVtave,  vVel,  hFacS, Nr,
     &     DDTT, bi, bj, myThid)

#endif /* NONLIN_FRSURF */

C-    Time Averages of "double" fields (* hFactor)
      CALL TIMEAVE_CUMUL_2VFC(UTtave, theta, uVel,  hFacW, Nr, 1,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2VFC(VTtave, theta, vVel,  hFacS, Nr, 2,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2VFC(UStave, salt, uVel,  hFacW, Nr, 1,
     &     DDTT, bi, bj, myThid)
      CALL TIMEAVE_CUMUL_2VFC(VStave, salt, vVel,  hFacS, Nr, 2,
     &     DDTT, bi, bj, myThid)

C-    Time Averages of "double" fields (no hFactor)
c     CALL TIMEAVE_CUMUL_2V(UTtave, theta, uVel, Nr, 1,
c    &     DDTT, bi, bj, myThid)
c     CALL TIMEAVE_CUMUL_2V(VTtave, theta, vVel, Nr, 2,
c    &     DDTT, bi, bj, myThid)
c     CALL TIMEAVE_CUMUL_2V(UStave, salt, uVel, Nr, 1,
c    &     DDTT, bi, bj, myThid)
c     CALL TIMEAVE_CUMUL_2V(VStave, salt, vVel, Nr, 2,
c    &     DDTT, bi, bj, myThid)

C-    Keep record of how much time has been integrated over
      timeAve_half(bi,bj) = timeAve_half(bi,bj)+DDTT

C-- end if DDTT ...
      ENDIF

C-    Time Averages of "intermediate" fields (no hFactor)
      IF ( myIter .NE. nIter0 ) THEN

C-    Time Averages of surface fluxes
C     <- moved to external_forcing_surf

        CALL TIMEAVE_CUMULATE(phiHydLowtave, phiHydLow, 1,
     &                                   deltaTclock, bi, bj, myThid)
        CALL TIMEAVE_CUMULATE(phiHydtave, totPhihyd,  Nr,
     &                                   deltaTclock, bi, bj, myThid)
        CALL TIMEAVE_CUMUL_2V(phiHydLow2Tave,
     &       phiHydLow,phiHydLow, 1,  0, deltaTclock, bi, bj, myThid)
        timeAve_full(bi,bj) = timeAve_full(bi,bj)+deltaTclock
      ENDIF

#endif /* ALLOW_TIMEAVE */

      RETURN
      END
