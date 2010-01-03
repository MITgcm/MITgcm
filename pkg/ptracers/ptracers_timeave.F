C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_timeave.F,v 1.2 2010/01/03 03:11:21 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_TIMEAVE
C     !INTERFACE:
      SUBROUTINE PTRACERS_TIMEAVE(
     I                    myTime, myIter, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R PTRACERS_TIMEAVE
C     | o Time averaging routine for PTRACERS
C     |   in model main time-stepping
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "PTRACERS_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Iteration number
C     bi, bj :: Tile indices
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER bi, bj
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      INTEGER iTr
      _RL DDTT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
        DO iTr=1,PTRACERS_numInUse
         CALL TIMEAVE_RESET( ptracerFluxtave(1-Olx,1-Oly, 1,1,iTr),
     &                       1 , bi, bj, myThid )
         CALL TIMEAVE_RESET( ptracertave   (1-Olx,1-Oly,1,1,1,iTr),
     &                       Nr, bi, bj, myThid )
        ENDDO
        ptracer_half(bi,bj) = 0.
        ptracer_full(bi,bj) = 0.
      ENDIF

      IF ( PTRACERS_taveFreq.GT.0. _d 0 ) THEN
C--   Cumulate state-variables with Half or Full time step :
       IF ( myIter .EQ. nIter0 .OR.
     &    DIFFERENT_MULTIPLE( PTRACERS_taveFreq, myTime, deltaTClock )
     &    ) THEN
        DDTT = 0.5*deltaTclock
       ELSE
        DDTT = deltaTclock
       ENDIF

C-    Time Averages of state variables
       DO iTr=1,PTRACERS_numInUse
         CALL TIMEAVE_CUMULATE( ptracertave(1-Olx,1-Oly,1,1,1,iTr),
     &                          pTracer(1-Olx,1-Oly,1,1,1,iTr),
     &                          Nr, DDTT, bi, bj, myThid )
       ENDDO
C-    Keep record of how much time has been integrated over
        ptracer_half(bi,bj) = ptracer_half(bi,bj)+DDTT

C-    Time Averages of "intermediate" fields
       IF ( myIter .NE. nIter0 ) THEN

C-    Time Averages of surface fluxes
        DO iTr=1,PTRACERS_numInUse
         CALL TIMEAVE_CUMULATE( ptracerFluxtave(1-Olx,1-Oly,1,1,iTr),
     &                        surfaceForcingPTr(1-Olx,1-Oly,1,1,iTr),
     &                          1, deltaTclock, bi, bj, myThid )
        ENDDO
        ptracer_full(bi,bj) = ptracer_full(bi,bj)+deltaTclock

       ENDIF

C-    end block if PTRACERS_taveFreq > 0
      ENDIF

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
