C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_statvars.F,v 1.9 2007/11/05 18:48:04 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif

      SUBROUTINE PTRACERS_STATVARS(
     I     myTime, myIter, bi, bj, myThid)
C     /==========================================================\
C     | SUBROUTINE PTRACERS_STATVARS                              |
C     | o Time averaging routine for PTRACERS  |
C     |   in model main time-stepping                            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "PTRACERS_STATV.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter, bi, bj
      _RL     myTime

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE

C     == Local variables ==
      INTEGER iTracer, i, j, k
      _RL DDTT
c     _RL tempArray (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C-    Initialize fields for the first call ever
      IF ( myIter .EQ. nIter0 ) THEN
        DO iTracer=1,PTRACERS_numInUse
         CALL TIMEAVE_RESET(ptracerFluxtave(1-Olx,1-Oly,  1,1,iTracer),
     &                                                  1 ,bi,bj,myThid)
         CALL TIMEAVE_RESET(ptracertave    (1-Olx,1-Oly,1,1,1,iTracer),
     &                                                  Nr,bi,bj,myThid)
        ENDDO
        DO K=1,Nr
         ptracer_half(k,bi,bj)=0.
         ptracer_full(k,bi,bj)=0.
        ENDDO
      ENDIF

C--   Cumulate state-variables with Half or Full time step :
      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE( PTRACERS_taveFreq, myTime, deltaTClock)
     &   ) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF

C-    Time Averages of single fields (no hFactor)
      DO iTracer=1,PTRACERS_numInUse
       CALL TIMEAVE_CUMULATE(ptracertave(1-Olx,1-Oly,1,1,1,iTracer),
     &                       pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     &                       Nr, DDTT, bi, bj, myThid)
      ENDDO

C-    Keep record of how much time has been integrated over
      DO K=1,Nr
        ptracer_half(k,bi,bj)=ptracer_half(k,bi,bj)+DDTT
      ENDDO

C-    Time Averages of "intermediate" fields (no hFactor)
      IF ( myIter .NE. nIter0 ) THEN

C-    Time Averages of surface fluxes
c      IF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
c       k=Nr
c      ELSE
c       k=1
c      ENDIF

C     ptracerFlux
       DO iTracer=1,PTRACERS_numInUse
c       DO j=1,sNy
c        DO i=1,sNx
c         tempArray(i,j,bi,bj)=maskC(i,j,k,bi,bj)*
c    &     surfaceForcingPTr(i,j,bi,bj,iTracer)*
c    &     drF(k)*hFacC(i,j,k,bi,bj)
c        ENDDO
c       ENDDO
c       CALL TIMEAVE_CUMULATE(ptracerFluxtave(1-Olx,1-Oly,1,1,iTracer),
c    &                        tempArray,1,deltaTclock,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(ptracerFluxtave(1-Olx,1-Oly,1,1,iTracer),
     &                      surfaceForcingPTr(1-Olx,1-Oly,1,1,iTracer),
     &                                  1,deltaTclock,bi,bj,myThid)
       ENDDO

       DO K=1,Nr
        ptracer_full(k,bi,bj)=ptracer_full(k,bi,bj)+deltaTclock
       ENDDO
      ENDIF

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
