C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_do_diags.F,v 1.5 2001/03/06 18:01:02 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_DO_DIAGS( myCurrentTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_DO_DIAGS                                  |
C     | o Do KPP diagnostic output.                              |
C     |==========================================================|
C     | Right now this subroutine assumes JPL direct access I/O  |
C     | format.  You will need to update this with your favorite |
C     | I/O package.                                             |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "KPP_DIAGS.h"

C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myCurrentTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KPP

C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      _RL DDTT

C----------------------------------------------------------------
C     Dump snapshot of KPP variables.
C----------------------------------------------------------------

      IF (
     &     DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,
     &     myCurrentTime-deltaTClock)
     &     ) THEN
         
         IF (KPPmixingMaps) THEN
            CALL PLOT_FIELD_XYRL  ( KPPhbl    , 'KPPhbl'    ,    
     &           myIter, myThid )
            CALL PLOT_FIELD_XYZRL ( KPPviscAz , 'KPPviscAz' ,
     &           Nr, myIter, myThid )
            CALL PLOT_FIELD_XYZRL ( KPPdiffKzT, 'KPPdiffKzT',
     &           Nr, myIter, myThid )
            CALL PLOT_FIELD_XYZRL ( KPPghat   , 'KPPghat'   ,
     &           Nr, myIter, myThid )
         ENDIF

         if (KPPwriteState) then
            CALL WRITE_REC_XYZ_RL('KPPviscAz',KPPviscAz,kpp_drctrec,
     &                          myIter,myThid)
            CALL WRITE_REC_XYZ_RL('KPPdiffKzT',KPPdiffKzT,kpp_drctrec,
     &                          myIter,myThid)
            CALL WRITE_REC_XYZ_RL('KPPdiffKzS',KPPdiffKzS,kpp_drctrec,
     &                          myIter,myThid)
            CALL WRITE_REC_XYZ_RL('KPPghat',KPPghat,kpp_drctrec,
     &                          myIter,myThid)
            CALL WRITE_REC_XY_RL('KPPhbl',KPPhbl,kpp_drctrec,
     &                          myIter,myThid)
         endif

C--   Increment record counter
         kpp_drctrec = kpp_drctrec + 1

      ENDIF

C----------------------------------------------------------------
C     Do KPP time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_RESET(KPPviscAztave, Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPdiffKzTtave,Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPghattave,   Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPdiffKzStave,Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPhbltave,    1, bi,bj,myThid)
               DO k=1,Nr
                  kpp_TimeAve(k,bi,bj)=0.
               ENDDO
            ENDDO
         ENDDO
      ENDIF

C     Time Average KPP fields
      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE
     &     (taveFreq,myCurrentTime,myCurrentTime-deltaTClock) )
     &     THEN
         DDTT=0.5*deltaTclock
      ELSE
         DDTT=deltaTclock
      ENDIF
      DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
            CALL TIMEAVE_CUMULATE(
     &           KPPviscAztave, KPPviscAz, Nr,DDTT,bi,bj,myThid)
            CALL TIMEAVE_CUMULATE(
     &           KPPdiffKzTtave,KPPdiffKzT,Nr,DDTT,bi,bj,myThid)
            CALL TIMEAVE_CUMULATE(
     &           KPPghattave,   KPPghat,   Nr,DDTT,bi,bj,myThid)
            CALL TIMEAVE_CUMULATE(
     &           KPPdiffKzStave,KPPdiffKzS,Nr,DDTT,bi,bj,myThid)
            CALL TIMEAVE_CUMULATE(
     &           KPPhbltave,    KPPhbl,    1, DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
            DO k=1,Nr
               kpp_TimeAve(k,bi,bj)=kpp_TimeAve(k,bi,bj)+DDTT
            ENDDO
         ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(taveFreq,myCurrentTime,
     &     myCurrentTime-deltaTClock)
     &     ) THEN
         
C     Normalize by integrated time
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_NORMALIZ(KPPviscAztave,kpp_timeave,
     &                 Nr, bi,bj,myThid)
               CALL TIMEAVE_NORMALIZ(KPPdiffKzTtave,kpp_timeave,
     &                 Nr, bi,bj,myThid)
               CALL TIMEAVE_NORMALIZ(KPPghattave,kpp_timeave,
     &                 Nr, bi,bj,myThid)
               CALL TIMEAVE_NORMALIZ(KPPdiffKzStave,kpp_timeave,
     &                 Nr, bi,bj,myThid)
               CALL TIMEAVE_NORMALIZ(KPPhbltave,kpp_timeave,
     &                 1,  bi,bj,myThid)
            ENDDO
         ENDDO
         
         CALL WRITE_REC_XYZ_RS('KPPviscAz-T',KPPviscAzTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XYZ_RS('KPPdiffKzT-T',KPPdiffKzTTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XYZ_RS('KPPdiffKzS-T',KPPdiffKzSTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XYZ_RS('KPPghat-T',KPPghatTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XY_RS('KPPhbl-T',KPPhblTave,
     &                         kpp_drctrecTave,myIter,myThid)
         
C--   Increment record counter
         kpp_drctrecTave = kpp_drctrecTave + 1
         
C     Reset averages to zero
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_RESET(KPPviscAztave, Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPdiffKzTtave,Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPghattave,   Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPdiffKzStave,Nr,bi,bj,myThid)
               CALL TIMEAVE_RESET(KPPhbltave,    1, bi,bj,myThid)
               DO k=1,Nr
                  kpp_TimeAve(k,bi,bj)=0.
               ENDDO
            ENDDO
         ENDDO
         
C     Time Average KPP fields
         DDTT=0.5*deltaTclock
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               CALL TIMEAVE_CUMULATE(
     &              KPPviscAztave, KPPviscAz, Nr,DDTT,bi,bj,myThid)
               CALL TIMEAVE_CUMULATE(
     &              KPPdiffKzTtave,KPPdiffKzT,Nr,DDTT,bi,bj,myThid)
               CALL TIMEAVE_CUMULATE(
     &              KPPghattave,   KPPghat,   Nr,DDTT,bi,bj,myThid)
               CALL TIMEAVE_CUMULATE(
     &              KPPdiffKzStave,KPPdiffKzS,Nr,DDTT,bi,bj,myThid)
               CALL TIMEAVE_CUMULATE(
     &              KPPhbltave,    KPPhbl,    1, DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
               DO k=1,Nr
                  kpp_TimeAve(k,bi,bj)=kpp_TimeAve(k,bi,bj)+DDTT
               ENDDO
            ENDDO
         ENDDO
      ENDIF

#endif
      
#endif
      
      RETURN
      END
