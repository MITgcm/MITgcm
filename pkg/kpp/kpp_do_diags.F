C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_do_diags.F,v 1.2 2000/11/13 16:37:02 heimbach Exp $

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

#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               DO k=1,Nr
                  CALL TIMEAVER_INI_XYZ(KPPviscAztave, bi,bj,K,myThid)
                  CALL TIMEAVER_INI_XYZ(KPPdiffKzTtave, bi,bj,K,myThid)
                  CALL TIMEAVER_INI_XYZ(KPPghattave, bi,bj,K,myThid)
                  CALL TIMEAVER_INI_XYZ(KPPdiffKzStave, bi,bj,K,myThid)
                  IF (K.EQ.1)
     &                 CALL TIMEAVER_INI_XY(KPPhbltave, bi,bj,myThid)
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
            DO k=1,Nr
               CALL TIMEAVER_1FLD_XYZ(
     &              KPPviscAz,KPPviscAztave,  DDTT,bi,bj,K,myThid)
               CALL TIMEAVER_1FLD_XYZ(
     &              KPPdiffKzT,KPPdiffKzTtave,DDTT,bi,bj,K,myThid)
               CALL TIMEAVER_1FLD_XYZ(
     &              KPPghat,   KPPghattave,   DDTT,bi,bj,K,myThid)
               CALL TIMEAVER_1FLD_XYZ(
     &              KPPdiffKzS,KPPdiffKzStave,DDTT,bi,bj,K,myThid)
               IF (K.EQ.1)
     &              CALL TIMEAVER_1FLD_XYZ(
     &              KPPhbl,KPPhbltave,DDTT,bi,bj,1,myThid)
               
C     Keep record of how much time has been integrated over
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
               DO k=1,Nr
                  CALL TIMEAVER_FINAL_XYZ(KPPviscAztave,kpp_timeave,
     &                 bi,bj,K,myThid)
                  CALL TIMEAVER_FINAL_XYZ(KPPdiffKzTtave,kpp_timeave,
     &                 bi,bj,K,myThid)
                  CALL TIMEAVER_FINAL_XYZ(KPPghattave,kpp_timeave,
     &                 bi,bj,K,myThid)
                  CALL TIMEAVER_FINAL_XYZ(KPPdiffKzStave,kpp_timeave,
     &                 bi,bj,K,myThid)
               ENDDO
               CALL TIMEAVER_FINAL_XY(KPPhbltave,kpp_timeave,
     &              bi,bj,myThid)
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
               DO k=1,Nr
                  CALL TIMEAVER_INI_XYZ(KPPviscAztave,
     &                 bi,bj,K,myThid)
                  CALL TIMEAVER_INI_XYZ(KPPdiffKzTtave,
     &                 bi,bj,K,myThid)
                  CALL TIMEAVER_INI_XYZ(KPPghattave,
     &                 bi,bj,K,myThid)
                  CALL TIMEAVER_INI_XYZ(KPPdiffKzStave,
     &                 bi,bj,K,myThid)
                  IF (K.EQ.1)
     &                 CALL TIMEAVER_INI_XY(KPPhbltave, bi,bj,myThid)
                  kpp_TimeAve(k,bi,bj)=0.
               ENDDO
            ENDDO
         ENDDO
         
C     Time Average KPP fields
         DDTT=0.5*deltaTclock
         DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
               DO k=1,Nr
                  CALL TIMEAVER_1FLD_XYZ(
     &                 KPPviscAz,KPPviscAztave,  DDTT,bi,bj,K,myThid)
                  CALL TIMEAVER_1FLD_XYZ(
     &                 KPPdiffKzT,KPPdiffKzTtave,DDTT,bi,bj,K,myThid)
                  CALL TIMEAVER_1FLD_XYZ(
     &                 KPPghat,   KPPghattave,   DDTT,bi,bj,K,myThid)
                  CALL TIMEAVER_1FLD_XYZ(
     &                 KPPdiffKzS,KPPdiffKzStave,DDTT,bi,bj,K,myThid)
                  IF (K.EQ.1)
     &                 CALL TIMEAVER_1FLD_XYZ(
     &                 KPPhbl,KPPhbltave,DDTT,bi,bj,1,myThid)
                  
C     Keep record of how much time has been integrated over
                  kpp_TimeAve(k,bi,bj)=kpp_TimeAve(k,bi,bj)+DDTT
               ENDDO
            ENDDO
         ENDDO
      ENDIF

#endif
      
#endif
      
      RETURN
      END
