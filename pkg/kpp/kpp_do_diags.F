C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_do_diags.F,v 1.9 2004/07/23 19:43:56 heimbach Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

#undef  MULTIPLE_RECORD_KPP_STATE_FILES
CBOP
C     !ROUTINE: KPP_DO_DIAGS
C     !INTERFACE:
      SUBROUTINE KPP_DO_DIAGS( myCurrentTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_DO_DIAGS                                  
C     | o Do KPP diagnostic output.                              
C     *==========================================================
C     | The following CPP flag (MULTIPLE_RECORD_KPP_STATE_FILES) is
C     | #define/#undefed here since it is specific to this routine
C     | and very user-preference specific.
C     |
C     | If #undefed (default) the state files are written as in all versions
C     | prior to checkpoint32, where a file is created per variable, per time
C     | and per tile. This *has* to be the default because most users use this
C     | mode and all utilities and scripts (diagnostic) assume this form.
C     | It is also robust, as explained below.
C     |
C     | If #defined, subsequent snap-shots are written as records in the
C     | same file (no iteration number in filenames).
C     | Advantages: - fewer files
C     |       - for small problems, is easy to copy the output around
C     | Disadvantages:
C     |       - breaks a lot of diagnostic scripts
C     |       - for large or long problems this creates huge files
C     |       - is an unexpected, unsolicited change in behaviour which came
C     |         as a surprise (in c32) and inconvenience to several users
C     |       - can not accomodate changing the frequency of output
C     |         after a pickup (this is trivial in previous method
C     |         but needs new code and parameters in this new method)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#include "KPP_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myCurrentTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KPP

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      _RL DDTT
CEOP

C----------------------------------------------------------------
C     Dump snapshot of KPP variables.
C----------------------------------------------------------------

      IF (
     &     DIFFERENT_MULTIPLE(kpp_dumpFreq,myCurrentTime,
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
#ifdef MULTIPLE_RECORD_KPP_STATE_FILES
C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
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
#else /* MULTIPLE_RECORD_KPP_STATE_FILES */
C       Write each snap-shot as a new file 
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
            WRITE(suff,'(I10.10)') myIter
            CALL WRITE_FLD_XYZ_RL('KPPviscAz.',suff,KPPviscAz,
     &                          myIter,myThid)
            CALL WRITE_FLD_XYZ_RL('KPPdiffKzT.',suff,KPPdiffKzT,
     &                          myIter,myThid)
            CALL WRITE_FLD_XYZ_RL('KPPdiffKzS.',suff,KPPdiffKzS,
     &                          myIter,myThid)
            CALL WRITE_FLD_XYZ_RL('KPPghat.',suff,KPPghat,
     &                          myIter,myThid)
            CALL WRITE_FLD_XY_RL('KPPhbl.',suff,KPPhbl,
     &                          myIter,myThid)
#endif /* MULTIPLE_RECORD_KPP_STATE_FILES */
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
     &     (kpp_taveFreq,myCurrentTime,myCurrentTime-deltaTClock) )
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
     &     DIFFERENT_MULTIPLE(kpp_taveFreq,myCurrentTime,
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
         
#ifdef MULTIPLE_RECORD_KPP_STATE_FILES
C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
         CALL WRITE_REC_XYZ_RL('KPPviscAz-T',KPPviscAzTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XYZ_RL('KPPdiffKzT-T',KPPdiffKzTTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XYZ_RL('KPPdiffKzS-T',KPPdiffKzSTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XYZ_RL('KPPghat-T',KPPghatTave,
     &                         kpp_drctrecTave,myIter,myThid)
         CALL WRITE_REC_XY_RL('KPPhbl-T',KPPhblTave,
     &                         kpp_drctrecTave,myIter,myThid)
C--   Increment record counter
         kpp_drctrecTave = kpp_drctrecTave + 1

#else /* MULTIPLE_RECORD_KPP_STATE_FILES */

C       Write each snap-shot as a new file 
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
            WRITE(suff,'(I10.10)') myIter
         CALL WRITE_FLD_XYZ_RL('KPPviscAz-T.',suff,KPPviscAzTave,
     &                         myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('KPPdiffKzT-T.',suff,KPPdiffKzTTave,
     &                         myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('KPPdiffKzS-T.',suff,KPPdiffKzSTave,
     &                         myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('KPPghat-T.',suff,KPPghatTave,
     &                         myIter,myThid)
         CALL WRITE_FLD_XY_RL('KPPhbl-T.',suff,KPPhblTave,
     &                         myIter,myThid)
#endif /* MULTIPLE_RECORD_KPP_STATE_FILES */          
         
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
