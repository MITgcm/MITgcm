#include "DIAG_OPTIONS.h"

      SUBROUTINE DIAGNOSTICS_WRITE (
     I                               modelEnd,
     I                               myTime, myIter, myThid )
C***********************************************************************
C  Purpose
C  -------
C    Output sequence for the (multiple) diagnostics output files
C
C  Arguments  Description
C  ----------------------
C     modelEnd :: true if call at end of model run.
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current Iteration Number
C     myThid   :: my Thread Id number
C***********************************************************************
       IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "PARAMS.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      LOGICAL modelEnd
      _RL     myTime
      INTEGER myIter, myThid

C     !FUNCTIONS:
      LOGICAL  DIFF_PHASE_MULTIPLE
      EXTERNAL DIFF_PHASE_MULTIPLE
#ifdef ALLOW_FIZHI
      LOGICAL  ALARM2
      EXTERNAL ALARM2
#endif

c Local variables
c ===============
      INTEGER   n
      INTEGER   myItM1, wrIter
      LOGICAL   dump2fileNow, write2file
      _RL       phiSec, freqSec, wrTime
#ifdef ALLOW_FIZHI
      CHARACTER *9 tagname
#endif

Cts ---
C   For adjoint state variable diagnostics:
C   Want ability to print adjoint variable at iter0
Cts ---
#ifdef DIAGNOSTICS_ADJ_OUTPUT 
      if ( .true. ) then
#else
      IF ( myIter.NE.nIter0 ) THEN
#endif

        myItM1 = myIter - 1

C***********************************************************************
C***   Check to see if its time for Diagnostic Output                ***
C***********************************************************************

        write2file = .FALSE.
        DO n = 1,nlists
Cts ---
C   Only want to access iter0 for adjoint variables
Cts ---
#ifdef DIAGNOSTICS_ADJ_OUTPUT
          if ( flds(1,n)(1:3).eq.'ADJ' .or. myIter.ne.nIter0 ) then 
#endif

          freqSec = freq(n)
          phiSec = phase(n)

          IF ( freqSec.LT.0. ) THEN
C--     write snap-shot with suffix = myIter to be consistent with
C       time-average diagnostics (e.g., freq=-1 & freq=1):
c           wrIter = myIter
c           wrTime = myTime
C--     write snap-shot with suffix = myIter-1 to be consistent with
C       state-variable time-step:

Cts ---
C   Want time step of adjoint state variables to match actual time step
C   to mirror ADJdump
Cts ---
#ifdef DIAGNOSTICS_ADJ_OUTPUT
            if ( flds(1,n)(1:3).eq.'ADJ' ) then
              wrIter = myIter
              wrTime = myTime
            else
              wrIter = myItM1
              wrTime = myTime - deltaTClock
            endif
#else
            wrIter = myItM1
            wrTime = myTime - deltaTClock
#endif
          ELSE
            wrIter = myIter
            wrTime = myTime
          ENDIF
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                        wrTime, deltaTClock )
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagtag',n
           dump2fileNow = ALARM2(tagname)
          ENDIF
#endif
#ifdef ALLOW_CAL
          IF ( useCAL ) THEN
            CALL CAL_TIME2DUMP( phiSec, freqSec, deltaTClock,
     U                          dump2fileNow,
     I                          wrTime, myIter, myThid )
          ENDIF
#endif /* ALLOW_CAL */
          IF ( dumpAtLast .AND. modelEnd
     &                    .AND. freqSec.GE.0. ) dump2fileNow = .TRUE.
          IF ( dump2fileNow ) THEN
              
            write2file = .TRUE.
            CALL DIAGNOSTICS_OUT(n,wrTime,wrIter,myThid)
          ENDIF
#ifdef DIAGNOSTICS_ADJ_OUTPUT
          endif ! end if (adj var || myIter != 0) 
#endif
        ENDDO

C---   Check to see if its time for Statistics Diag. Output

Cts ---
C   Mirror above if statement, no stats for adjoint vars
Cts ---
#ifdef DIAGNOSTICS_ADJ_OUTPUT
        if ( myIter.ne.nIter0 ) then 
#endif
        DO n = 1,diagSt_nbLists
          freqSec = diagSt_freq(n)
          phiSec = diagSt_phase(n)

          IF ( freqSec.LT.0. ) THEN
C--     write snap-shot with suffix = myIter to be consistent with
C       time-average diagnostics (e.g., freq=-1 & freq=1):
c           wrIter = myIter
c           wrTime = myTime
C--     write snap-shot with suffix = myIter-1 to be consistent with
C       state-variable time-step:
            wrIter = myItM1
            wrTime = myTime - deltaTClock
          ELSE
            wrIter = myIter
            wrTime = myTime
          ENDIF
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                        wrTime, deltaTClock )
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagStg',n
           dump2fileNow = ALARM2(tagname)
          ENDIF
#endif
#ifdef ALLOW_CAL
          IF ( useCAL ) THEN
            CALL CAL_TIME2DUMP( phiSec, freqSec, deltaTClock,
     U                          dump2fileNow,
     I                          wrTime, myIter, myThid )
          ENDIF
#endif /* ALLOW_CAL */
          IF ( dumpAtLast .AND. modelEnd
     &                    .AND. freqSec.GE.0. ) dump2fileNow = .TRUE.
          IF ( dump2fileNow ) THEN
            write2file = .TRUE.
            CALL DIAGSTATS_OUTPUT(n,wrTime,wrIter,myThid)
          ENDIF

        ENDDO
#ifdef DIAGNOSTICS_ADJ_OUTPUT
        endif ! end if ( myIter != 0) 
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF ( write2file ) THEN
          IF ( debugLevel.GE.debLevC ) THEN
            CALL DIAGNOSTICS_SUMMARY( myTime, myIter, myThid )
          ENDIF
C-      wait for everyone before setting arrays to zero:
          _BARRIER
        ENDIF
        IF ( modelEnd ) THEN
C-      Track diagnostics pkg activation status:
c         IF ( diag_pkgStatus.NE.ready2fillDiags ) STOP
          _BARRIER
          _BEGIN_MASTER(myThid)
C Don't want to disable adj output
#ifndef DIAGNOSTICS_ADJ_OUTPUT
          diag_pkgStatus = 99
#endif
          _END_MASTER(myThid)
          _BARRIER
C       Close all Stat-diags output files
          CALL DIAGSTATS_CLOSE_IO( myThid )
        ENDIF

C--     Clear storage space:

        DO n = 1,nlists
Cts ---
C   Only want to access iter0 for adjoint variables
Cts ---
#ifdef DIAGNOSTICS_ADJ_OUTPUT
          if ( flds(1,n)(1:3).eq.'ADJ' .or. myIter.ne.nIter0 ) then 
#endif
          freqSec = freq(n)
          phiSec = phase(n)

          wrTime = myTime

#ifdef DIAGNOSTICS_ADJ_OUTPUT
          if ( flds(1,n)(1:3).ne.'ADJ' .and. freqSec.lt.0 ) then
            wrTime = myTime - deltaTClock
          endif
#else
          IF ( freqSec.LT.0. ) wrTime = myTime - deltaTClock
#endif

          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                        wrTime, deltaTClock )
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagtag',n
           dump2fileNow = ALARM2(tagname)
          ENDIF
#endif
#ifdef ALLOW_CAL
          IF ( useCAL ) THEN
            CALL CAL_TIME2DUMP( phiSec, freqSec, deltaTClock,
     U                          dump2fileNow,
     I                          wrTime, myIter, myThid )
          ENDIF
#endif /* ALLOW_CAL */
          IF ( dumpAtLast .AND. modelEnd
     &                    .AND. freqSec.GE.0. ) dump2fileNow = .TRUE.
          IF ( dump2fileNow ) CALL DIAGNOSTICS_CLEAR(n,myThid)
#ifdef DIAGNOSTICS_ADJ_OUTPUT
          endif ! end if (adj var || myIter != 0) 
#endif
        ENDDO

Cts ---
C   Mirror above if statement, no stats for adjoint vars
Cts ---
#ifdef DIAGNOSTICS_ADJ_OUTPUT
        if ( myIter.ne.nIter0 ) then 
#endif

        DO n = 1,diagSt_nbLists
          freqSec = diagSt_freq(n)
          phiSec = diagSt_phase(n)
          wrTime = myTime
          IF ( freqSec.LT.0. ) wrTime = myTime - deltaTClock
          dump2fileNow = DIFF_PHASE_MULTIPLE( phiSec, freqSec,
     &                                        wrTime, deltaTClock )
#ifdef ALLOW_FIZHI
          IF ( useFIZHI ) THEN
           WRITE(tagname,'(A,I2.2)')'diagStg',n
           dump2fileNow = ALARM2(tagname)
          ENDIF
#endif
#ifdef ALLOW_CAL
          IF ( useCAL ) THEN
            CALL CAL_TIME2DUMP( phiSec, freqSec, deltaTClock,
     U                          dump2fileNow,
     I                          wrTime, myIter, myThid )
          ENDIF
#endif /* ALLOW_CAL */
          IF ( dumpAtLast .AND. modelEnd
     &                    .AND. freqSec.GE.0. ) dump2fileNow = .TRUE.
          IF ( dump2fileNow ) CALL DIAGSTATS_CLEAR( n, myThid )

        ENDDO
#ifdef DIAGNOSTICS_ADJ_OUTPUT
        endif ! end if ( myIter != 0) 
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      RETURN
      END
