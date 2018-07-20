#include "DIAG_OPTIONS.h"

      SUBROUTINE DIAGNOSTICS_WRITE_ADJ (
     I                               modelEnd,
     I                               myTime, myIter, myThid )
C***********************************************************************
C  Purpose
C  -------
C    Output sequence for adjoint diagnostic variables
C    Note: This closely mirrors diagnostics_write but is separated for
C          clarity
C    Note: For snapshots, mirror adjDump time step convention rather
C          than forward model diagnostic convention.
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
      INTEGER   n, nd
      INTEGER   myItM1, wrIter
      LOGICAL   dump2fileNow, write2file
      LOGICAL   writeDiags(numLists)
      _RL       phiSec, freqSec, wrTime
#ifdef ALLOW_FIZHI
      CHARACTER *9 tagname
#endif

      myItM1 = myIter - 1

C***********************************************************************
C***   Check to see if its time for Diagnostic Output                ***
C***********************************************************************

      write2file = .FALSE.
      DO n = 1,nlists
        nd = ABS(jdiag(1,n))
        IF ( gdiag(nd)(4:4).EQ.'A' ) THEN
          freqSec = freq(n)
          phiSec = phase(n)

C   Want time step of adjoint state variables to match actual time step
C   to mirror ADJdump
          wrIter = myIter
          wrTime = myTime

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
          writeDiags(n) = dump2fileNow
        ELSE
          writeDiags(n) = .FALSE.
C       end if ( adj var )
        ENDIF
C-    end loop on list id number n
      ENDDO

C--- No Statistics Diag. Output for adjoint variables

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( write2file ) THEN
        IF ( debugLevel.GE.debLevC ) THEN
          CALL DIAGNOSTICS_SUMMARY( myTime, myIter, myThid )
        ENDIF
C-    wait for everyone before setting arrays to zero:
        _BARRIER
      ENDIF

C--     Clear storage space:
      DO n = 1,nlists
        IF ( writeDiags(n) ) CALL DIAGNOSTICS_CLEAR(n,myThid)
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
