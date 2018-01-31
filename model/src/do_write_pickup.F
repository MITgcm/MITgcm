#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DO_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE DO_WRITE_PICKUP(
     I                    modelEnd,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine that decides when to write restart
C      (or "pickup" or "checkpoint" ) files. Then it calls 2 subroutines
C     to write the main-model pickup and each package pickup files.
C
C     Both ``rolling-pickup'' files and permanent pickup files
C     are written from here. A rolling pickup works through a circular
C     list of suffices. Generally the circular list has two entries so
C     that a rolling pickup will overwrite the last rolling
C     pickup but one. This is useful for running long jobs without
C     filling too much disk space.  In a permanent pickup, data is
C     written suffixed by the current timestep number. Permanent
C     pickups can be used to provide snap-shots from which the
C     model can be restarted.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT PARAMETERS:
C     modelEnd  :: true if call at end of model run.
C     myTime    :: Current time of simulation ( s )
C     myIter    :: Iteration number
C     myThid    :: Thread number for this instance of the routine.
      LOGICAL modelEnd
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
CEOP

C     !LOCAL VARIABLES:
C     permPickup :: Flag indicating whether a permanent pickup will
C                       be written.
C     tempPickup :: Flag indicating if it is time to write a non-permanent
C                       pickup (that will be permanent if permPickup=T)
C     suffix     :: pickup-name suffix
C     msgBuf     :: message buffer
      LOGICAL permPickup, tempPickup
      CHARACTER*(10) suffix
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      permPickup = .FALSE.
      tempPickup = .FALSE.
      permPickup =
     &     DIFFERENT_MULTIPLE(pChkPtFreq,myTime,deltaTClock)
      tempPickup =
     &     DIFFERENT_MULTIPLE( chkPtFreq,myTime,deltaTClock)

#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
         CALL CAL_TIME2DUMP( zeroRL, pChkPtFreq, deltaTClock,
     U                       permPickup,
     I                       myTime, myIter, myThid )
         CALL CAL_TIME2DUMP( zeroRL, chkPtFreq,  deltaTClock,
     U                       tempPickup,
     I                       myTime, myIter, myThid )
      ENDIF
#endif

      IF ( (modelEnd.AND.writePickupAtEnd)
     &     .OR. permPickup .OR. tempPickup ) THEN
C--   This is time to write pickup files

C-    Create suffix to pass on to main & package pickup routines
        IF ( permPickup .AND. rwSuffixType.EQ.0 ) THEN
          WRITE(suffix,'(I10.10)') myIter
        ELSEIF ( permPickup ) THEN
          CALL RW_GET_SUFFIX( suffix, myTime, myIter, myThid )
        ELSE
          WRITE(suffix,'(A)') checkPtSuff(nCheckLev)
        ENDIF

C-    Write a pickup for each package which need it to restart
        CALL PACKAGES_WRITE_PICKUP(
     I                permPickup, suffix, myTime, myIter, myThid )

C-    Write main model pickup
        IF ( .NOT.useOffLine .OR. nonlinFreeSurf.GT.0 ) THEN
           CALL WRITE_PICKUP(
     I                permPickup, suffix, myTime, myIter, myThid )
        ENDIF

        _BEGIN_MASTER(myThid)
C-    Write information to stdout so there is a record that
C     writing the pickup was completed
        WRITE(msgBuf,'(A11,I10,1X,A10)')
     &     "%CHECKPOINT ", myIter, suffix
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C-    Update pickup level for the next time we write pickup
        IF ( .NOT. permPickup ) THEN
          nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1
        ENDIF
        _END_MASTER(myThid)

      ELSEIF ( modelEnd ) THEN
        WRITE(msgBuf,'(A)')
     &     "Did not write pickup because writePickupAtEnd = FALSE"
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   time to write pickup files: end
      ENDIF

      RETURN
      END
