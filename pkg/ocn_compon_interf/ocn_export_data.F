#include "OCN_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: OCN_EXPORT_DATA

C     !INTERFACE:
      SUBROUTINE OCN_EXPORT_DATA(
     I                      myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OCN_EXPORT_DATA
C     | o Routine for controlling
C     |   - export of ocean coupling data to coupler layer
C     |     but only if using Sequential coupling.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*
C     \ev

C     !USES
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: Current time in simulation.
C     myIter  :: Current timestep number.
C     myThid  :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     msgBuf  :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Note MIT Coupler checkpoint1 does not allow asynchronous posting of
C     data, so ordering has to be consistent with coupling layer ordering.
C--------------------------------------------------------------------------
C     Sequential coupling: Send data to coupling layer

      IF ( ocn_cplSequential ) THEN
        IF ( debugMode ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(A,I10)')
     &    '  Exporting  oceanic surface fields at iter=', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF

        CALL OCN_STORE_MY_DATA(
     I           myTime, myIter, myThid )

        CALL OCN_EXPORT_FIELDS ( myIter, myThid )

      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
