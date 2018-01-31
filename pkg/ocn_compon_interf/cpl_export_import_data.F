#include "OCN_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_EXPORT_IMPORT_DATA

C     !INTERFACE:
      SUBROUTINE CPL_EXPORT_IMPORT_DATA(
     I                      myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_EXPORT_IMPORT_DATA
C     | o Routine for controlling
C     |   - export of coupling data to coupler layer AND
C     |   - import of coupling data from coupler layer.
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
C     Synchronous coupling: Send data to coupling layer

      IF ( .NOT.ocn_cplSequential ) THEN
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Fetch data from coupling layer.

      IF ( .TRUE. ) THEN
        IF ( debugMode ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(A,I10)')
     &    '  Importing atmospheric surf fluxes at iter=', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF

        CALL OCN_IMPORT_FIELDS ( myIter, myThid )

C     Fill diagnostics with updated coupling fields just received from Coupler
        CALL CPL_DIAGNOSTICS_FILL( myTime, myIter, myThid )

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef ALLOW_THSICE
C     Sequential coupling: Send data to coupling layer
C     call also OCN_CPL_DIAGS (to cumulate cpl time-ave) in both cases
C      (Synchronous or Sequential coupling)
      CALL OCN_EXPORT_DATA( myTime, myIter, myThid )
#endif /* ndef ALLOW_THSICE */

C     If compiling pkg/thsice, do it later by calling the same S/R
C     directly from DO_OCEANIC_PHYS, just after SEAICE_MODEL call.

#endif /* COMPONENT_MODULE */

      RETURN
      END
