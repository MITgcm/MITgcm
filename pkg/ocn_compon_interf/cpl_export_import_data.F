C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_export_import_data.F,v 1.2 2015/11/12 00:53:45 jmc Exp $
C $Name:  $

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
C     Send data to coupling layer

      IF ( .TRUE. ) THEN
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

        CALL OCN_EXPORT_FIELDS ( myThid )

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

        CALL OCN_IMPORT_FIELDS ( myThid )

C-    Modify forcing according to Imported fields from coupler
c       CALL OCN_APPLY_IMPORT( myTime, myIter, myThid )

        IF ( cpl_taveFreq.GT.0. _d 0 ) THEN
C--   note: will move this "if" inside OCN_CPL_DIAGS when other diags
C           than time-ave are computed within OCN_CPL_DIAGS.
         CALL OCN_CPL_DIAGS( myTime, myIter, myThid )
        ENDIF

      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
