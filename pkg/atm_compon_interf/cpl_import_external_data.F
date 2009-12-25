C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/cpl_import_external_data.F,v 1.4 2009/12/25 19:42:46 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_IMPORT_EXTERNAL_DATA
C     !INTERFACE:
      SUBROUTINE CPL_IMPORT_EXTERNAL_DATA(
     I                      myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_IMPORT_EXTERNAL_DATA
C     | o Routine for controlling import of coupling data from
C     |   coupler layer.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*
C     \ev

C     !USES
      IMPLICIT NONE

#include "EEPARAMS.h"
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
C     == Local variables ==

C     Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering
C     has to be consistent with ordering coupling layer.

      IF ( MOD(myIter,cplSendFrq_iter).EQ.0 ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(0,'(A,I5,A,I10)') '  Importing (pid=', myProcId,
     &            ' ) oceanic fields at iteration', myIter
       _END_MASTER( myThid )
       CALL ATM_IMPORT_FIELDS( myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
