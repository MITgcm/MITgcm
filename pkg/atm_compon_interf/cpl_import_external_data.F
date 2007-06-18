C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/cpl_import_external_data.F,v 1.3 2007/06/18 21:28:53 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_IMPORT_EXTERNAL_DATA
C     !INTERFACE:
      SUBROUTINE CPL_IMPORT_EXTERNAL_DATA(
     I                      myIter, myTime, myThid )

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
C     myIter - Current timestep number.
C     myTime - Current internal time.
C     myThid - Thread number for this instance of the routine.
      INTEGER myIter
      _RL     myTime
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     == Local variables ==

C     Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering
C     has to be consistent with ordering coupling layer.

      IF ( MOD(myIter,cplSendFrq_iter).EQ.0 ) THEN
       WRITE(0,*) ' Importing oceanic fields at iteration ', myIter
       CALL ATM_IMPORT_FIELDS( myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
