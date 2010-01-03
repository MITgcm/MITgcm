C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_import_external_data.F,v 1.4 2010/01/03 19:26:54 jmc Exp $
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

C-    Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering
C     has to be consistent with ordering coupling layer.
      CALL OCN_IMPORT_FIELDS ( myThid )

C-    Modify forcing according to Imported fields from coupler
c     CALL OCN_APPLY_IMPORT( myTime, myIter, myThid )

      IF ( cpl_taveFreq.GT.0. _d 0 ) THEN
C--   note: will move this "if" inside OCN_CPL_DIAGS when other diags
C           than time-ave are computed within OCN_CPL_DIAGS.
        CALL OCN_CPL_DIAGS( myTime, myIter, myThid )
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
