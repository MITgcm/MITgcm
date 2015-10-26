C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_export_import_data.F,v 1.1 2015/10/26 22:37:10 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        CALL OCN_STORE_MY_DATA(
     I           myTime, myIter, myThid )

C     Send data to coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous posting of data, so ordering
C     has to be consistent with coupling layer ordering.
        CALL OCN_EXPORT_FIELDS ( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering
C     has to be consistent with ordering coupling layer.
        CALL OCN_IMPORT_FIELDS ( myThid )

C-    Modify forcing according to Imported fields from coupler
c       CALL OCN_APPLY_IMPORT( myTime, myIter, myThid )

        IF ( cpl_taveFreq.GT.0. _d 0 ) THEN
C--   note: will move this "if" inside OCN_CPL_DIAGS when other diags
C           than time-ave are computed within OCN_CPL_DIAGS.
         CALL OCN_CPL_DIAGS( myTime, myIter, myThid )
        ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
