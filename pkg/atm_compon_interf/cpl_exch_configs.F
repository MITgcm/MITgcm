#include "ATM_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_EXCH_CONFIGS

C !INTERFACE:
      SUBROUTINE CPL_EXCH_CONFIGS( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_EXCH_CONFIGS
C     | o Controlling routine for initial config exchange between
C     |   component models and atmosphere component.
C     | - Atmospheric version -
C     *==========================================================*
C     | Controls the import of configuration information
C     | (grid/topography,etc...) from other components and the
C     | export of configuration information from this component.
C     | The routine does some basic checking on consistency
C     | components and summarizes the information that has been
C     | imported.
C     | The routine will need to be customised for different
C     | styles of coupled run. The coupler requires consistency
C     | between sending and receiving operations posted by
C     | various components. Therefore changes in one component
C     | model CPL_EXCH_CONFIG may require changes in other
C     | component models CPL_EXCH_CONFIG routines as well
C     | as in the coupler EXCH_COMPONENT_CONFIG routine.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
c#include "CPL_PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid

C !LOCAL VARIABLES:
C     errMsg   :: error message to print to clog file
      LOGICAL errFlag
      CHARACTER*70 errMsg
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
C--   local variable in common block
      _RL landMask_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON / CPL_EXCH_CONFIGS_LOC / landMask_loc
CEOP

      errFlag = .FALSE.
      errMsg  = ' '

C-    Get configuration information (=land/sea mask) from other pkg
      CALL ATM_GET_ATMCONFIG(
     O                        landMask_loc,
     I                        myThid )

C-    Post my configuration information to the coupler "layer".
      CALL ATM_EXPORT_ATMCONFIG(
     U                        errFlag,
     I                        landMask_loc, myThid )

C-    Import other component model(s) configuration(s) from the coupler "layer"
C     o Get ocean model configuration
      CALL ATM_IMPORT_OCNCONFIG( myThid )

C-    Summarise coupling config set-up and check for inconsistency
      _BARRIER
      CALL ATM_CHECK_CPLCONFIG(
     U                        errFlag, errMsg,
     I                        landMask_loc, myThid )

C--   All procs in World check for error and stop if any
      _BEGIN_MASTER( myThid )
      CALL MITCPLR_ALL_CHECK( errFlag, errMsg )
      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
