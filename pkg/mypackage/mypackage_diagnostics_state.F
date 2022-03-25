#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MYPACKAGE_DIAGNOSTICS_STATE

C !INTERFACE:
      SUBROUTINE MYPACKAGE_DIAGNOSTICS_STATE(
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Fill-in diagnostics array for MYPACKAGE state variables

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C  put following include within #ifdef in case this file is copied to local
C  code dir but pkg/mypackage is not compiled (missing file MYPACKAGE.h).
#ifdef ALLOW_MYPACKAGE
#include "MYPACKAGE.h"
#endif

C     !INPUT PARAMETERS:
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_MYPACKAGE
C     !LOCAL VARIABLES:

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

#ifdef MYPACKAGE_3D_STATE
         CALL DIAGNOSTICS_FILL( mypa_StatScal1,'MYPaSta1',
     &                          0,Nr, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( mypa_StatScal2,'MYPaSta2',
     &                          0,Nr, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( mypa_StatVelU, 'MYPaStaU',
     &                          0,Nr, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( mypa_StatVelV, 'MYPaStaV',
     &                          0,Nr, 0, 1, 1, myThid )
#endif /* MYPACKAGE_3D_STATE */

#ifdef MYPACKAGE_2D_STATE
         CALL DIAGNOSTICS_FILL( mypa_Surf1 ,   'MYPaSur1',
     &                          0, 1, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( mypa_Surf2 ,   'MYPaSur2',
     &                          0, 1, 0, 1, 1, myThid )
#endif /* MYPACKAGE_2D_STATE */

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
