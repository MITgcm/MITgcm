#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_DIAGNOSTICS_FILL
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_DIAGNOSTICS_FILL(
     I             bi, bj, myThid )

C     !DESCRIPTION:
C     Provide the information for the diagnostics

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:

      IF ( useDiagnostics ) THEN

C-    Salt plume depth (>0), [m]
       CALL DIAGNOSTICS_FILL( SaltPlumeDepth,
     &                             'oceSPDep',0, 1,0,1,1,myThid )

C-    net Salt plume flux into the ocean during freezing (+=down), [g/m2/s]
       CALL DIAGNOSTICS_FILL( saltPlumeFlux,
     &                             'oceSPflx',0, 1,0,1,1,myThid )
#ifdef SALT_PLUME_VOLUME
       CALL DIAGNOSTICS_FILL(
     &      SPbrineVolFlux,'oceSPvol',0,1,0,1,1,myThid )
       CALL DIAGNOSTICS_FILL(
     &      SPforcS1,'SPforcS1',0,1,0,1,1,myThid )
       CALL DIAGNOSTICS_FILL(
     &      SPforcT1,'SPforcT1',0,1,0,1,1,myThid )
       CALL DIAGNOSTICS_FILL(
     &      dSPvolSurf2kLev, 'SPVsuf2k',0,Nr,0,1,1,myThid )
       CALL DIAGNOSTICS_FILL(
     &      dSPvolkLev2Above,'SPVk2m  ',0,Nr,0,1,1,myThid )
#endif
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
