C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_diagnostics_fill.F,v 1.3 2011/12/28 20:50:58 jmc Exp $
C $Name:  $

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

C-    net Salt plume flux into the ocean during freezing (+=down), [psu.kg/m2/s ~ g/m2/s]
       CALL DIAGNOSTICS_FILL( saltPlumeFlux,
     &                             'oceSPflx',0, 1,0,1,1,myThid )

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
