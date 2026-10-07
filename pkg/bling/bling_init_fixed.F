#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_INIT_FIXED( myThid )

C     ==========================================================
C     | subroutine bling_init_fixed
C     | o Initialize fixed parameters
C     ==========================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BLING_VARS.h"

C     !INPUT PARAMETERS:
C     myThid       :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C set up coefficients for DIC chemistry
C define Schmidt no. coefficients for CO2
      sca1 = 2073.1 _d 0
      sca2 = -125.62 _d 0
      sca3 = 3.6276 _d 0
      sca4 = -0.043219 _d 0
C define Schmidt no. coefficients for O2
C based on Keeling et al [GBC, 12, 141, (1998)]
C New Wanninkhof numbers
      sox1 = 1929.7 _d 0
      sox2 = -117.46 _d 0
      sox3 = 3.116 _d 0
      sox4 = -0.0306 _d 0

C coefficients for determining saturation O2
      oA0 = 2.00907 _d 0
      oA1 = 3.22014 _d 0
      oA2 = 4.05010 _d 0
      oA3 = 4.94457 _d 0
      oA4 = -2.56847 _d -1
      oA5 = 3.88767 _d 0
      oB0 = -6.24523 _d -3
      oB1 = -7.37614 _d -3
      oB2 = -1.03410 _d -2
      oB3 = -8.17083 _d -3
      oC0 = -4.88682 _d -7

      _END_MASTER(myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDIAGNOSTICS ) THEN
        CALL BLING_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BLING */

      RETURN
      END
