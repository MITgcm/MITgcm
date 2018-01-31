#include "GGL90_OPTIONS.h"

CBOP
C !ROUTINE: GGL90_EXCHANGES

C !INTERFACE: ==========================================================
      SUBROUTINE GGL90_EXCHANGES( myThid )

C !DESCRIPTION: \bv
C     *================================================================*
C     | S/R GGL90_EXCHANGES
C     | Exchange data to update overlaps for GGL90TKE
C     *================================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS: ========================================
C     == Routine arguments ==
C  myThid               :: thread number
      INTEGER myThid

#ifdef ALLOW_GGL90
C !LOCAL VARIABLES: ====================================================
C     == Local variables ==
CEOP

#ifdef ALLOW_GGL90_HORIZDIFF
      IF (GGL90isON .AND. GGL90diffTKEh .GT. 0. _d 0) THEN
C Exchange overlaps
       _EXCH_XYZ_RL(GGL90TKE,myThid)
      ENDIF
#endif /* ALLOW_GGL90_HORIZDIFF */
#ifdef ALLOW_GGL90_IDEMIX
      IF ( useIDEMIX ) THEN
       _EXCH_XYZ_RL(IDEMIX_E,myThid)
       _EXCH_XYZ_RL(IDEMIX_V0,myThid)
      ENDIF
#endif
#endif /* ALLOW_GGL90 */

      RETURN
      END
