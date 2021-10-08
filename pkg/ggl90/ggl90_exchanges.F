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
C      myThid     :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_GGL90
C !LOCAL VARIABLES: ====================================================
CEOP

#ifdef ALLOW_GGL90_IDEMIX
      IF ( useIDEMIX .AND. IDEMIX_tau_h .GT. zeroRL ) THEN
       _EXCH_XYZ_RL( GGL90TKE, myThid )
       _EXCH_XYZ_RL( IDEMIX_E, myThid )
      ELSEIF ( GGL90diffTKEh .GT. zeroRL ) THEN
#else /* ALLOW_GGL90_IDEMIX */
      IF ( GGL90diffTKEh .GT. zeroRL ) THEN
#endif /* ALLOW_GGL90_IDEMIX */
       _EXCH_XYZ_RL( GGL90TKE, myThid )
      ENDIF

C--   Just to check that it does not affects results:
c      CALL EXCH_UV_3D_RL( GGL90viscArU, GGL90viscArV,
c    I                     .FALSE., Nr, myThid )
#endif /* ALLOW_GGL90 */

      RETURN
      END
