#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE GGL90_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE GGL90_READ_PICKUP
C     | o read ggl90 pickups
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of GGL90_READ_CHECKPOINT
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

C--   Suffix for pickup files
      IF (pickupSuff.EQ.' ') THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
        ENDIF
      ELSE
        WRITE(suff,'(A10)') pickupSuff
      ENDIF

      _BARRIER

      prec = precFloat64

      WRITE(fn,'(A,A10)') 'pickup_ggl90.',suff
      CALL READ_REC_3D_RL( fn, prec, Nr, GGL90TKE, 1, myIter, myThid )

      _EXCH_XYZ_RL( GGL90TKE, myThid )

#ifdef ALLOW_GGL90_IDEMIX
      IF (useIDEMIX) THEN
       CALL READ_REC_3D_RL( fn, prec, Nr, IDEMIX_E, 2, myIter, myThid )
       _EXCH_XYZ_RL( IDEMIX_E, myThid )
      ENDIF
#endif

#endif /*  ALLOW_GGL90  */

      RETURN
      END

