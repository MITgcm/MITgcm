#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE GGL90_WRITE_PICKUP( permPickup, suff,
     I                               myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE GGL90_WRITE_PICKUP
C     | o write ggl90 pickups
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
C     permPickup :: write a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      prec = precFloat64
      WRITE(fn,'(A,A)') 'pickup_ggl90.',suff

      CALL WRITE_REC_3D_RL( fn, prec, Nr, GGL90TKE, 1, myIter, myThid )

#ifdef ALLOW_GGL90_IDEMIX
      IF (useIDEMIX) THEN
       CALL WRITE_REC_3D_RL( fn, prec, Nr, IDEMIX_E, 2, myIter, myThid )
      ENDIF
#endif

#endif /*  ALLOW_GGL90  */

      RETURN
      END
