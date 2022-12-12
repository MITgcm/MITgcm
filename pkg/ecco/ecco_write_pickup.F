#include "ECCO_OPTIONS.h"

CBOP
C     !ROUTINE: ECCO_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE ECCO_WRITE_PICKUP( permPickup, suff,
     I                               myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE ECCO_WRITE_PICKUP
C     | o write ecco pickups
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

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

#ifdef ALLOW_PSBAR_STERIC

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      _RL tmparr(2)
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(fn,'(A,A10)') 'pickup_ecco.',suff
      IF (fn .NE. ' ') THEN
        tmparr(1) = VOLsumGlob_0
        tmparr(2) = RHOsumGlob_0
        CALL WRITE_GLVEC_RL( fn, ' ',
     &                      tmparr, 2, myIter, myThid )
      ENDIF

#endif /*  ALLOW_PSBAR_STERIC  */

      RETURN
      END
