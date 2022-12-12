#include "ECCO_OPTIONS.h"

CBOP
C     !ROUTINE: ECCO_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE ECCO_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE ECCO_READ_PICKUP
C     | o read ecco pickups
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
C     myThid -  Number of this instance of ECCO_READ_CHECKPOINT
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PSBAR_STERIC

C !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn, fntmp
      CHARACTER*(10) suff
      INTEGER prec
      INTEGER IL
      LOGICAL exst
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

      WRITE(fn,'(A,A10)') 'pickup_ecco.',suff
C-    Check first for global file with simple name (ie. fn)
      INQUIRE( file=fn, exist=exst )
      IF ( .NOT.exst ) THEN
C-    Check for global file with ".data" suffix
       IL  = ILNBLNK( fn )
       WRITE(fntmp,'(2A)') fn(1:IL),'.data'
       INQUIRE( file=fntmp, exist=exst )
      ENDIF

      IF (exst) THEN
        CALL READ_GLVEC_RL( fn, ' ',
     &                      VOLsumGlob_0, 1, 1, myThid )
        CALL READ_GLVEC_RL( fn, ' ',
     &                      RHOsumGlob_0, 1, 2, myThid )
      ENDIF

#endif /*  ALLOW_PSBAR_STERIC  */

      RETURN
      END

