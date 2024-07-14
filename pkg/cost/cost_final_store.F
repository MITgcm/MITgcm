#include "COST_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_FINAL_STORE

C     !INTERFACE:
      SUBROUTINE COST_FINAL_STORE( lastDiva, myThid )

C     !DESCRIPTION:
C     Store cost function value
C     This is required in conjunction with DIVA
C     heimbach@mit.edu 5-Mar-2003

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "cost.h"

C     !INPUT/OUTPUT PARAMETERS:
C     lastDiva :: output argument, initialised to False (if DIVA)
C     myThid   :: my Thread Id number
      LOGICAL lastDiva
      INTEGER myThid

#ifdef ALLOW_DIVIDED_ADJOINT
C     !LOCAL VARIABLES:
CEOP

C--   Each process has calculated the global part for itself.
      lastDiva = .FALSE.

      IF ( myProcId .EQ. 0 ) THEN
        _BEGIN_MASTER( myThid )
        OPEN(UNIT=76,FILE='costfinal')
        WRITE(76,*) fc
        CLOSE(76)
        _END_MASTER( myThid )
      ENDIF
      _BARRIER

#endif /* ALLOW_DIVIDED_ADJOINT */

      RETURN
      END
