#include "COST_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_FINAL_RESTORE

C     !INTERFACE:
      SUBROUTINE COST_FINAL_RESTORE( lastDiva, myThid )

C     !DESCRIPTION:
C     Restore cost function value
C     This is required in conjunction with DIVA
C     heimbach@mit.edu 5-Mar-2003

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "cost.h"

C     !INPUT/OUTPUT PARAMETERS:
C     lastDiva :: True if last AD run from DIVA sequence, False otherwise
C     myThid   :: my Thread Id number
      LOGICAL lastDiva
      INTEGER myThid

#ifdef ALLOW_DIVIDED_ADJOINT
C     !LOCAL VARIABLES:
      LOGICAL exst
      INTEGER idivbeg, idivend
CEOP

      _BEGIN_MASTER( myThid )

        INQUIRE(FILE='divided.ctrl',EXIST=exst)
        IF (exst) THEN
          OPEN(UNIT=76,FILE='divided.ctrl',FORM='formatted')
          READ(UNIT=76,FMT=*) idivbeg,idivend
          CLOSE(UNIT=76)
        ELSE
          idivbeg = -2
        ENDIF

        IF ( idivbeg .EQ. 0 ) THEN
          lastDiva = .TRUE.
          OPEN(UNIT=76,FILE='costfinal')
          READ(76,*) fc
          CLOSE(76)
        ELSE
          lastDiva = .FALSE.
        ENDIF

      _END_MASTER( myThid )
      _BARRIER

#endif /* ALLOW_DIVIDED_ADJOINT */

      RETURN
      END
