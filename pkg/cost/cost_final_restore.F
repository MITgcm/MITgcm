#include "COST_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      SUBROUTINE COST_FINAL_RESTORE( lastdiva, myThid )

c     ==================================================================
c     SUBROUTINE cost_final_restore
c     ==================================================================
c
c     Restore cost function value
c     This is required in conjunction with DIVA
c     heimbach@mit.edu 5-Mar-2003
c
c     ==================================================================
c     SUBROUTINE cost_final_restore
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "cost.h"

c     == routine arguments ==
      LOGICAL lastdiva
      INTEGER myThid

#ifdef ALLOW_DIVIDED_ADJOINT
c     == local variables ==
      LOGICAL exst
      INTEGER idivbeg, idivend
c     == end of interface ==

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
          lastdiva = .TRUE.
          OPEN(UNIT=76,FILE='costfinal')
          READ(76,*) fc
          CLOSE(76)
        ELSE
          lastdiva = .FALSE.
        ENDIF

      _END_MASTER( myThid )
      _BARRIER

#endif /* ALLOW_DIVIDED_ADJOINT */

      RETURN
      END
