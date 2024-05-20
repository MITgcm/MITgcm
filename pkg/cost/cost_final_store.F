#include "COST_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      SUBROUTINE COST_FINAL_STORE( lastdiva, myThid )

c     ==================================================================
c     SUBROUTINE cost_final_store
c     ==================================================================
c
c     Store cost function value
c     This is required in conjunction with DIVA
c     heimbach@mit.edu 5-Mar-2003
c
c     ==================================================================
c     SUBROUTINE cost_final_store
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
c     == end of interface ==

c--   Each process has calculated the global part for itself.
      lastdiva = .FALSE.

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
