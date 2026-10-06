#include "COST_OPTIONS.h"

      SUBROUTINE COST_DEPENDENT_INIT( myThid )

c     ==================================================================
c     SUBROUTINE cost_dependent_init
c     ==================================================================
c
c     o Initialise the variable ad cost function part.
c
c     started: heimbach@mit.edu 17-Jan-2002
c
c     ==================================================================
c     SUBROUTINE cost_dependent_init
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "cost.h"
#include "adcost.h"
#ifdef ALLOW_TAPENADE
# include "COST_TAP_ADJ.h"
#endif

c     == routine arguments ==
      INTEGER myThid

c     == local variables ==
#if ( defined ALLOW_COST_VECTOR || defined  ALLOW_COST_STATE_FINAL )
      INTEGER i
#endif
#ifdef ALLOW_COST_STATE_FINAL
      INTEGER j, k
#endif
      INTEGER bi,bj
c     == end of interface ==

#ifdef ALLOW_OPENAD
      fc%v = 0.0
#else
      fc   = 0. _d 0
#endif

#ifdef ALLOW_AUTODIFF

#ifdef ALLOW_TAPENADE
      fcb = 0. _d 0
      IF ( myProcId .EQ. 0 ) fcb = 1. _d 0
#endif

#ifdef ALLOW_OPENAD
      adfc = 1.0
#else
      adfc = 0. _d 0
      IF ( myProcId .EQ. 0 ) adfc = 1. _d 0
#endif

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_COST_VECTOR
         DO i=1,sNx
          objf_vector(i,bi,bj) = 0. _d 0
          adobjf_vector(i,bi,bj) = 1. _d 0
         ENDDO
#endif
#ifdef ALLOW_COST_STATE_FINAL
         DO k=1,4*Nr+1
          DO j=1,sNy
           DO i=1,sNx
            objf_state_final(i,j,bi,bj,k) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
cph No init. of cost_state_final here,
cph because we need it in ADM*TLM
#endif
       ENDDO
      ENDDO

#endif /* ALLOW_AUTODIFF */

      RETURN
      END
