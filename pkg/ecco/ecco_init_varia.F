#include "ECCO_OPTIONS.h"

      SUBROUTINE ECCO_INIT_VARIA( myThid )

C     ==================================================================
C     SUBROUTINE ecco_init_varia
C     ==================================================================
C
C     o Initialise ecco variables.
C
C     ==================================================================
C     SUBROUTINE ecco_cost_init_varia
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif

C     == routine arguments ==
      INTEGER myThid

C     == local variables ==

C     == end of interface ==

#ifdef ALLOW_PSBAR_STERIC
# ifndef ALLOW_AUTODIFF
      _BEGIN_MASTER(myThid)
# endif
      RHOsumGlob_0 = 0. _d 0
      VOLsumGlob_0 = 0. _d 0
# ifndef ALLOW_AUTODIFF
      _END_MASTER(myThid)
# endif
      IF ( .NOT. ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ') ) THEN
        CALL ECCO_READ_PICKUP ( nIter0, myThid )
      ENDIF
#endif /* ALLOW_PSBAR_STERIC */

      CALL ECCO_PHYS( startTime, -1, myThid )

#ifdef ALLOW_PSBAR_STERIC
C RHO/VOLsumGlob_0 are zeros if S/R ECCO_READ_PICKUP is not called
C or pickup files (pickup_ecco.*.data/meta) do not exist. Assign
C RHO/VOLsumGlob calculated in S/R ECCO_PHYS to RHO/VOLsumGlob_0.
      _BEGIN_MASTER(myThid)
      IF ( RHOsumGlob_0 .EQ. 0. _d 0 .AND.
     &     VOLsumGlob_0 .EQ. 0. _d 0 ) THEN
        RHOsumGlob_0 = RHOsumGlob
        VOLsumGlob_0 = VOLsumGlob
      ENDIF
      _END_MASTER(myThid)
#endif

      CALL ECCO_COST_INIT_VARIA( myThid )

      _BARRIER

      RETURN
      END
