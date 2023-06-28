#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_COST_INIT_VARIA( myThid )

C     ==================================================================
C     SUBROUTINE SEAICE_COST_INIT_VARIA
C     ==================================================================
C     o Initialise the variable cost function part.
C     added sea-ice term: menemenlis@jpl.nasa.gov 26-Feb-2003
C
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_COST
# include "SEAICE_SIZE.h"
# include "SEAICE_COST.h"
#endif

C     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_COST
C     == local variables ==
      INTEGER bi,bj
#ifdef ALLOW_SEAICE_COST_EXPORT
      INTEGER i,j
#endif
C     == end of interface ==

C--   Initialize the tiled cost function contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          objf_ice(bi,bj)        = 0. _d 0
          objf_ice_export(bi,bj) = 0. _d 0
          num_ice(bi,bj)         = 0. _d 0
#ifdef ALLOW_SEAICE_COST_EXPORT
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
               uHeffExportCell(i,j,bi,bj) = 0. _d 0
               vHeffExportCell(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
#endif
        ENDDO
      ENDDO

#endif /* ALLOW_COST */

      RETURN
      END
