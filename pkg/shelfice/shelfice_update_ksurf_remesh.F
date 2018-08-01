#include "SHELFICE_OPTIONS.h"


      SUBROUTINE SHELFICE_UPDATE_KSURF_REMESH( myThid )
C     *============================================================*
C     | SUBROUTINE SHELFICE_INIT_FIXED
C     | o Routine to initialize SHELFICE parameters and variables.
C     *============================================================*
C     | Initialize SHELFICE parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
#ifdef ALLOW_COST
# include "cost.h"
# include "SHELFICE_COST.h"
#endif /* ALLOW_COST */

C     === Routine arguments ===
C     myThid ::  Number of this instance of SHELFICE_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_SHELFICE_REMESHING
C     === Local variables ===
C     i, j, bi, bj :: Loop counters
      INTEGER i, j, bi, bj
#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif /* ALLOW_DIAGNOSTICS */




C-----------------------------------------------------------------------
C--   Initialize SHELFICE variables kTopC
C--   kTopC is the same as kSurfC, except outside ice-shelf area:
C--   kTop = 0 where there is no ice-shelf (where kSurfC=1)
C--   and over land (completely dry column) where kSurfC = Nr+1
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &         R_shelfIce(i,j,bi,bj).LT.zeroRS ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
          ELSE
            kTopC(i,j,bi,bj) = 0
          ENDIF
          
         ENDDO
        ENDDO
       ENDDO
      ENDDO


#endif
#endif /* ALLOW_SHELFICE */

      RETURN
      END
