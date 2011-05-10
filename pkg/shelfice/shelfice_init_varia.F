C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_init_varia.F,v 1.5 2011/05/10 07:49:19 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"


CBOP
      SUBROUTINE SHELFICE_INIT_VARIA( myThid )
C     /============================================================\
C     | SUBROUTINE SHELFICE_INIT_VARIA                             |
C     | o Routine to initialize SHELFICE variables.                |
C     |============================================================|
C     | Initialize SHELFICE parameters and variables.              |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
#ifdef ALLOW_COST
# include "SHELFICE_COST.h"
#endif /* ALLOW_COST */

C     === Routine arguments ===
C     myThid -  Number of this instance of SHELFICE_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SHELFICE
C     === Local variables ===
C     I,J,bi,bj - Loop counters
      INTEGER I, J, K, bi, bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          shelficeLoadAnomaly   (i,j,bi,bj) = 0. _d 0
          shelficeForcingT      (i,j,bi,bj) = 0. _d 0
          shelficeForcingS      (i,j,bi,bj) = 0. _d 0
          shelficeHeatFlux      (i,j,bi,bj) = 0. _d 0
          shelficeFreshWaterFlux(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_COST
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
ccc        DO K = 1, Nr
         DO J = 1-OLy, sNy+OLy
          DO I = 1-OLx, sNx+OLx
           cMeanSHIforT (i,j,bi,bj) = 0. _d 0
           cMeanSHIforS (i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
ccc        ENDDO
       ENDDO
      ENDDO
c--
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        objf_shelfice(bi,bj) = 0. _d 0
        objf_shifwflx(bi,bj) = 0. _d 0
       ENDDO
      ENDDO
#endif /* ALLOW_COST */

      IF ( SHELFICEloadAnomalyFile .NE. ' ' ) THEN
C-    must wait for initialisation to be finished before master loads from file
       _BARRIER
       CALL READ_FLD_XY_RS( SHELFICEloadAnomalyFile, ' ',
     &      shelficeLoadAnomaly, 0, myThid )
C-    EXCH call implicitly contains Barrier: no need for an other one.
       _EXCH_XY_RS( shelficeLoadAnomaly, myThid )
      ENDIF

#endif /* ALLOW_SHELFICE */

      RETURN
      END

