C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_init_varia.F,v 1.2 2008/09/12 07:21:55 mlosch Exp $
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

C     === Routine arguments ===
C     myThid -  Number of this instance of SHELFICE_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SHELFICE
C     === Local variables ===
C     I,J,bi,bj - Loop counters
      INTEGER I, J, bi, bj
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
      IF ( SHELFICEloadAnomalyFile .NE. ' ' ) THEN
C-    must wait for initialisation to be finished before master loads from file
       _BARRIER
       CALL READ_FLD_XY_RS( SHELFICEloadAnomalyFile, ' ',
     &      shelficeLoadAnomaly, 0, myThid )
C-    EXCH call implicitly contains Barrier: no need for an other one.
       _EXCH_XY_R4( shelficeLoadAnomaly, myThid )
      ENDIF

#endif /* ALLOW_SHELFICE */

      RETURN
      END

