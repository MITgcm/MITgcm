C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_init_varia.F,v 1.1 2010/01/20 23:33:45 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"


CBOP
      SUBROUTINE ICEFRONT_INIT_VARIA( myThid )
C     /============================================================\
C     | SUBROUTINE ICEFRONT_INIT_VARIA                             |
C     | o Routine to initialize ICEFRONT variables.                |
C     |============================================================|
C     | Initialize ICEFRONT parameters and variables.              |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEFRONT.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_ICEFRONT
C     === Local variables ===
C     I,J,bi,bj - Loop counters
      INTEGER I, J, bi, bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          icefrontLoadAnomaly   (i,j,bi,bj) = 0. _d 0
          icefrontForcingT      (i,j,bi,bj) = 0. _d 0
          icefrontForcingS      (i,j,bi,bj) = 0. _d 0
          icefrontHeatFlux      (i,j,bi,bj) = 0. _d 0
          icefrontFreshWaterFlux(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      IF ( ICEFRONTloadAnomalyFile .NE. ' ' ) THEN
C-    must wait for initialisation to be finished before master loads from file
       _BARRIER
       CALL READ_FLD_XY_RS( ICEFRONTloadAnomalyFile, ' ',
     &      icefrontLoadAnomaly, 0, myThid )
C-    EXCH call implicitly contains Barrier: no need for an other one.
       _EXCH_XY_RS( icefrontLoadAnomaly, myThid )
      ENDIF

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
