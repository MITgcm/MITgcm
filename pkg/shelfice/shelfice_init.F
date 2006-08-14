C $Header: /u/gcmpack/MITgcm/pkg/shelfice/Attic/shelfice_init.F,v 1.3 2006/08/14 16:52:45 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

      SUBROUTINE SHELFICE_INIT_FIXED( myThid )
C     /============================================================\
C     | SUBROUTINE SHELFICE_INIT_FIXED                             |
C     | o Routine to initialize SHELFICE parameters and variables. |
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
C     myThid -  Number of this instance of SHELFICE_INIT
      INTEGER myThid

#ifdef ALLOW_SHELFICE

C     === Local variables ===
C     I,J,K,bi,bj - Loop counters

      INTEGER I, J, K, bi, bj

C-----------------------------------------------------------------------
C Initialize SHELFICE variables kTopC
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          kTopC(i,j,bi,bj) = 0
          DO K = Nr, 1, -1
           IF ( hFacC(I,J,K,bi,bj) .NE. 0. _d 0 ) 
     &          kTopC(I,J,bi,bj) = K
          ENDDO
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
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RS( SHELFICEloadAnomalyFile, ' ', 
     &      shelficeLoadAnomaly, 0, myThid )
       _END_MASTER(myThid)
       _EXCH_XY_R4( shelficeLoadAnomaly, myThid )
      ENDIF

#endif /* ALLOW_SHELFICE */

      return
      end
