C $Header: /u/gcmpack/MITgcm/pkg/shelfice/Attic/shelfice_init.F,v 1.5 2007/03/14 15:43:43 mlosch Exp $
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
#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif /* ALLOW_DIAGNOSTICS */

C-----------------------------------------------------------------------
C Initialize SHELFICE variables kTopC
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          kTopC(i,j,bi,bj) = 0
          DO K = Nr, 1, -1
           IF ( maskC(I,J,K,bi,bj) .NE. 0. _d 0 )
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
C-    must wait for initialisation to be finished before master loads from file
       _BARRIER
       CALL READ_FLD_XY_RS( SHELFICEloadAnomalyFile, ' ',
     &      shelficeLoadAnomaly, 0, myThid )
C-    EXCH call implicitly contains Barrier: no need for an other one.
       _EXCH_XY_R4( shelficeLoadAnomaly, myThid )
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       diagName  = 'SHIfwFlx'
       diagTitle = 'SHELFICE fresh water flux (positive upward)'
       diagUnits = 'm/s             '
       diagCode  = 'SM      M1      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'SHIhtFlx'
       diagTitle = 'SHELFICE heat flux  (positive upward)'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      M1      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'USHIDrag'
       diagTitle = 'U momentum tendency from Shelfice Drag'
       diagUnits = 'm/s^2           '
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )

       diagName  = 'VSHIDrag'
       diagTitle = 'V momentum tendency from Shelfice Drag'
       diagUnits = 'm/s^2           '
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
