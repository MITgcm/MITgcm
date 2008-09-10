C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_init_fixed.F,v 1.1 2008/09/10 09:18:35 mlosch Exp $
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
C     myThid -  Number of this instance of SHELFICE_INIT_FIXED
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
C--   Initialize SHELFICE variables kTopC
C--   kTopC is the same as kSurfC, except for places over land.
C--   Over land (completely dry cells), kTop = 0, while kSurfC = Nr+1
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
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       diagName  = 'SHIfwFlx'
       diagTitle = 'SHELFICE fresh water flux (positive upward)'
       diagUnits = 'kg/m^2/s        '
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

