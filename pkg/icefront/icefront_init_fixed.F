C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_init_fixed.F,v 1.1 2010/01/20 23:33:45 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

      SUBROUTINE ICEFRONT_INIT_FIXED( myThid )
C     *============================================================*
C     | SUBROUTINE ICEFRONT_INIT_FIXED
C     | o Routine to initialize ICEFRONT parameters and variables.
C     *============================================================*
C     | Initialize ICEFRONT parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEFRONT.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_ICEFRONT
C     === Local variables ===
C     I,J,K,bi,bj - Loop counters
      INTEGER I, J, K, bi, bj
#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_MNC
C     Initialize MNC variable information for ICEFRONT
      IF ( useMNC .AND. (icefront_tave_mnc.OR.icefront_dump_mnc)
     &    ) THEN
        CALL ICEFRONT_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

C-----------------------------------------------------------------------
C--   Initialize ICEFRONT variables kTopC
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
       diagTitle = 'Ice shelf fresh water flux (positive upward)'
       diagUnits = 'kg/m^2/s        '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIhtFlx'
       diagTitle = 'Ice shelf heat flux  (positive upward)'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIUDrag'
       diagTitle = 'U momentum tendency from ice shelf drag'
       diagUnits = 'm/s^2           '
       diagCode  = 'UU      L1      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'SHIVDrag'
       diagTitle = 'V momentum tendency from ice shelf drag'
       diagUnits = 'm/s^2           '
       diagCode  = 'VV      L1      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'SHIForcT'
       diagTitle = 'Ice shelf forcing for theta, >0 increases theta'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIForcS'
       diagTitle = 'Ice shelf forcing for salt, >0 increases salt'
       diagUnits = 'g/m^2/s         '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ICEFRONT */

      RETURN
      END
