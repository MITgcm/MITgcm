C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_init_fixed.F,v 1.8 2012/03/02 15:05:50 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

      SUBROUTINE SHELFICE_INIT_FIXED( myThid )
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
C     myThid -  Number of this instance of SHELFICE_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_SHELFICE
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
#if  (defined (ALLOW_SHIFWFLX_COST_CONTRIBUTION) && \
      defined (ALLOW_SHIFWFLX_CONTROL))
      _RL dummy
#endif

#ifdef ALLOW_MNC
C     Initialize MNC variable information for SHELFICE
      IF ( useMNC .AND. (shelfice_tave_mnc.OR.shelfice_dump_mnc)
     &    ) THEN
        CALL SHELFICE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

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
#ifdef ALLOW_COST
#if  (defined (ALLOW_SHIFWFLX_COST_CONTRIBUTION) && \
      defined (ALLOW_SHIFWFLX_CONTROL))
      IF ( shifwflx_errfile .NE. ' ' ) THEN
       CALL READ_REC_XY_RL( shifwflx_errfile, wshifwflx, 1, 0, myThid )
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
c--   Test for missing values.
          IF (wshifwflx(i,j,bi,bj) .LT. -9900.) THEN
           wshifwflx(i,j,bi,bj) = 0. _d 0
          ENDIF
c--   use weight as mask
          wshifwflx(i,j,bi,bj) =
     &         max(wshifwflx(i,j,bi,bj),wshifwflx0)
     &         *maskSHI(I,J,1,bi,bj)
          IF (wshifwflx(i,j,bi,bj) .NE. 0.) THEN
           wshifwflx(i,j,bi,bj) =
     &          1./wshifwflx(i,j,bi,bj)/wshifwflx(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL ACTIVE_WRITE_XY_LOC( 'wshifwflx', wshifwflx,
     &    1, 0, mythid, dummy)
#endif /* ALLOW_SHIFWFLX_COST_CONTRIBUTION or ALLOW_SHIFWFLX_CONTROL */
#endif /* ALLOW_COST */

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

       diagName  = 'SHIgammT'
       diagTitle = 'Ice shelf exchange coefficient for theta'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIgammS'
       diagTitle = 'Ice shelf exchange coefficient for salt'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
