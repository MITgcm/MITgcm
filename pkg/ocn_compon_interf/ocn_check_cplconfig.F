#include "OCN_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_CHECK_CPLCONFIG

C !INTERFACE:
      SUBROUTINE OCN_CHECK_CPLCONFIG(
     U                        errFlag, errMsg,
     I                        myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OCN_CHECK_CPLCONFIG
C     | o Check for inconsistency in coupling set-up config
C     *==========================================================*
C     | The routine checks on consistent coupler-exchange config
C     |  and performs some basic checking on consistency between
C     |  components (e.g., land-sea mask);
C     | Also summarises coupling set-up config and output fields
C     |  that were imported
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CPL_PARAMS.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     errFlag  :: logical flag to report an error
C     errMsg   :: error message to print to clog file
C     myThid   :: Thread number for this instance of the routine
      LOGICAL errFlag
      CHARACTER*(*) errMsg
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL atm_waterOnly, atm_landOnly, depth_noWater
CEOP

C--   Summarise fields that were imported.
C     o Plot atmosphere land-mask
      IF ( debugLevel.GE.debLevB ) THEN
        CALL WRITE_FLD_XY_RL( 'Atm_Land', ' ', landMask, 0, myThid )
      ENDIF
      IF ( plotLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYRL( landMask,
     &                  'Atmosphere land/sea mask on ocean grid',
     &                  1, myThid )
      ENDIF

C--   Report previously found errors
      _BEGIN_MASTER( myThid )

      IF ( cplErrorCount.NE.0 ) THEN
        errFlag = .TRUE.
        WRITE(msgBuf,'(2A,I4,A)') 'OCN_CHECK_CPLCONFIG: ',
     &    ' cplErrorCount=', cplErrorCount, ' (from previous error)'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Do consistency checks on imported fields.
C     o Check that:
C      a) where land/sea mask is "water-only", this should be a wet ocean pts
C      b) where land/sea mask has "no water",  this should be a dry ocean pts
      atm_waterOnly = 0. _d 0
      atm_landOnly  = 1. _d 0
      depth_noWater = 0. _d 0
      DO bj=1,nSy
       DO bi=1,nSx
        DO j=1,sNy
         DO i=1,sNx
          IF ( ( landMask(i,j,bi,bj) .EQ. atm_waterOnly
     &        .AND. R_low(i,j,bi,bj) .EQ. depth_noWater )
     &    .OR. ( landMask(i,j,bi,bj) .EQ. atm_landOnly
     &        .AND. R_low(i,j,bi,bj) .NE. depth_noWater ) ) THEN
           errFlag = .TRUE.
           WRITE(msgBuf,'(2(A,I6),2(A,I4),A)')
     &     'Inconsistent land/sea mask @ (i=', i, ',j=', j,
     &                              ',bi=', bi, ',bj=', bj, ')'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'Land (atmosphere) ==', landMask(i,j,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'Depth (ocean)     ==', R_low(i,j,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( errFlag ) WRITE(errMsg,'(A)')
     &   'OCN_CHECK_CPLCONFIG: inconsistent OCN/CPL/ATM config'

      _END_MASTER( myThid )

      RETURN
      END
