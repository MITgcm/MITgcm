c#include "PACKAGES_CONFIG.h"
#include "ATM_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: ATM_CHECK_CPLCONFIG

C !INTERFACE:
      SUBROUTINE ATM_CHECK_CPLCONFIG(
     U                        errFlag, errMsg,
     I                        landMask, myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_CHECK_CPLCONFIG
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
c#include "GRID.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     errFlag  :: logical flag to report an error
C     errMsg   :: error message to print to clog file
C     landMask :: land / sea mask (=1 : full land; =0 : full ocean grid cell)
C     myThid   :: Thread number for this instance of the routine
      LOGICAL errFlag
      CHARACTER*(*) errMsg
      _RL landMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL atm_waterOnly, atm_landOnly, mxlD_noWater
CEOP

C--   Summarise fields that were imported.
C     o Plot ocean depths
      IF ( debugLevel.GE.debLevB ) THEN
        CALL WRITE_FLD_XY_RL( 'Ocn_MxlD', ' ', ocMxlD, 0, myThid )
      ENDIF
      IF ( plotLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYRL( ocMxlD,
     &                  'Ocean mixed-layer depth on atmos grid',
     &                  1, myThid )
      ENDIF

C--   Report previously found errors
      _BEGIN_MASTER( myThid )

      IF ( cplErrorCount.NE.0 ) THEN
        errFlag = .TRUE.
        WRITE(msgBuf,'(2A,I4,A)') 'ATM_CHECK_CPLCONFIG: ',
     &    ' cplErrorCount=', cplErrorCount, ' (from previous error)'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Do consistency checks on imported fields.
C     o Check that:
C      a) where land/sea mask is "water-only", this should be a wet ocean pts
C      b) where land/sea mask has "no water",  this should be a dry ocean pts
      atm_waterOnly = 0. _d 0
      atm_landOnly  = 1. _d 0
      mxlD_noWater  = 0. _d 0
      DO bj=1,nSy
       DO bi=1,nSx
        DO j=1,sNy
         DO i=1,sNx
          IF ( ( landMask(i,j,bi,bj) .EQ. atm_waterOnly
     &       .AND. ocMxlD(i,j,bi,bj) .EQ. mxlD_noWater )
     &    .OR. ( landMask(i,j,bi,bj) .EQ. atm_landOnly
     &       .AND. ocMxlD(i,j,bi,bj) .NE. mxlD_noWater ) ) THEN
           errFlag = .TRUE.
           WRITE(msgBuf,'(2(A,I6),2(A,I4),A)')
     &     'Inconsistent land/sea mask @ (i=', i, ',j=', j,
     &                              ',bi=', bi, ',bj=', bj, ')'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'Land (atmosphere) ==', landMask(i,j,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'Mxl-Depth (ocean) ==', ocMxlD(i,j,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( errFlag ) WRITE(errMsg,'(A)')
     &   'ATM_CHECK_CPLCONFIG: inconsistent ATM/CPL/OCN config'

      _END_MASTER( myThid )

      RETURN
      END
