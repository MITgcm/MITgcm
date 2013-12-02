C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_exch_configs.F,v 1.3 2013/12/02 22:16:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_EXCH_CONFIGS

C !INTERFACE:
      SUBROUTINE CPL_EXCH_CONFIGS( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_EXCH_CONFIGS
C     | o Controlling routine for initial config exchange between
C     |   component models and ocean component.
C     | - Oceanic version -
C     *==========================================================*
C     | Controls the import of configuration information
C     | (grid/topography,etc...) from other components and the
C     | export of configuration information from this component.
C     | The routine does some basic checking on consistency
C     | components and summarizes the information that has been
C     | imported.
C     | The routine will need to be customised for different
C     | styles of coupled run. The coupler requires consistency
C     | between sending and receiving operations posted by
C     | various components. Therefore changes in one component
C     | model CPL_EXCH_CONFIG may require changes in other
C     | component models CPL_EXCH_CONFIG routines as well
C     | as in the coupler EXCH_COMPONENT_CONFIG routine.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      LOGICAL errFlag
      CHARACTER*70 errMsg
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL atm_waterOnly, atm_landOnly, depth_noWater
CEOP

C     Post my configuration information to the coupler "layer".
      CALL OCN_EXPORT_OCNCONFIG( myThid )

C     Import other component model(s) configuration(s) from the
C     coupler "layer".
C     o Get atmospheric model configuration
      CALL OCN_IMPORT_ATMCONFIG( myThid )

C     Summarise fields that were imported.
C     o Plot atmosphere orography
      IF ( debugLevel.GE.debLevB ) THEN
        CALL WRITE_FLD_XY_RL( 'Atm_Land', ' ', landMask, 0, myThid )
      ENDIF
      IF ( debugLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYRL( landMask,
     &                  'Atmosphere land/sea mask on ocean grid',
     &                  1, myThid )
      ENDIF

C     Do consistency checks on imported fields.
C     o Check that:
C      a) where land/sea mask is "water-only", this should be a wet ocean pts
C      b) where land/sea mask has "no water",  this should be a dry ocean pts
      _BARRIER
      _BEGIN_MASTER( myThid )
      errFlag = .FALSE.
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

      errMsg  = ' '
      IF ( errFlag ) WRITE(errMsg,'(A)')
     &   'OCN_EXCH_CONFIGS: Atm & Oce Land/Sea mask are inconsistent'

C--   All procs in World check for error and stop if any
      CALL MITCPLR_ALL_CHECK( errFlag, errMsg )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
