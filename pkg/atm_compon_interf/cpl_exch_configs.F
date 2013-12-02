C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_exch_configs.F,v 1.4 2013/12/02 22:13:23 jmc Exp $
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
C     |   component models and atmosphere component.
C     | - Atmospheric version -
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
#include "ATMCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      LOGICAL errFlag
      CHARACTER*70 errMsg
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL atm_waterOnly, atm_landOnly, mxlD_noWater
C--   local variable in common block
      _RL landMask_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON / CPL_EXCH_CONFIGS_LOC / landMask_loc
CEOP

      errFlag = .FALSE.

C     Get configuration information (=land/sea mask) from other pkg
      CALL ATM_GET_ATMCONFIG(
     O                        landMask_loc,
     I                        myThid )

C     Post my configuration information to the coupler "layer".
      CALL ATM_EXPORT_ATMCONFIG(
     U                        errFlag,
     I                        landMask_loc, myThid )

C     Import other component model(s) configuration(s) from the coupler "layer"
C     o Get ocean model configuration
      CALL ATM_IMPORT_OCNCONFIG( myThid )

C     Summarise fields that were imported.
C     o Plot ocean depths
      IF ( debugLevel.GE.debLevB ) THEN
        CALL WRITE_FLD_XY_RL( 'Ocn_MxlD', ' ', ocMxlD, 0, myThid )
      ENDIF
      IF ( debugLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYRL( ocMxlD,
     &                  'Ocean mixed-layer depth on atmos grid',
     &                  1, myThid )
      ENDIF

C     Do consistency checks on imported fields.
C     o Check that:
C      a) where land/sea mask is "water-only", this should be a wet ocean pts
C      b) where land/sea mask has "no water",  this should be a dry ocean pts
      _BARRIER
      _BEGIN_MASTER( myThid )
      atm_waterOnly = 0. _d 0
      atm_landOnly  = 1. _d 0
      mxlD_noWater  = 0. _d 0
      DO bj=1,nSy
       DO bi=1,nSx
        DO j=1,sNy
         DO i=1,sNx
          IF ( ( landMask_loc(i,j,bi,bj) .EQ. atm_waterOnly
     &           .AND. ocMxlD(i,j,bi,bj) .EQ. mxlD_noWater )
     &    .OR. ( landMask_loc(i,j,bi,bj) .EQ. atm_landOnly
     &           .AND. ocMxlD(i,j,bi,bj) .NE. mxlD_noWater ) ) THEN
           errFlag = .TRUE.
           WRITE(msgBuf,'(2(A,I6),2(A,I4),A)')
     &     'Inconsistent land/sea mask @ (i=', i, ',j=', j,
     &                              ',bi=', bi, ',bj=', bj, ')'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'Land (atmosphere) ==', landMask_loc(i,j,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'Mxl-Depth (ocean) ==', ocMxlD(i,j,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      errMsg  = ' '
      IF ( errFlag ) WRITE(errMsg,'(A)')
     &   'ATM_EXCH_CONFIGS: Oce & Atm configs are inconsistent'

C--   All procs in World check for error and stop if any
      CALL MITCPLR_ALL_CHECK( errFlag, errMsg )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
