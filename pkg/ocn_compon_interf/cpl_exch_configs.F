C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_exch_configs.F,v 1.2 2009/12/24 16:48:30 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_EXCH_CONFIGS

C !INTERFACE:
      SUBROUTINE CPL_EXCH_CONFIGS( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_EXCH_CONFIGS                              |
C     | o Controlling routine for initial config exchange between|
C     |   component models and ocean component.                  |
C     | - Oceanic version -                                      |
C     *==========================================================*
C     | Controls the import of configuration information         |
C     | (grid/topography,etc...) from other components and the   |
C     | export of configuration information from this component. |
C     | The routine does some basic checking on consistency      |
C     | components and summarizes the information that has been  |
C     | imported.                                                |
C     | The routine will need to be customised for different     |
C     | styles of coupled run. The coupler requires consistency  |
C     | between sending and receiving operations posted by       |
C     | various components. Therefore changes in one component   |
C     | model CPL_EXCH_CONFIG may require changes in other       |
C     | component models CPL_EXCH_CONFIG routines as well        |
C     | as in the CPL_MASTER_EXCH_CONFIG routine.                |
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

C     == Local variables ==
      INTEGER I,J,K,bi,bj
      INTEGER iDiff
      INTEGER jDiff
      INTEGER biDiff
      INTEGER bjDiff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL seaSurface_ocn
      _RL groundLevel_atm

C     Post my configuration information to the coupler "layer".
      CALL OCN_EXPORT_OCNCONFIG( myThid )

C     Import other component model(s) configuration(s) from the
C     coupler "layer".
C     o Get atmospheric model configuration
      CALL OCN_IMPORT_ATMCONFIG( myThid )

C     Summarise fields that were imported.
C     o Plot atmosphere orography
      CALL PLOT_FIELD_XYRL( Hatm,
     &                      'Atmosphere orography on ocean grid',
     &                      1, myThid )

C     Do consistency checks on imported fields.
C     o Check that atmos. depth is equal to sea-level for all ocean points.
C       Wherever there is ocean check the atmos extends to the sea-surface.
      iDiff  = 0
      jDiff  = 0
      biDiff = 0
      bjDiff = 0
      seaSurface_ocn  = 0. _d 0
      groundLevel_atm = 1. _d 5
      DO bj=myByLo(myTHid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          IF ( R_low(I,J,bi,bj) .NE. seaSurface_ocn .AND.
     &          Hatm(I,J,bi,bj) .NE. groundLevel_atm ) THEN
           iDiff  = I
           jDiff  = J
           biDiff = bi
           bjDiff = bj
           WRITE(msgBuf,'(A,I4,A,I4,A,I4,A,I4,A)')
     &     'Inconsistent land/sea mask @ (i=',iDiff,
     &     ',j=',jDiff,',bi=',biDiff,',bj=',bjDiff,')'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'H (atmosphere) ==',Hatm(I,J,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(A,E30.15)')
     &     'H (ocean)      ==',R_low(I,J,bi,bj)
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( iDiff .NE. 0 ) THEN
C      At least one point had land/sea "inconsistency" between atmos.
C      and ocean. Stop if this happens.
Ccnh       STOP 'ABNORMAL END: S/R OCN_EXCH_CONFIGS'
      ENDIF

      RETURN
      END
