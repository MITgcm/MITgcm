C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_readparms.F,v 1.3 2007/12/18 05:34:41 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_READPARMS
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize SALT_PLUME variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME

      NAMELIST /SALT_PLUME_PARM01/
     &     CriterionType, PlumeMethod, Npower,
     &     SaltPlumeCriterion, SPovershoot

C     !LOCAL VARIABLES:
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

C--   Default values for SALT_PLUME
      CriterionType       = 1
      PlumeMethod         = 1
      Npower              = 0
      SaltPlumeCriterion  = UNSET_RL
      SPovershoot         = 1.0 _d 0

      WRITE(msgBuf,'(A)')'SALT_PLUME_READPARMS: opening data.salt_plume'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.salt_plume', 'SALT_PLUME_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=SALT_PLUME_PARM01)
      WRITE(msgBuf,'(A)') 
     &    'SALT_PLUME_READPARMS: finished reading data.salt_plume'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
      CLOSE(iUnit)

       IF ( SaltPlumeCriterion .EQ. UNSET_RL ) THEN
          IF ( CriterionType .EQ. 1 ) THEN
             SaltPlumeCriterion  = 0.4 _d 0
          ELSEIF ( CriterionType .EQ. 2 ) THEN
             SaltPlumeCriterion  = 0.005 _d 0
          ELSE
             WRITE(msgBuf,'(A)') 'S/R SALT_PLUME_READPARMS'
             CALL PRINT_ERROR( msgBuf, myThid )
             WRITE(msgBuf,'(A)') 'Invalid CriterionType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R SALT_PLUME_READPARMS'
          ENDIF
       ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
