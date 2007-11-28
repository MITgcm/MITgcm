C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_readparms.F,v 1.1 2007/11/28 09:26:16 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C !ROUTINE: SALT_PLUME_READPARMS

C !INTERFACE:
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

      NAMELIST /SALT_PLUME_PARM01/ SaltPlumeCriterion

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

C--   Default values for SALT_PLUME
      SaltPlumeCriterion  = 0.4 _d 0
      
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

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
