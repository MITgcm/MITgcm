
#include "CPP_OPTIONS.h"
#include "OFFLINE_OPTIONS.h"

      SUBROUTINE OFFLINE_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE OFFLINE_READPARMS                                 |
C     | o Routine to initialize OFFLINE variables and constants.     |
C     |==========================================================|
C     | Initialize OFFLINE    parameters, read in data.off          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_OFFLINE
#include "OFFLINE.h"
#endif

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_OFFLINE    
C--   Bulk Formula parameter
      NAMELIST /OFFLINE_PARM01/
     &         UvelFile, VvelFile, WvelFile, ThetFile,
     &         SaltFile, ConvFile, GMwxFile, GMwyFile,
     &         GMwzFile, HfluxFile, SfluxFile, KPP_DiffSFile,
     &         KPP_ghatFile

      NAMELIST /OFFLINE_PARM02/
     &         offlineIter0, offlineOffsetIter,
     &         deltaToffline, offlineForcingPeriod, 
     &         offlineForcingCycle

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' OFFLINE_READPARMS: opening data.off'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.off', 'OFFLINE_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values 
      UvelFile=' '
      VvelFile=' '
      WvelFile=' '
      ThetFile=' '
      SaltFile='  '
      ConvFile=' '
      GMwxFile=' '
      GMwyFile=' '
      GMwzFile=' ' 
      HFluxFile=' '
      SFluxFile=' '
      KPP_DiffSFile=' '
      KPP_ghatFile=' '

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=OFFLINE_PARM01)

      deltaToffline=deltaTclock
      offlineIter0=nIter0       !initial offline field timestep
      offlineOffsetIter=0        !offset
      offlineForcingPeriod=2592000.
      offlineForcingCycle=31104000.

     
C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=OFFLINE_PARM02)

      WRITE(msgBuf,'(A)')
     &      ' OFFLINE_READPARMS: finished reading data.off'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER


#endif /* ALLOW_OFFLINE */

      return
      end
