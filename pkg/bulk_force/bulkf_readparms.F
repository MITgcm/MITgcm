
#include "CPP_OPTIONS.h"

      SUBROUTINE BULKF_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE BULKF_READPARMS                                 |
C     | o Routine to initialize BULKF variables and constants.     |
C     |==========================================================|
C     | Initialize BULKF    parameters, read in data.rbc          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
cswdblk
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif
#endif
cswdblk

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_BULK_FORCE

C--   Bulk Formula parameter
      NAMELIST /BULKF_PARM01/
     &         AirTempFile, AirHumidityFile, RainFile,
     &         SolarFile, LongwaveFile, UWindFile,
     &         VWindFile,  RunoffFile, WSpeedFile, QnetFile,
     &         EmPFile, CloudFile, readwindstress, readsurface

#ifdef CONSERV_BULKF
c-    conserving qnet, empmr
      NAMELIST /BULKF_PARM02/
     &         qnet_off, empmr_off, conservcycle
#endif


C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: opening data.blk'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.blk', 'BULKF_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values 
      AirTempFile=' '
      AirHumidityFile=' '
      RainFile=' '
      SolarFile=' '
      LongwaveFile=' '
      UWindFile=' '
      VWindFile=' '
      WspeedFile=' '
      RunoffFile=' '
      QnetFile=' '
      EmPFile=' '
      CloudFile=' '
      SnowFile=' '
      readwindstress=.TRUE.
      readsurface=.TRUE.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=BULKF_PARM01)


      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#ifdef CONSERV_BULKF
c -- default
      qnet_off=0.d0
      empmr_off=0.d0
      READ(UNIT=iUnit,NML=BULKF_PARM02)
      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: read BULKF_PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#endif /* CONSERV_BULKF */


C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER


#endif /* ALLOW_BULK_FORCE */

      return
      end
