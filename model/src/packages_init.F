C $Header: /u/gcmpack/MITgcm/model/src/Attic/packages_init.F,v 1.1 2000/06/21 19:25:06 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGE_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGE_INIT                                  |
C     | o Routine to initialize package configuration            |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C--   data.pkg namelists
      NAMELIST /PACKAGES/
     &          use_KPPmixing,
     &          use_GMRedi

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' PACKAGE_INIT: opening data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.pkg', 'PACKAGE_INIT',
     O                          iUnit,
     I                          myThid )

C--   Default package configuration
      use_GMRedi=.FALSE.
      use_KPPmixing=.FALSE.


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=PACKAGES)


      WRITE(msgBuf,'(A)') ' PACKAGE_INIT: finished reading data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      return
      end
