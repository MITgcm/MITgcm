C $Header: /u/gcmpack/MITgcm/model/src/packages_boot.F,v 1.3 2001/02/20 19:39:42 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_BOOT( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_BOOT                                 |
C     | o Routine to parse runtime package selection file        |
C     |==========================================================|
C     | Routine reads a flat file which contains a single flag   |
C     | for each known "package". Flag can enables runtime       |
C     | activation or deactivation of a package. The data is read|
C     | from a file called data.pkg.                             |
C     | Note - If a runtime activation flag is set on but the    |
C     |        package code was not included in the build then   |
C     |        the default behavior is to stop with an error.    |
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
     &          useKPP,
     &          useGMRedi,
     &          useOBCS,
     &          useAIM,
     &          useECCO

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
     I                          'data.pkg', 'PACKAGES_BOOT',
     O                          iUnit,
     I                          myThid )

C--   Default package configuration
      useGMRedi=.FALSE.
      useKPP=.FALSE.
      useOBCS=.FALSE.
      useAIM=.FALSE.
      useECCO=.FALSE.

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
