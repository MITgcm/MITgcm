C $Header: /u/gcmpack/MITgcm/model/src/packages_boot.F,v 1.7 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_BOOT
C     !INTERFACE:
      SUBROUTINE PACKAGES_BOOT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_BOOT                                  
C     | o Routine to parse runtime package selection file         
C     *==========================================================*
C     | Routine reads a flat file which contains a single flag    
C     | for each known "package". Flag can enables runtime        
C     | activation or deactivation of a package. The data is read 
C     | from a file called data.pkg.                              
C     | Note - If a runtime activation flag is set on but the     
C     |        package code was not included in the build then    
C     |        the default behavior is to stop with an error.     
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C     data.pkg namelists
      NAMELIST /PACKAGES/
     &          useKPP,
     &          useGMRedi,
     &          useOBCS,
     &          useAIM,
     &          useGrdchk,
     &          useECCO,
     &          useSHAP_FILT,
     &          useFLT
CEOP

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: opening data.pkg'
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
      useSHAP_FILT=.FALSE.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=PACKAGES)


      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: finished reading data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      return
      end
