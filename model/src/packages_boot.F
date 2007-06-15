C $Header: /u/gcmpack/MITgcm/model/src/packages_boot.F,v 1.41 2007/06/15 20:48:38 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
     &          useOPPS,
     &          usePP81,
     &          useMY82,
     &          useGGL90,
     &          useKPP,
     &          useGMRedi,
     &          useOBCS,
     &          useAIM,
     &          useLand,
     &          useGrdchk,
     &          useEXF,
     &          useECCO,
     &          useSHAP_FILT,
     &          useZONAL_FILT,
     &          useFLT,
     &          usePTRACERS,
     &          useGCHEM,
     &          useRBCS,
     &          useOffLine,
     &          useSBO,
     &          useSEAICE,
     &          useShelfIce,
     &          useThSIce,
     &          useATM2D,
     &          useBulkForce,
     &          useEBM,
     &          usefizhi,
     &          usegridalt,
     &          useDiagnostics,
     &          useMNC,
     &          useREGRID,
     &          useRunClock,
     &          useMATRIX,
     &          useEMBED_FILES,
     &          useMYPACKAGE
CEOP

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: opening data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )


      CALL OPEN_COPY_DATA_FILE(
     I                          'data.pkg', 'PACKAGES_BOOT',
     O                          iUnit,
     I                          myThid )

C--   Default package configuration
      useOPPS         =.FALSE.
      usePP81         =.FALSE.
      useMY82         =.FALSE.
      useGGL90        =.FALSE.
      useKPP          =.FALSE.
      useGMRedi       =.FALSE.
      useOBCS         =.FALSE.
      useAIM          =.FALSE.
      useLand         =.FALSE.
      useCAL          =.FALSE.
      useEXF          =.FALSE.
      useEBM          =.FALSE.
      useGrdchk       =.FALSE.
      useECCO         =.FALSE.
      useSHAP_FILT    =.FALSE.
      useZONAL_FILT   =.FALSE.
      useFLT          =.FALSE.
      usePTRACERS     =.FALSE.
      useGCHEM        =.FALSE.
      useRBCS         =.FALSE.
      useOffLine      =.FALSE.
      useMATRIX       =.FALSE.
      useSBO          =.FALSE.
      useSEAICE       =.FALSE.
      useShelfIce     =.FALSE.
      useThSIce       =.FALSE.
      useATM2D        =.FALSE.
      useBulkForce    =.FALSE.
      usefizhi        =.FALSE.
      usegridalt      =.FALSE.
      useDiagnostics  =.FALSE.
      useMNC          =.FALSE.
      useRunClock     =.FALSE.
      useREGRID       =.FALSE.
      useMYPACKAGE    =.FALSE.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=PACKAGES)


      WRITE(msgBuf,'(A)') ' PACKAGES_BOOT: finished reading data.pkg'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C--   Close the open data file
      CLOSE(iUnit)

C--   packages with hard-coded switch
#ifdef ALLOW_CAL
      IF (useEXF) useCAL = .TRUE.
#endif

#ifdef ALLOW_PROFILES
      useCAL = .TRUE.
#endif

C--   packages with an implied switch
      useGAD = tempStepping .OR. saltStepping .OR. usePTRACERS

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
