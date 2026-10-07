#include "GCHEM_OPTIONS.h"

CBOP
C     !ROUTINE: CFC_READPARMS
C     !INTERFACE:
      SUBROUTINE CFC_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CFC_READPARMS
C     | o Initialise and read cfc package parameters
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CFC.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_CFC

C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     iUnit     :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
C     retired parameters
      INTEGER nRetired
      _RL CFC_forcingPeriod
      _RL CFC_forcingCycle
      CHARACTER*(MAX_LEN_FNAM) CFC_windFile
      CHARACTER*(MAX_LEN_FNAM) CFC_atmospFile
      CHARACTER*(MAX_LEN_FNAM) CFC_iceFile

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      NAMELIST /CFC_FORCING/
     &          atmCFC_inpFile, atmCFC_recSepTime, atmCFC_timeOffset,
     &          atmCFC_yNorthBnd, atmCFC_ySouthBnd,
     &          CFC_windFile, CFC_atmospFile, CFC_iceFile,
     &          CFC_forcingPeriod, CFC_forcingCycle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

       atmCFC_inpFile    = 'cfc1112.atm'
       atmCFC_recSepTime = 360. _d 0 * 86400. _d 0
       atmCFC_timeOffset = UNSET_RL
       atmCFC_yNorthBnd  =  10. _d 0
       atmCFC_ySouthBnd  = -10. _d 0
C--   retired parameters
       CFC_windFile  = ' '
       CFC_atmospFile= ' '
       CFC_iceFile   = ' '
       CFC_forcingPeriod = UNSET_RL
       CFC_forcingCycle  = UNSET_RL

      WRITE(msgBuf,'(A)') ' CFC_READPARMS: opening data.cfc'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data.cfc', 'CFC_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    forcing filenames and parameters
      READ(UNIT=iUnit,NML=CFC_FORCING)

      WRITE(msgBuf,'(A)')
     &   ' CFC_READPARMS: finished reading data.cfc'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     I                    SQUEEZE_RIGHT, myThid )

C--   Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      IF ( atmCFC_timeOffset .EQ. UNSET_RL ) THEN
C-    Assume that cfcTime=0 corresponds to the beginning of the 1rst record
C     time-period. This is consistent with 1rst record value = time-averaged
C     atmos-CFC over time period: cfcTime= 0 to cfcTime= 1 x atmCFC_recSepTime
C-    To reproduce previous time interpolation (formerly in cfc12_forcing.F),
C     set default timeOffset according to PTRACERS_Iter0 :
        atmCFC_timeOffset = atmCFC_recSepTime
     &                     - deltaTClock*PTRACERS_Iter0
      ENDIF

      CFC_monFreq = 0. _d 0
      IF ( debugLevel.GE.debLevB ) CFC_monFreq = PTRACERS_monitorFreq

C     retired parameters
      nRetired = 0
      IF ( CFC_forcingPeriod .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: "CFC_forcingPeriod" ',
     &      'is no longer allowed in file "data.cfc"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: ',
     &      'use gchem_forcingPeriod in data.gchem instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( CFC_forcingCycle .NE. UNSET_RL ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: "CFC_forcingCycle" ',
     &      'is no longer allowed in file "data.cfc"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: ',
     &      'use gchem_forcingCycle in data.gchem instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( CFC_windFile .NE. ' ' ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: "CFC_windFile" ',
     &      'is no longer allowed in file "data.cfc"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: ',
     &      'use gchem_windFile in data.gchem instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( CFC_atmospFile .NE. ' ' ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: "CFC_atmospFile" ',
     &      'is no longer allowed in file "data.cfc"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: ',
     &      'use gchem_apresFile in data.gchem instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( CFC_iceFile .NE. ' ' ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: "CFC_iceFile" ',
     &      'is no longer allowed in file "data.cfc"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CFC_READPARMS: ',
     &      'use gchem_iceFile in data.gchem instead'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      IF ( nRetired .GT. 0 ) THEN
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R CFC_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_CFC */

      RETURN
      END
