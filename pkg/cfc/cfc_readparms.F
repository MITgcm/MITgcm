C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_readparms.F,v 1.3 2013/06/10 02:51:08 jmc Exp $
C $Name:  $

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
       CFC_windFile  = ' '
       CFC_atmospFile= ' '
       CFC_iceFile   = ' '
c default periodic forcing to same as for physics
       CFC_forcingPeriod = externForcingPeriod
       CFC_forcingCycle  = externForcingCycle

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
      CLOSE(iUnit)

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

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_CFC */

      RETURN
      END
