C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_readparms.F,v 1.3 2014/05/27 23:41:27 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_READPARMS

C !INTERFACE:
      SUBROUTINE ATM_PHYS_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize AtmPhys parameters and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATM_PHYS_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_ATM_PHYS

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER k

      NAMELIST /ATM_PHYS_PARM01/
     &       atmPhys_addTendT, atmPhys_addTendS,
     &       atmPhys_addTendU, atmPhys_addTendV,
     &       atmPhys_tauDampUV, atmPhys_dampUVfac,
     &       atmPhys_stepSST,
     &       atmPhys_SSTFile, atmPhys_QflxFile

      _BEGIN_MASTER(myThid)

      IF ( .NOT.useAtm_Phys ) THEN
C-    pkg ATM_PHYS is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.atm_phys is found
         CALL PACKAGES_UNUSED_MSG( 'useAtm_Phys', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C--   Default values for ATM_PHYS
      atmPhys_addTendT  = .TRUE.
      atmPhys_addTendS  = .TRUE.
      atmPhys_addTendU  = .TRUE.
      atmPhys_addTendV  = .TRUE.
      atmPhys_tauDampUV = 0. _d 0
      DO k=1,Nr
        atmPhys_dampUVfac(k) = 1. _d 0
      ENDDO
C-    additional parameters:
      atmPhys_stepSST   = .FALSE.
      atmPhys_SSTFile  = ' '
      atmPhys_QflxFile = ' '

      WRITE(msgBuf,'(A)') 'ATM_PHYS_READPARMS: opening data.atm_phys'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.atm_phys', 'ATM_PHYS_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=ATM_PHYS_PARM01)
      WRITE(msgBuf,'(A)')
     &    'ATM_PHYS_READPARMS: finished reading data.atm_phys'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
      CLOSE(iUnit)

      IF ( atmPhys_tauDampUV.LE.zeroRL ) THEN
       DO k=1,Nr
        atmPhys_dampUVfac(k) = 0. _d 0
       ENDDO
      ELSE
       DO k=1,Nr
        atmPhys_dampUVfac(k) = atmPhys_dampUVfac(k)/atmPhys_tauDampUV
       ENDDO
      ENDIF

C     Make sure that we locally honor the global MNC on/off flag
c     myPa_MNC = myPa_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
c     myPa_MNC = .FALSE.
#endif
c     myPa_MDSIO = (.NOT. myPa_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_ATM_PHYS */

      RETURN
      END
