C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_readparms.F,v 1.1 2013/05/08 22:14:14 jmc Exp $
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

      NAMELIST /ATM_PHYS_PARM01/
c    &       myPa_MNC,
c    &       myPa_StaV_Cgrid, myPa_Tend_Cgrid,
     &       atmPhys_addTendT, atmPhys_addTendS,
     &       atmPhys_addTendU, atmPhys_addTendV,
     &       atmPhys_stepSST,
c    &       myPa_index1,     myPa_index2,  myPa_doSwitch2,
c    &       myPa_param1,     myPa_param2,
c    &       myPa_string1,    myPa_string2,
c    &       myPa_Scal1File,  myPa_Scal2File,
c    &       myPa_VelUFile,   myPa_VelVFile,
     &       atmPhys_SSTFile, atmPhys_QflxFile

      _BEGIN_MASTER(myThid)

C--   Default values for ATM_PHYS
c     myPa_MNC         = useMNC
c     myPa_StaV_Cgrid  = .TRUE.
c     myPa_Tend_Cgrid  = .TRUE.
      atmPhys_addTendT  = .TRUE.
      atmPhys_addTendS  = .TRUE.
      atmPhys_addTendU  = .TRUE.
      atmPhys_addTendV  = .TRUE.
C-    additional parameters:
      atmPhys_stepSST   = .FALSE.
c     myPa_doSwitch2   = .FALSE.
c     myPa_index1      = 0
c     myPa_index2      = 0
c     myPa_param1      = 0. _d 0
c     myPa_param2      = 0. _d 0
c     myPa_string1     = ' '
c     myPa_string2     = ' '
C-    file names for initial conditions:
c     myPa_Scal1File   = ' '
c     myPa_Scal2File   = ' '
c     myPa_VelUFile    = ' '
c     myPa_VelVFile    = ' '
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
