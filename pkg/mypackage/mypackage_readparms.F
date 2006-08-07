C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_readparms.F,v 1.2 2006/08/07 21:13:52 edhill Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_READPARMS

C !INTERFACE:
      SUBROUTINE MYPACKAGE_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize MYPACKAGE variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MYPACKAGE

      NAMELIST /MYPACKAGE_PARM01/
     &       MYPA_MNC,
     &       MYPA_index1, MYPA_index2,
     &       MYPA_param1, MYPA_param2,
     &       MYPA_string1, MYPA_string2,
     &       mypaStatScal1File, mypaStatScal2File,
     &       mypaStatUvelFile, mypaStatVvelFile,
     &       mypaSurf1File, mypaSurf2File

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   MYPACKAGE_READPARMS has been called so we know that
C     the package is active.
      MYPAisOn = .TRUE.

      _BEGIN_MASTER(myThid)

C--   Default values for MYPACKAGE
      MYPA_MNC            = useMNC
      MYPA_doSwitch1      = .FALSE.
      MYPA_doSwitch2      = .FALSE.
      MYPA_index1         = 0
      MYPA_index2         = 0
      MYPA_param1         = 0. _d 0
      MYPA_param2         = 0. _d 0
      MYPA_string1        = ' '
      MYPA_string2        = ' '
      mypaStatScal1File   = ' '
      mypaStatScal2File   = ' '
      mypaStatUvelFile    = ' '
      mypaStatVvelFile    = ' '
      mypaSurf1File   = ' '
      mypaSurf2File   = ' '
      
      WRITE(msgBuf,'(A)') 'MYPACKAGE_READPARMS: opening data.mypackage'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.mypackage', 'MYPACKAGE_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=MYPACKAGE_PARM01)
      WRITE(msgBuf,'(A)') 
     &    'MYPACKAGE_READPARMS: finished reading data.mypackage'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
      CLOSE(iUnit)

C     Make sure that we locally honor the global MNC on/off flag
      MYPA_MNC = MYPA_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      MYPA_MNC = .FALSE.
#endif
      MYPA_MDSIO = (.NOT. MYPA_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
