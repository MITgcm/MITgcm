#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MYPACKAGE_READPARMS

C !INTERFACE:
      SUBROUTINE MYPACKAGE_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize MYPACKAGE variables and constants.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /MYPACKAGE_PARM01/
     &       myPa_MNC,
     &       myPa_StaV_Cgrid, myPa_Tend_Cgrid,
     &       myPa_applyTendT, myPa_applyTendS,
     &       myPa_applyTendU, myPa_applyTendV,
     &       myPa_doSwitch1,  myPa_doSwitch2,
     &       myPa_index1,     myPa_index2,
     &       myPa_param1,     myPa_param2,
     &       myPa_string1,    myPa_string2,
     &       myPa_Scal1File,  myPa_Scal2File,
     &       myPa_VelUFile,   myPa_VelVFile,
     &       myPa_Surf1File,  myPa_Surf2File

      IF ( .NOT.useMYPACKAGE ) THEN
C-    pkg MYPACKAGE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.mypackage is found
         CALL PACKAGES_UNUSED_MSG( 'useMYPACKAGE', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for MYPACKAGE
      myPa_MNC         = useMNC
      myPa_StaV_Cgrid  = .TRUE.
      myPa_Tend_Cgrid  = .TRUE.
      myPa_applyTendT  = .FALSE.
      myPa_applyTendS  = .FALSE.
      myPa_applyTendU  = .FALSE.
      myPa_applyTendV  = .FALSE.
C-    additional parameters:
      myPa_doSwitch1   = .FALSE.
      myPa_doSwitch2   = .FALSE.
      myPa_index1      = 0
      myPa_index2      = 0
      myPa_param1      = 0. _d 0
      myPa_param2      = 0. _d 0
      myPa_string1     = ' '
      myPa_string2     = ' '
C-    file names for initial conditions:
      myPa_Scal1File   = ' '
      myPa_Scal2File   = ' '
      myPa_VelUFile    = ' '
      myPa_VelVFile    = ' '
      myPa_Surf1File   = ' '
      myPa_Surf2File   = ' '

      WRITE(msgBuf,'(A)') 'MYPACKAGE_READPARMS: opening data.mypackage'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.mypackage', 'MYPACKAGE_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=MYPACKAGE_PARM01)
      WRITE(msgBuf,'(A)')
     &    'MYPACKAGE_READPARMS: finished reading data.mypackage'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C     Make sure that we locally honor the global MNC on/off flag
      myPa_MNC = myPa_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      myPa_MNC = .FALSE.
#endif
      myPa_MDSIO = (.NOT. myPa_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
