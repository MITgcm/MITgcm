#include "OPPS_OPTIONS.h"

CBOP
C !ROUTINE: OPPS_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE OPPS_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize OPPS parameters, read in data.opps

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OPPS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_OPPS

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /OPPS_PARM01/
     &     MAX_ABE_ITERATIONS,
     &     OPPSdebugLevel,
     &     PlumeRadius,
     &     STABILITY_THRESHOLD,
     &     FRACTIONAL_AREA,
     &     MAX_FRACTIONAL_AREA,
     &     VERTICAL_VELOCITY,
     &     ENTRAINMENT_RATE,
     &     useGCMwVel
C     &     , OPPSwriteState, OPPSdumpFreq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useOPPS ) THEN
C-    pkg OPPS is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         OPPSisON = .FALSE.
C     print a (weak) warning if data.opps is found
         CALL PACKAGES_UNUSED_MSG( 'useOPPS', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      OPPSisON = .TRUE.

C Set defaults values for parameters in OPPS.h
      MAX_ABE_ITERATIONS   =  1
      OPPSdebugLevel       =  0
      PlumeRadius          = 100.   _d 0
      STABILITY_THRESHOLD  =  -1.   _d -4
      FRACTIONAL_AREA      =    .1  _d 0
      MAX_FRACTIONAL_AREA  =    .8  _d 0
      VERTICAL_VELOCITY    =    .03 _d 0
      ENTRAINMENT_RATE     =   -.05 _d 0
      useGCMwVel           = .FALSE.
C     OPPSdumpFreq         = dumpFreq
C     OPPSwriteState       = .FALSE.

C Open and read the data.opps file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' OPPS_READPARMS: opening data.opps'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.opps', 'OPPS_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=OPPS_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' OPPS_READPARMS: finished reading data.opps'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

C Now set-up any remaining parameters that result from the input parameters
      e2    =   2.*ENTRAINMENT_RATE
#endif /* ALLOW_OPPS */

      RETURN
      END
