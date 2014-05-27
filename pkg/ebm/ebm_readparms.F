C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_readparms.F,v 1.4 2014/05/27 23:56:23 jmc Exp $
C $Name:  $
#include "EBM_OPTIONS.h"

CBOP 0
C !ROUTINE: EBM_READPARMS

C !INTERFACE:
      SUBROUTINE EBM_READPARMS( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EBM_READPARMS
C     | o Routine to read in parameters from file data.ebm
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EBM
#include "EBM.h"
#endif

C     !INPUT PARAMETERS:
C     myThid     :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     errIO      :: IO error flag
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   Namelist
      NAMELIST /EBM_PARM01/
     & tauThetaZonRelax,
     & scale_runoff,
     & RunoffFile

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useEBM ) THEN
C-    PKG ebm is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ebm is found
         CALL PACKAGES_UNUSED_MSG( 'useEBM', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' EBM_READPARMS: opening data.ebm'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.ebm', 'EBM_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default
      tauThetaZonRelax = 0.
      scale_runoff = 1.
      RunoffFile = ' '

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.ebm".
      READ(UNIT=iUnit,NML=EBM_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)') 'S/R EBM_READPARMS'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'parameter file "data.ebm"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'Problem in namelist EBM_PARM01'
       CALL PRINT_ERROR( msgBuf, myThid )
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R EBM_READPARMS'
      ENDIF
C     Close the open data file
      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' EBM_READPARMS: finished reading data.ebm'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Set other parameters
      IF ( tauThetaZonRelax .NE. 0. _d 0 ) THEN
        IF ( tauThetaClimRelax.NE.0. _d 0 ) THEN
          WRITE(msgBuf,'(2A)') 'EBM_READPARMS: ',
     &     'tauThetaClimRelax > 0 AND tauThetaZonRelax > 0'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)') 'EBM_READPARMS: ',
     &     'cannot relax Temp. to 2-d map AND ZonAver SST'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R EBM_READPARMS'
        ELSE
          WRITE(msgBuf,'(2A)') ' EBM_READPARMS: ',
     &     'set tauThetaClimRelax to tauThetaZonRelax value'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          tauThetaClimRelax = tauThetaZonRelax
        ENDIF
      ENDIF
C-   Set latitude boundaries used in EBM
      latBnd(1) =  0. _d 0
      latBnd(2) = 35. _d 0
      latBnd(3) = 85. _d 0

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_EBM */

      RETURN
      END
