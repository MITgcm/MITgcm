C $Header: /u/gcmpack/MITgcm/pkg/ebm/ebm_readparms.F,v 1.1 2004/05/14 21:10:34 heimbach Exp $
C $Name:  $
#include "EBM_OPTIONS.h"

      SUBROUTINE EBM_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE EBM_READPARMS                                 |
C     | o Routine to read in file data.ebm                       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EBM
#include "EBM.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instance
      INTEGER myThid

#ifdef ALLOW_EBM

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   Namelist
      NAMELIST /EBM_PARM01/
     & tauThetaZonRelax, lambdaThetaZonRelax,
     & scale_runoff,
     & RunoffFile

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' ebm_readparms: opening data.ebm'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                     'data.ebm', 'ebm_readparms',
     O                     iUnit, myThid )

C--   set default 
      tauThetaZonRelax = 0.
      lambdaThetaZonRelax = 0.
      scale_runoff = 1.
      RunoffFile = ' '

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.ebm".
      READ(UNIT=iUnit,NML=EBM_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R ebm_readparms'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.ebm"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist EBM_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R ebm_readparms'
      ENDIF

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' ebm_readparms: finished reading data.ebm'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      IF ( tauThetaZonRelax .NE. 0. _d 0 ) THEN
       lambdaThetaZonRelax = 1./tauThetaZonRelax
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_EBM */

      return
      end
