#include "OBSFIT_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: OBSFIT_READPARMS

C !INTERFACE:
      SUBROUTINE OBSFIT_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize OBSFIT variables and constants.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBSFIT_SIZE.h"
#include "OBSFIT.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER   iUnit, num_file, IL
      LOGICAL   exst
      CHARACTER*(128) fname

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      NAMELIST /obsfit_nml/
     &                   obsfitDir,
     &                   obsfitFiles,
     &                   mult_obsfit,
     &                   obsfit_facmod,
     &                   obsfitDoNcOutput,
     &                   obsfitDoGenGrid

      IF ( .NOT.useOBSFIT ) THEN
C     pkg OBSFIT is not used
        _BEGIN_MASTER(myThid)
C     Track pkg activation status:
C     print a (weak) warning if data.obsfit is found
         CALL PACKAGES_UNUSED_MSG( 'useOBSFIT', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C     Default values for OBSFIT
      obsfitDir=' '

      DO num_file=1,NFILESMAX_OBS
       obsfitFiles(num_file)      =    ' '
      ENDDO

      DO num_file=1,NFILESMAX_OBS
        mult_obsfit(num_file) = 1. _d 0
        obsfit_facmod(num_file) = 1. _d 0
      ENDDO

      obsfitDoNcOutput=.false.

      IF ( (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
       obsfitDoGenGrid=.true.
      ELSE
       obsfitDoGenGrid=.false.
      ENDIF
      
C     Read OBSFIT cost data file
      WRITE(msgBuf,'(A)') 'OBSFIT_READPARMS: opening data.obsfit'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.obsfit', 'OBSFIT_READPARMS',
     O                          iUnit,
     I                          myThid )

      READ(  iUnit, nml = obsfit_nml )

      WRITE(msgBuf,'(2A)') 'OBSFIT_READPARMS: ',
     &       'finished reading data.obsfit'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT, 1)

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      DO num_file=1,NFILESMAX_OBS

          IF ( obsfitFiles(num_file) .NE. ' ' ) THEN
           IL  = ILNBLNK( obsfitFiles(num_file) )
           fname = obsfitFiles(num_file)(1:IL)//'.nc'
           inquire( file=fname, exist=exst )
           IF (.NOT.exst) THEN
C     Warn user as we override obsfitFiles
            WRITE(msgBuf,'(3A)')
     &       '** WARNING ** OBSFIT_READPARMS: missing file: ',
     &       obsfitFiles(num_file)(1:IL),' gets switched off'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                    SQUEEZE_RIGHT, myThid)
C     Switch off this file (and potential cost function term)
             obsfitFiles(num_file) = ' '
            ENDIF
          ENDIF
        ENDDO

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
