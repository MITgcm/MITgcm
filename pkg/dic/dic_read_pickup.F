C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_read_pickup.F,v 1.4 2008/04/07 20:31:16 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

      SUBROUTINE DIC_READ_PICKUP(
     O                            pH_isLoaded,
     I                            myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"

C     == Routine arguments ==
C     myThid  :: my Thread Id number
      LOGICAL ph_isLoaded
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC
#ifdef DIC_BIOTIC

C     !FUNCTIONS
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn, dFileName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileExist, shareExist
      INTEGER fp, iL, i, ioUnit

      COMMON / LOCAL_DIC_PICKUP / shareExist
CEOP

      ph_isLoaded =.FALSE.
      ioUnit = errorMessageUnit

C--   pickup file name :
      IF (pickupSuff.EQ.' ') THEN
        WRITE(fn,'(A,I10.10)') 'pickup_dic.', myIter
      ELSE
        WRITE(fn,'(A,A10)') 'pickup_dic.', pickupSuff
      ENDIF
      fp = precFloat64

      _BARRIER
      _BEGIN_MASTER( myThid )

C--   First check if pickup file exist
      fileExist = .FALSE.
      iL = ILNBLNK(fn)
      IF ( .NOT.fileExist ) THEN
C-    look for file = {fn}
        WRITE(dFileName,'(A)') fn(1:iL)
        i = iL
        INQUIRE( FILE=dFileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist ) THEN
C-    look for file = {fn}'.data'
        WRITE(dFileName,'(2A)') fn(1:iL), '.data'
        i = iL + 5
        INQUIRE( FILE=dFileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist ) THEN
C-    look for file = {fn}'.001.001.data'
        WRITE(dFileName,'(2A)') fn(1:iL), '.001.001.data'
        i = iL + 5 + 8
        INQUIRE( FILE=dFileName, EXIST=fileExist )
      ENDIF
      shareExist = fileExist

      IF ( .NOT.fileExist ) THEN
        WRITE(msgBuf,'(4A)') 'WARNING >> DIC_READ_PICKUP: file: ',
     &          fn(1:iL), ' , .data , ', dFileName(1:i)
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &           'WARNING >> DIC_READ_PICKUP: Files DO not exist'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      IF ( shareExist ) THEN
C--   Read pickup file
        CALL READ_REC_3D_RL( fn, fp, 1, pH, 1, myIter, myThid )
        pH_isLoaded = .TRUE.

        _EXCH_XY_R8( pH, myThid )
      ELSE
        pH_isLoaded = .FALSE.
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          STOP 'ABNORMAL END: S/R DIC_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'will restart from approximated pH'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

#endif /*  DIC_BIOTIC  */
#endif /*  ALLOW_DIC  */

      RETURN
      END
