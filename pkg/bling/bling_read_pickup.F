#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_READ_PICKUP(
     O           pH_isLoaded,
     I           myIter, myThid )

C     ==========================================================
C     | subroutine bling_read_pickup
C     | o Read BLING arrays from a pickup file
C     ==========================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BLING_VARS.h"

C     == Routine arguments ==
C     myThid  :: my Thread Id number
      LOGICAL pH_isLoaded
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_BLING

C     !FUNCTIONS

C     !LOCAL VARIABLES:
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn, filNam
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL useCurrentDir, fileExist
      INTEGER fp, ioUnit
CEOP

      pH_isLoaded =.FALSE.
      ioUnit = errorMessageUnit

C--   pickup file name :
      IF (pickupSuff.EQ.' ') THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_bling.', myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
          WRITE(fn,'(A,A)') 'pickup_bling.', myIter
        ENDIF
      ELSE
        WRITE(fn,'(A,A10)') 'pickup_bling.', pickupSuff
      ENDIF
      fp = precFloat64

C--   First check if pickup file exist
#ifdef ALLOW_MDSIO
      useCurrentDir = .FALSE.
      CALL MDS_CHECK4FILE(
     I                     fn, '.data', 'BLING_READ_PICKUP',
     O                     filNam, fileExist,
     I                     useCurrentDir, myThid )
#else
      STOP 'ABNORMAL END: S/R BLING_READ_PICKUP: Needs MDSIO pkg'
#endif

      IF ( fileExist ) THEN
C--   Read pickup file
        CALL READ_REC_3D_RL( fn, fp, Nr, pH, 1, myIter, myThid )
        pH_isLoaded = .TRUE.

        CALL READ_REC_3D_RL( fn, fp, Nr, irr_mem, 2, myIter, myThid )
        CALL READ_REC_3D_RL( fn, fp, Nr, chl, 3, myIter, myThid )
        CALL READ_REC_3D_RL( fn, fp, Nr, phyto_sm, 4, myIter, myThid )
        CALL READ_REC_3D_RL( fn, fp, Nr, phyto_lg, 5, myIter, myThid )
#ifndef USE_BLING_V1
        CALL READ_REC_3D_RL( fn, fp, Nr, phyto_diaz, 6, myIter, myThid )
#endif

        _EXCH_XYZ_RL( pH, myThid )
        _EXCH_XYZ_RL( irr_mem, myThid )
        _EXCH_XYZ_RL( chl, myThid )
        _EXCH_XYZ_RL( phyto_sm, myThid )
        _EXCH_XYZ_RL( phyto_lg, myThid )
#ifndef USE_BLING_V1
        _EXCH_XYZ_RL( phyto_diaz, myThid )
#endif

      ELSE
        pH_isLoaded = .FALSE.
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'BLING_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          STOP 'ABNORMAL END: S/R BLING_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(2A)') 'WARNING >> BLING_READ_PICKUP: ',
     &      'will restart from approximated pH'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

#endif /*  ALLOW_BLING  */

      RETURN
      END
