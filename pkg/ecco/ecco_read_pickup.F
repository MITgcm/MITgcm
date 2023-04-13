#include "ECCO_OPTIONS.h"

CBOP
C     !ROUTINE: ECCO_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE ECCO_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE ECCO_READ_PICKUP
C     | o read ecco pickups
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PSBAR_STERIC
C !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fn, fntmp
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER prec, IL, ioUnit
      LOGICAL exst
      _RL tmparr(2), dummyRS(1)
CEOP

C--   Suffix for pickup files
      IF (pickupSuff.EQ.' ') THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
        ENDIF
      ELSE
        WRITE(suff,'(A10)') pickupSuff
      ENDIF

      _BEGIN_MASTER(myThid)

      WRITE(fn,'(A,A10)') 'pickup_ecco.', suff
c#ifdef ALLOW_MDSIO
c       useCurrentDir = .FALSE.
c       CALL MDS_CHECK4FILE(
c    I                       fn, '.data', 'ECCO_READ_PICKUP',
c    O                       filNam, fileExist,
c    I                       useCurrentDir, myThid )
c#endif
C-    Check first for global file with simple name (ie. fn)
      INQUIRE( file=fn, exist=exst )
      IF ( .NOT.exst ) THEN
C-    Check for global file with ".data" suffix
        IL  = ILNBLNK( fn )
        WRITE(fntmp,'(2A)') fn(1:IL),'.data'
        INQUIRE( file=fntmp, exist=exst )
      ENDIF

      IF (exst) THEN
        ioUnit = 0
        prec = precFloat64
#ifdef ALLOW_MDSIO
        CALL MDS_READVEC_LOC( fn, prec, ioUnit, 'RL',
     &                        2, tmparr, dummyRS, 0, 0, 1, myThid )
#else
        STOP 'ABNORMAL END: S/R ECCO_READ_PICKUP: Needs MDSIO pkg'
#endif
        VOLsumGlob_0 = tmparr(1)
        RHOsumGlob_0 = tmparr(2)
      ELSE
        WRITE(msgBuf,'(2A)') 'ECCO_READ_PICKUP: ',
     &        fn(1:IL)//' and '//fntmp(1:iL+5)//' not provided.'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A,I10)') 'ECCO_READ_PICKUP: ',
     &   'sterGloH is referenced to its value at time step:', nIter0
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

      _END_MASTER(myThid)

#endif /*  ALLOW_PSBAR_STERIC  */

      RETURN
      END
