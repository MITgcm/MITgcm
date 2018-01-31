#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_SEND_OCN_CPLPARMS

C !INTERFACE:
      SUBROUTINE CPL_SEND_OCN_CPLPARMS( msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_SEND_OCN_CPLPARMS
C     | o Routine for sending coupler-parameters
C     |   to ocean component
C     *==========================================================*
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "CPL_PARAMS.h"
#include "OCNIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit   :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
C     i        :: Loop counter
C     parBuf   :: local buffer to send coupler-params
      INTEGER i
      INTEGER parBuf(ocnParSize)
CEOP

C-    Initialise buffer
      DO i=1,ocnParSize
        parBuf(i) = 0
      ENDDO

C-    Send exchange field selectors (coupler config) to ocean component
      IF ( ocnParSize.EQ.6 ) THEN
        parBuf(1) = cpl_sequential
        parBuf(2) = cpl_exchange_RunOff
        parBuf(3) = cpl_exchange1W_sIce
        parBuf(4) = cpl_exchange2W_sIce
        parBuf(5) = cpl_exchange_SaltPl
        parBuf(6) = cpl_exchange_DIC
      ELSE
C-    If not using an up-to-date OCNIDS.h with the wrong ocnParSize
        cplErrorCount = cplErrorCount + 1
        WRITE(msgUnit,'(2A,I4,A)')
     &       ' *** ERROR *** CPL_SEND_OCN_CPLPARMS: ',
     &       'ocnParSize=', ocnParSize, ' is wrong (expect 6)'
        WRITE(msgUnit,'(2A,I4,A)')
     &       ' *** ERROR *** CPL_SEND_OCN_CPLPARMS: ',
     &       '==> set ErrorCount=', cplErrorCount, ' (Fatal)'
      ENDIF

      CALL COUPSEND_I4VEC(
     I              ocnCompName, ocnCplParamsName, ocnParSize,
     I              parBuf )

      RETURN
      END
