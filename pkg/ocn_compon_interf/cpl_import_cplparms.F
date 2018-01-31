#include "OCN_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_IMPORT_CPLPARMS

C !INTERFACE:
      SUBROUTINE CPL_IMPORT_CPLPARMS( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_IMPORT_CPLPARMS
C     | o Routine for importing coupling parameters
C     |   from/to the coupler layer
C     *==========================================================*
C     |   this version is specific to 1 component (ocean)
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid   :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

C !LOCAL VARIABLES:
C     == Local variables ==
C     i        :: Loop counter
C     parBuf   :: local buffer to receive coupler-params
C     msgBuf   :: Informational/error message buffer
      INTEGER i
      INTEGER parBuf(ocnParSize)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BARRIER
      _BEGIN_MASTER( myThid )

C--   Initialise error counter:
      cplErrorCount = 0

C-    Initialise buffer
      DO i=1,ocnParSize
        parBuf(i) = 0
      ENDDO

C-    Receive coupler configuration info.

C     o Import coupling exchange-field selectors (coupler params)
      CALL COMPRECV_I4VEC(
     I              ocnCplParamsName, ocnParSize,
     O              parBuf )

C-dBug:
      WRITE(standardMessageUnit,'(A,10I6)')
     & ' CPL_IMPORT_CPLPARMS: Recv parBuf=', (parBuf(i),i=1,ocnParSize)
C-dBug:

C-    o Set options for coupling exchange
      IF ( ocnParSize.EQ.6 ) THEN
        ocn_cplSequential  = parBuf(1).EQ.1
        ocn_cplExch_RunOff = MOD(parBuf(2),2).EQ.1
        ocn_cplExch1W_sIce = MOD(parBuf(3),2).EQ.1
        ocn_cplExch2W_sIce = MOD(parBuf(4),2).EQ.1
        ocn_cplExch_SaltPl = MOD(parBuf(5),2).EQ.1
        ocn_cplExch_DIC    = MOD(parBuf(6),2).EQ.1
C-    save coupler parameter:
        cpl_exchange_RunOff = parBuf(2)
        cpl_exchange1W_sIce = parBuf(3)
        cpl_exchange2W_sIce = parBuf(4)
        cpl_exchange_SaltPl = parBuf(5)
        cpl_exchange_DIC    = parBuf(6)
      ELSE
C-    If not using an up-to-date OCNIDS.h with the wrong ocnParSize
        cplErrorCount = cplErrorCount + 1
        WRITE(msgBuf,'(2A,I4,A)') 'CPL_IMPORT_CPLPARMS: ',
     &       'ocnParSize=', ocnParSize, ' is wrong (expect 6)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I4,A)') 'CPL_IMPORT_CPLPARMS: ',
     &       '==> set ErrorCount=', cplErrorCount, ' (Fatal)'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
