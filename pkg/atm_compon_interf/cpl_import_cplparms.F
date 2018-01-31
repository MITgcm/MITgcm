#include "ATM_CPL_OPTIONS.h"

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
C     |   this version is specific to 1 component (atmos)
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMIDS.h"

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
      INTEGER parBuf(atmParSize)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BARRIER
      _BEGIN_MASTER( myThid )

C--   Initialise error counter:
      cplErrorCount = 0

C-    Initialise buffer
      DO i=1,atmParSize
        parBuf(i) = 0
      ENDDO

C-    Receive coupler configuration info.

C     o Import coupling exhange-field selectors (coupler params)
      CALL COMPRECV_I4VEC(
     I              atmCplParamsName, atmParSize,
     O              parBuf )

C-dBug:
      WRITE(standardMessageUnit,'(A,10I6)')
     & ' CPL_IMPORT_CPLPARMS: Recv parBuf=', (parBuf(i),i=1,atmParSize)
C-dBug:

C-    o Set options for coupling exchange
      IF ( atmParSize.EQ.6 ) THEN
        atm_cplSequential  = parBuf(1).EQ.1
        atm_cplExch_RunOff = parBuf(2).GE.2
        atm_cplExch1W_sIce = parBuf(3).GE.2
        atm_cplExch2W_sIce = parBuf(4).GE.2
        atm_cplExch_SaltPl = parBuf(5).GE.2
        atm_cplExch_DIC    = parBuf(6).GE.2
C-    save coupler parameter:
        cpl_exchange_RunOff = parBuf(2)
        cpl_exchange1W_sIce = parBuf(3)
        cpl_exchange2W_sIce = parBuf(4)
        cpl_exchange_SaltPl = parBuf(5)
        cpl_exchange_DIC    = parBuf(6)
      ELSE
C-    If not using an up-to-date ATMIDS.h with the wrong atmParSize
        cplErrorCount = cplErrorCount + 1
        WRITE(msgBuf,'(2A,I4,A)') 'CPL_IMPORT_CPLPARMS: ',
     &       'atmParSize=', atmParSize, ' is wrong (expect 6)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I4,A)') 'CPL_IMPORT_CPLPARMS: ',
     &       '==> set ErrorCount=', cplErrorCount, ' (Fatal)'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
