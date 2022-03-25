#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MYPACKAGE_CHECK

C !INTERFACE:
      SUBROUTINE MYPACKAGE_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'MYPACKAGE_CHECK: #define MYPACKAGE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C--  Print out some kee parameters :
       CALL WRITE_0D_L( myPa_StaV_Cgrid, INDEX_NONE,
     &  'myPa_StaV_Cgrid =', ' /* state vector on C-grid */')
       CALL WRITE_0D_L( myPa_Tend_Cgrid, INDEX_NONE,
     &  'myPa_Tend_Cgrid =', ' /* vector tendency on C-grid */')
       CALL WRITE_0D_L( myPa_applyTendT, INDEX_NONE,
     &  'myPa_applyTendT =',
     &  ' /* apply MYPA tendency to Temperature Eq.*/')
       CALL WRITE_0D_L( myPa_applyTendS, INDEX_NONE,
     &  'myPa_applyTendS =',
     &  ' /* apply MYPA tendency to Salinity Eq.*/')
       CALL WRITE_0D_L( myPa_applyTendU, INDEX_NONE,
     &  'myPa_applyTendU =',
     &  ' /* apply MYPA tendency to U momentum Eq.*/')
       CALL WRITE_0D_L( myPa_applyTendV, INDEX_NONE,
     &  'myPa_applyTendV =',
     &  ' /* apply MYPA tendency to V momentum Eq.*/')
C-   add some other as example:
       CALL WRITE_0D_I( myPa_index1, INDEX_NONE, ' myPa_index1 =',
     &  ' /* user defined parameter */')
       CALL WRITE_0D_RL( myPa_param1, INDEX_NONE, ' myPa_param1 =',
     &  ' /* user defined parameter */')
       CALL WRITE_0D_C( myPa_string1,-1,INDEX_NONE,'myPa_string1 =',
     &  ' /* user defined parameter */')

C--  Check parameters:

C-     Put stops here if some flags are wrongly chosen.
C-     For example, require this package to use tempStepping
       IF ( .NOT.tempStepping .AND. myPa_applyTendT ) THEN
        WRITE(msgBuf,'(A)') 'An example requirement is:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'myPa_applyTendT needs tempStepping=.true.'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R MYPACKAGE_CHECK'
       ENDIF

      _END_MASTER(myThid)

      RETURN
      END
