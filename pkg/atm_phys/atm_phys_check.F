C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_check.F,v 1.2 2013/06/28 21:25:25 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_CHECK

C !INTERFACE:
      SUBROUTINE ATM_PHYS_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATM_PHYS_PARAMS.h"

C     !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_ATM_PHYS
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'ATM_PHYS_CHECK: #define ALLOW_ATM_PHYS'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

C--  Print out some kee parameters :
       CALL WRITE_0D_L( atmPhys_addTendT, INDEX_NONE,
     &  'atmPhys_addTendT =',
     &  ' /* apply Atm-Phys tendency to Temperature Eq.*/')
       CALL WRITE_0D_L( atmPhys_addTendS, INDEX_NONE,
     &  'atmPhys_addTendS =',
     &  ' /* apply Atm-Phys tendency to Salinity Eq.*/')
       CALL WRITE_0D_L( atmPhys_addTendU, INDEX_NONE,
     &  'atmPhys_addTendU =',
     &  ' /* apply Atm-Phys tendency to U momentum Eq.*/')
       CALL WRITE_0D_L( atmPhys_addTendV, INDEX_NONE,
     &  'atmPhys_addTendV =',
     &  ' /* apply Atm-Phys tendency to V momentum Eq.*/')
       CALL WRITE_0D_L( atmPhys_stepSST, INDEX_NONE,
     &  'atmPhys_stepSST =', ' /* step forward SST field */')
       IF ( atmPhys_tauDampUV.LE.zeroRL ) THEN
        CALL WRITE_0D_RL( atmPhys_tauDampUV, INDEX_NONE,
     &  'atmPhys_tauDampUV =', ' /* U,V damping time-scale (s) */')
       ELSE
        CALL WRITE_1D_RL( atmPhys_dampUVfac, Nr, INDEX_K,
     &  'atmPhys_dampUVfac =', ' /* U,V danping coefficient (1/s) */')
       ENDIF

       CALL WRITE_0D_C( atmPhys_SSTFile, -1, INDEX_NONE,
     &  'atmPhys_SSTFile =',' /* SST input file */')
       CALL WRITE_0D_C( atmPhys_QflxFile, -1, INDEX_NONE,
     &  'atmPhys_QflxFile =',' /* Q-flux input file */')

C--  Check parameters:

C-     Put stops here if some flags are wrongly chosen.
C-     For example, require this package to use tempStepping
       IF ( .NOT.tempStepping .AND. atmPhys_addTendT ) THEN
        WRITE(msgBuf,'(A)') 'An example requirement is:'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)') 'atmPhys_addTendT needs tempStepping=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R ATM_PHYS_CHECK'
       ENDIF

       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      _END_MASTER(myThid)

#endif /* ALLOW_ATM_PHYS */

      RETURN
      END
