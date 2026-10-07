#include "GCHEM_OPTIONS.h"

CBOP
C     !ROUTINE: CFC_CHECK
C     !INTERFACE:
      SUBROUTINE CFC_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CFC_CHECK
C     | o Print CFC parameter summary and
C     | o Check for consistent CFC pkg setting
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CFC.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_CFC
C     !LOCAL VARIABLES:
C     msgBuf    :: Informational/error message buffer
C     ioUnit    :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
c     INTEGER errCount

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

      ioUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') 'CFC_CHECK:   #define ALLOW_CFC'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C---  Print summary:

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// CFC pkg configuration summary'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_C( atmCFC_inpFile, -1, INDEX_NONE,
     & 'atmCFC_inpFile =', ' /* atmos CFC time-series input file */')
      CALL WRITE_0D_RL( atmCFC_recSepTime, INDEX_NONE,
     &  'atmCFC_recSepTime =', ' /* time record spacing (s) */')
      CALL WRITE_0D_RL( atmCFC_timeOffset, INDEX_NONE,
     &  'atmCFC_timeOffset =', ' /* time offset for atm CFC (s) */')
      CALL WRITE_0D_RL( atmCFC_ySouthBnd, INDEX_NONE,
     &  'atmCFC_ySouthBnd =', ' /* S Lat boundary for atm CFC (s) */')
      CALL WRITE_0D_RL( atmCFC_yNorthBnd, INDEX_NONE,
     &  'atmCFC_yNorthBnd =', ' /* N Lat boundary for atm CFC (s) */')
      CALL WRITE_0D_RL( CFC_monFreq, INDEX_NONE,
     &  'CFC_monFreq =', ' /* frequency for CFC monitor (s) */')

c     CALL WRITE_0D_L( fluidIsAir,   INDEX_NONE,
c    & 'fluidIsAir   =', '  /* fluid major constituent is Air */')

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of CFC pkg config summary'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C---  Check CFC pkg settings:
c     errCount = 0 ! nothing to check currently

c     IF ( errCount.GE.1 ) THEN
c       WRITE(msgBuf,'(A,I3,A)')
c    &       'CFC_CHECK: detected', errCount,' fatal error(s)'
c       CALL PRINT_ERROR( msgBuf, myThid )
c       CALL ALL_PROC_DIE( 0 )
c       STOP 'ABNORMAL END: S/R CFC_CHECK'
c     ENDIF

c     WRITE(msgBuf,'(A)') 'CFC_CHECK: done'
c     CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_CFC */

      RETURN
      END
