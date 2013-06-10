C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_check.F,v 1.1 2013/06/10 02:59:02 jmc Exp $
C $Name:  $

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
      INTEGER errCount

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

      CALL WRITE_0D_C( CFC_windFile, -1, INDEX_NONE,
     & 'CFC_windFile =', ' /* wind-speed input file */')
      CALL WRITE_0D_C( CFC_atmospFile, -1, INDEX_NONE,
     & 'CFC_atmospFile =', ' /* atmos surf pressure input file */')
      CALL WRITE_0D_C( CFC_iceFile, -1, INDEX_NONE,
     & 'CFC_iceFile =', ' /* seaice fraction input file */')

      CALL WRITE_0D_RL( CFC_forcingPeriod, INDEX_NONE,
     &  'CFC_forcingPeriod =',
     &  ' /* forcing period (s) of input file */')
      CALL WRITE_0D_RL( CFC_forcingCycle, INDEX_NONE,
     &  'CFC_forcingCycle =',
     &  ' /* period of Cycle for input file (s) */')

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of CFC pkg config summary'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

C---  Check CFC pkg settings:
      errCount = 0

      IF ( CFC_windFile .NE. ' ' .AND. useEXF ) THEN
        WRITE(msgBuf,'(A)')
     &    'CFC_CHECK: setting CFC_windFile conflicts with using EXF'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( CFC_iceFile .NE. ' ' .AND.
     &     ( useThSIce .OR. useSEAICE ) ) THEN
        WRITE(msgBuf,'(A)')
     &    'CFC_CHECK: setting CFC_iceFile conflicts with using seaice'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( CFC_forcingCycle.LE.zeroRL .AND.
     &     ( CFC_windFile   .NE. ' '
     &  .OR. CFC_atmospFile .NE. ' '
     &  .OR. CFC_iceFile    .NE. ' ' ) ) THEN
        WRITE(msgBuf,'(A)')
     &       'CFC_CHECK: CFC_forcingCycle=0 but code to load'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'CFC_CHECK:  non periodic forcing is missing'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'CFC_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R CFC_CHECK'
      ENDIF

      WRITE(msgBuf,'(A)') 'CFC_CHECK: done'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_CFC */

      RETURN
      END
