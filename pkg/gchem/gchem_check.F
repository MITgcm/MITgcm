#include "GCHEM_OPTIONS.h"
#undef GCHEM_REPORT_UNUSED_PARAMS

CBOP
C     !ROUTINE: GCHEM_CHECK
C     !INTERFACE:
      SUBROUTINE GCHEM_CHECK( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GCHEM_CHECK
C     | o Validate basic package setup and inter-package
C     |   dependencies.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"
#ifdef ALLOW_CFC
# include "CFC_SIZE.h"
#endif
#ifdef ALLOW_SPOIL
# include "SPOIL_SIZE.h"
#endif
#include "GCHEM_SIZE.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: My Thread Id number
      INTEGER myThid

#ifdef ALLOW_GCHEM
C     !FUNCTIONS:
#ifdef ALLOW_CAL
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
      INTEGER nb_tendTr
      INTEGER ioUnit
#ifdef ALLOW_CAL
      INTEGER IL
#endif
#ifdef GCHEM_REPORT_UNUSED_PARAMS
      INTEGER k
      INTEGER gchem_int(5)
      _RL     gchem_rl(5)
      CHARACTER*(MAX_LEN_FNAM) gchem_fileName(5)
      CHARACTER*(10) varName
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(2A)') 'GCHEM_CHECK ',
     &                     ' --> Starts to check GCHEM set-up'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Check GCHEM Options and Parameters:

#ifndef GCHEM_ADD2TR_TENDENCY
      IF ( useCFC ) THEN
        WRITE(msgBuf,'(A)') 'GCHEM_ADD2TR_TENDENCY needed to use CFC'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef GCHEM_ADD2TR_TENDENCY */

#ifndef GCHEM_SEPARATE_FORCING
      IF ( useDIC ) THEN
        WRITE(msgBuf,'(A)') 'GCHEM_SEPARATE_FORCING needed to use DIC'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useBLING ) THEN
        WRITE(msgBuf,'(A)')
     &       'GCHEM_SEPARATE_FORCING needed to use BLING'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( useDARWIN ) THEN
        WRITE(msgBuf,'(A)')
     &       'GCHEM_SEPARATE_FORCING needed to use DARWIN'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef GCHEM_SEPARATE_FORCING */

C     GCHEM needs passive tracer package
      IF ( .NOT.usePTRACERS) THEN
        WRITE(msgBuf,'(A)') 'usePTRACERS has to be .TRUE. for GCHEM'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C     GCHEM cannot run BLING and DIC together
      IF ( useBLING .AND. useDIC ) THEN
        WRITE(msgBuf,'(A)')
     &       'useBLING and useDIC cannot both be .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C     GCHEM cannot run BLING and DARWIN together
      IF ( useBLING .AND. useDARWIN ) THEN
        WRITE(msgBuf,'(A)')
     &       'useBLING and useDARWIN cannot both be .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C     GCHEM cannot run DARWIN and DIC together
      IF ( useDARWIN .AND. useDIC ) THEN
        WRITE(msgBuf,'(A)')
     &       'useDARWIN and useDIC cannot both be .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      nb_tendTr = gchem_Tracer_num - gchem_sepFTr_num
      IF ( nb_tendTr.GT.GCHEM_tendTr_num ) THEN
        WRITE(msgBuf,'(2A,I5,A)') 'GCHEM_CHECK: ',
     &    'Number of tendency-tracers: nb_tendTr=', nb_tendTr
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A,I5,A)') 'GCHEM_CHECK: ',
     &    'Exceeds GCHEM_tendTr_num=', GCHEM_tendTr_num,
     &    ' (from GCHEM_SIZE.h)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( gchem_Tracer_num.GT.PTRACERS_numInUse ) THEN
        WRITE(msgBuf,'(2A,I5,A)') 'GCHEM_CHECK: ',
     &  'Number of GCHEM tracers: gchem_Tracer_num =', gchem_Tracer_num
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I5,A)') 'GCHEM_CHECK: ',
     &  ' exceeds number of pTr: PTRACERS_numInUse =',PTRACERS_numInUse
c    &  ' exceeds number of PTRACERS(_numInUse)=', PTRACERS_numInUse
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'GCHEM_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     some consistency checks with a soft warning
      ioUnit = errorMessageUnit
#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
       errCount = 0
       IF ( usingGregorianCalendar .AND.
     &      gchem_secondsPeryear .NE. 365.25 _d 0*86400. _d 0 ) THEN
        errCount = errCount + 1
       ELSEIF ( .NOT. usingGregorianCalendar .AND.
     &       gchem_secondsPerYear .NE. nDaysNoLeap*86400. _d 0 ) THEN
        errCount = errCount + 1
       ENDIF
       IF ( errCount .GT. 0 ) THEN
        IL = ILNBLNK(theCalendar)
        WRITE(msgBuf,'(A,F13.1,3A)')
     &       '** warning ** GCHEM_CHECK: gchem_secondsPeryear=',
     &       gchem_secondsPerYear,
     &       ' is not consistent with theCalendar= "',
     &       theCalendar(1:IL),'"'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &       '** warning ** GCHEM_CHECK:   Is this intentional ?'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF
#endif
      IF ( periodicExternalForcing ) THEN
       errCount = 0
       IF ( gchem_ForcingCycle .EQ. externForcingCycle .AND.
     &      gchem_secondsPeryear .NE. externForcingCycle ) THEN
        WRITE(msgBuf,'(2(A,F13.1))')
     &       '** warning ** GCHEM_CHECK: gchem_secondsPeryear=',
     &       gchem_secondsPerYear,
     &       ' is different from externForcingCycle=',
     &       externForcingCycle
        errCount = errCount + 1
       ELSEIF ( gchem_secondsPeryear .NE. gchem_ForcingCycle ) THEN
        WRITE(msgBuf,'(2(A,F13.1))')
     &       '** warning ** GCHEM_CHECK: gchem_secondsPeryear=',
     &       gchem_secondsPerYear,
     &       ' is different from gchem_ForcingCycle=',
     &       gchem_ForcingCycle
        errCount = errCount + 1
       ENDIF
       IF ( errCount .GT. 0 ) THEN
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &       '** warning ** GCHEM_CHECK:   Is this intentional ?'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check other GCHEM pkgs:

#ifdef ALLOW_CFC
C-    run checks specific to CFC
      IF ( useCFC ) THEN
        CALL CFC_CHECK( myThid )
      ENDIF
#endif

#ifdef ALLOW_DARWIN
C-    run checks specific to darwin
      IF ( useDARWIN ) THEN
        CALL DARWIN_CHECK( myThid )
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)
      ioUnit = standardMessageUnit

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// pkg/gchem configuration'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_L( useCFC, INDEX_NONE,
     &     'useCFC =', ' /* use pkg/cfc */')
      CALL WRITE_0D_L( useDIC, INDEX_NONE,
     &     'useDIC =', '  /* use pkg/dic */')
      CALL WRITE_0D_L( useBLING, INDEX_NONE,
     &     'useBLING =', ' /* use pkg/bling */')
      CALL WRITE_0D_L( useSPOIL, INDEX_NONE,
     &     'useSPOIL =', ' /* use pkg/spoil */')
      CALL WRITE_0D_L( useDARWIN, INDEX_NONE,
     &     'useDARWIN =', ' /* use pkg/darwin */')
      CALL WRITE_0D_I( nsubtime, INDEX_NONE, 'nsubtime =',
     &     ' /* number of sub-timesteps withing bgc-module */')
#ifdef GCHEM_REPORT_UNUSED_PARAMS
      gchem_int = (/gchem_int1,gchem_int2,
     &     gchem_int3,gchem_int4,gchem_int5/)
      gchem_rl = (/gchem_rl1,gchem_rl2,
     &     gchem_rl3,gchem_rl4,gchem_rl5/)
      gchem_fileName = (/fileName1,fileName2,
     &     fileName3,fileName4,fileName5/)
      DO k = 1, 5
       varName = ' '
       IF ( gchem_fileName(k) .NE. ' ' ) THEN
        WRITE(varName,'(A,I1)') 'fileName',k
        CALL WRITE_0D_C( gchem_fileName(k), -1, INDEX_NONE,
     &      varName(1:9)//' =', ' /* generic file name parameter */')
       ENDIF
       IF ( gchem_int(k) .NE. 0 ) THEN
        WRITE(varName,'(A,I1)') 'gchem_int',k
        CALL WRITE_0D_I( gchem_int(k), INDEX_NONE,
     &      varName(1:10)//' =', ' /* generic INTEGER parameter */')
       ENDIF
       IF ( gchem_rl(k) .NE. 0. _d 0 ) THEN
        WRITE(varName,'(A,I1)') 'gchem_rl',k
        CALL WRITE_0D_RL( gchem_rl(k), INDEX_NONE,
     &      varName(1:9)//' =', ' /* generic REAL*8 parameter */')
       ENDIF
      ENDDO
#endif
      CALL WRITE_0D_RL( gchem_ForcingPeriod, INDEX_NONE,
     & 'gchem_ForcingPeriod =', ' /* gchem forcing period (s) */')
      CALL WRITE_0D_RL( gchem_ForcingCycle, INDEX_NONE,
     & 'gchem_ForcingCycle =',
     & ' /* period of gchem forcing cycle (s) */')
      CALL WRITE_0D_RL( gchem_secondsPerYear, INDEX_NONE,
     & 'gchem_secondsPerYear =',
     & ' /* second in a year for light computations */')

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of pkg/gchem config. summary'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(2A)') 'GCHEM_CHECK ', ' <-- Ends Normally'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') ' '
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_GCHEM */

      RETURN
      END
