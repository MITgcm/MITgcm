#include "GCHEM_OPTIONS.h"

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

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: My Thread Id number
      INTEGER myThid

#ifdef ALLOW_GCHEM
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
      INTEGER nb_tendTr
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

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(2A)') 'GCHEM_CHECK ',
     &                     ' <-- Ends Normally'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_GCHEM */

      RETURN
      END
