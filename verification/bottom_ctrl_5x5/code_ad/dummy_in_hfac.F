#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C--  File dummy_in_hfac.F:
C--   Contents
C--   o DUMMY_IN_HFAC
C--   o ADDUMMY_IN_HFAC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DUMMY_IN_HFAC
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_HFAC( myName, myIter, myThid )

C     !DESCRIPTION: \bv
C     Forward S/R is empty
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine.
      CHARACTER*(*) myName
      INTEGER myIter
      INTEGER myThid
CEOP

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADDUMMY_IN_HFAC
C     !INTERFACE:
      SUBROUTINE ADDUMMY_IN_HFAC( myName, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE addummy_in_hfac
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE addummy_in_hfac
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
#include "adcommon.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: Thread number for this instance of the routine.
      CHARACTER*(1) myName
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_AUTODIFF_MONITOR
#ifdef ALLOW_DEPTH_CONTROL
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !LOCAL VARIABLES:
C     suff            :: Hold suffix part of a filename
C     beginIOErrCount :: Begin IO error counts
C     endIOErrCount   :: End IO error counts
C     msgBuf          :: Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL myTime
      CHARACTER*(5) myFullName
CEOP

      myTime = 0.

      IF ( DIFFERENT_MULTIPLE( dumpFreq, myTime, myTime-deltaTClock )
     &   ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
c       writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        IF ( myName .EQ. 'C' ) THEN
          myFullName = 'hFacC'
          CALL WRITE_FLD_XYZ_RL( 'ADJhFacC.', suff, adhfacc,
     &                           myIter, myThid )
        ELSE IF ( myName .EQ. 'W' ) THEN
          myFullName = 'hFacW'
          CALL WRITE_FLD_XYZ_RL( 'ADJhFacW.', suff, adhfacw,
     &                           myIter, myThid )
        ELSE IF ( myName .EQ. 'S' ) THEN
          myFullName = 'hFacS'
          CALL WRITE_FLD_XYZ_RL( 'ADJhFacS.', suff, adhfacs,
     &                           myIter, myThid )
        ELSE
          WRITE(*,*) 'addummy_in_hfac: no valid myName specified'
        END IF
C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
          WRITE(msgBuf,'(A)')  'S/R WRITE_STATE'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')  'Error writing out model state'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
          CALL PRINT_ERROR( msgBuf, myThid )
        ELSE
          WRITE(msgBuf,'(A,I10)')
     &     '// ad'//myFullName//' written, timestep', myIter
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A)')  ' '
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_DEPTH_CONTROL */
#endif /* ALLOW_AUTODIFF_MONITOR */

      RETURN
      END
