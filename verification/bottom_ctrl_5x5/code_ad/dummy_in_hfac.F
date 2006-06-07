#include "CPP_OPTIONS.h"

      subroutine dummy_in_hfac( myname, myIter, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE dummy_in_hfac                                |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
      INTEGER myIter
      CHARACTER*(*) myname

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      end

CBOP
C     !ROUTINE: addummy_in_hfac
C     !INTERFACE:
      subroutine addummy_in_hfac( myname, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE addummy_in_hfac                              |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE addummy_in_hfac                              |
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

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myIter
      CHARACTER*(1) myname

#ifdef ALLOW_AUTODIFF_MONITOR
C     !LOCAL VARIABLES:
c     == local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL mytime
      CHARACTER*(5) myfullname

c     == end of interface ==
CEOP

#ifdef ALLOW_DEPTH_CONTROL

      mytime = 0.

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      IF (
     &  DIFFERENT_MULTIPLE(dumpFreq,mytime,
     &                     mytime-deltaTClock)
     & ) THEN

       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        IF ( myname .eq. 'C' ) THEN
           myfullname = 'hFacC'
           CALL WRITE_FLD_XYZ_RL ( 'ADJhFacC.', suff, adhfacc, 
     &          myIter, myThid)
        ELSE IF ( myname .eq. 'W' ) THEN
           myfullname = 'hFacW'
           CALL WRITE_FLD_XYZ_RL ( 'ADJhFacW.', suff, adhfacw, 
     &          myIter, myThid)
        ELSE IF ( myname .eq. 'S' ) THEN
           myfullname = 'hFacS'
           CALL WRITE_FLD_XYZ_RL ( 'ADJhFacS.', suff, adhfacs, 
     &          myIter, myThid)
        ELSE
           write(*,*) 'addummy_in_hfac: no valid myname specified'
        END IF
C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_STATE'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out model state'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
         WRITE(msgBuf,'(A,I10)')  
     &    '// ad'//myfullname//' written, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
#endif /* ALLOW_DEPTH_CONTROL */
#endif ALLOW_AUTODIFF_MONITOR

      end
