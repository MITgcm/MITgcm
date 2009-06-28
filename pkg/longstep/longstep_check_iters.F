C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_check_iters.F,v 1.2 2009/06/28 04:58:41 jmc Exp $
C $Name:  $

#include "LONGSTEP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LONGSTEP_CHECK_ITERS

C     !INTERFACE:
      SUBROUTINE LONGSTEP_CHECK_ITERS( myThid )

C     !DESCRIPTION:
C     Check that starttime and endtime are multiples of long time step.
C     Fix endtime if not, abort if starttime is not.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "LONGSTEP_PARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LONGSTEP

C     !LOCAL VARIABLES:
C     extraIter  :: extra iterations added to finish on a longstep
C     msgBuf     :: message buffer
      INTEGER extraIter
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (MOD(nIter0, LS_nIter) .NE. 0) THEN
C       Restarting from a pickup in the middle of a longstep will lead
C       to invalid vel,... averages, so we abort
        WRITE(msgBuf,'(2A,2I10)')
     &       ' LONGSTEP_CHECK_ITERS: nIter0 is not a multiple of',
     &       ' LS_niter', nIter0, LS_niter
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R LONGSTEP_CHECK_ITERS'
      ENDIF

      IF (MOD(nEndIter, LS_nIter) .NE. 0) THEN
C       Add as many time steps as necessary to bring endtime to the end
C       of a long time step, so that the pickup will be valid.
        WRITE(msgBuf,'(2A,2I10)')
     &       ' LONGSTEP_CHECK_ITERS: nEndIter is not a multiple of',
     &       ' LS_niter', nEndIter, LS_niter
        CALL PRINT_ERROR( msgBuf, myThid )
        extraIter = LS_nIter - MOD(nEndIter, LS_nIter)
        nEndIter = nEndIter + extraIter
        nTimeSteps = nEndIter - nIter0
        endTime = baseTime + deltaTClock*float(nEndIter)
        WRITE(msgBuf,'(A)')
     &       ' LONGSTEP_CHECK_ITERS: adjusting to'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,I10)')
     &       ' LONGSTEP_CHECK_ITERS:     nEndIter   =', nEndIter
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,I10)')
     &       ' LONGSTEP_CHECK_ITERS:     nTimeSteps =', nTimeSteps
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,1PE23.15)')
     &       ' LONGSTEP_CHECK_ITERS:     endTime    =', endTime
        CALL PRINT_ERROR( msgBuf, myThid )
C       log to standard output too
        WRITE(msgBuf,'(2A)') 'S/R LONGSTEP_CHECK_ITERS: ',
     &    ' adjusting endTime:'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)
        CALL WRITE_0D_RL( endTime, INDEX_NONE,'endTime =',
     &'   /* Integration ending time ( s ). */')
        CALL WRITE_0D_I( nTimeSteps, INDEX_NONE,'nTimeSteps =',
     &'   /* Number of timesteps */')
      ENDIF

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
