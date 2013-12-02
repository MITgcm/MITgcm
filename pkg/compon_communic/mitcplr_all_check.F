C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_all_check.F,v 1.1 2013/12/02 21:35:45 jmc Exp $
C $Name:  $

CBOP
C     !INTERFACE:
      SUBROUTINE MITCPLR_ALL_CHECK( errFlag, errMsg )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE MITCPLR_ALL_CHECK
C     | o Stop every Processes in World if flag is true
C     *==========================================================*
C     | Gather error-flag from all processes in World and
C     |  stop if one is in error. This assumes that every-one
C     | (all coupler procs and all components) call this routine
C     *==========================================================*

      IMPLICIT NONE

C Predefined constants/arrays
#include "CPLR_SIG.h"
C MPI variables
#include "mpif.h"

C !INPUT/OUTPUT PARAMETERS:
C     errFlag :: stop if this logical flag is true
C     errMsg  :: error message to print in case it stops
      LOGICAL errFlag
      CHARACTER*(*) errMsg

C !LOCAL VARIABLES:
C     msgBuf       :: I/O Buffer
C     errCount     :: error counter
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount, errLoc, mpiRC
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Collect error from all Procs
      errLoc = 0
      IF ( errFlag ) THEN
        errLoc = 1
      ENDIF
      CALL MPI_Allreduce( errLoc, errCount, 1, MPI_INTEGER, MPI_SUM,
     &                    MPI_COMM_WORLD, mpiRC )

      IF ( errCount.GE.1 ) THEN
C--   Print message
        IF ( errFlag .AND. errMsg.NE.' ' ) THEN
          WRITE(LogUnit,'(2A)') ' *** ERROR *** ', errMsg
        ENDIF
        WRITE(LogUnit,'(A,I8,A)')
     &   'FATAL ERROR for ', errCount, ' Proc(s) ==> Stop here'
C--   Finishes
c       CALL ALL_PROC_DIE( myThid )
        CALL MPI_FINALIZE( mpiRC )
        STOP 'ABNORMAL END: S/R MITCPLR_ALL_CHECK'
      ENDIF

      RETURN
      END
