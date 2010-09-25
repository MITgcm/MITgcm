C $Header: /u/gcmpack/MITgcm/eesupp/src/all_proc_die.F,v 1.4 2010/09/25 23:09:54 mlosch Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: ALL_PROC_DIE

C     !INTERFACE:
      SUBROUTINE ALL_PROC_DIE( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ALL_PROC_DIE
C     | o when all process call this routine, die cleanly:
C     |   set Error-Flag and call MPI finalise
C     *==========================================================*
C     | used before a STOP:
C     | - Only implemented for MPI multi-proc.
C     | - if some Proc do not call this S/R, MPI will hang.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
C     msgBuf       :: I/O Buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_USE_MPI
C     mpiRC        :: Error code reporting variable used with MPI.
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Print message
      WRITE(msgBuf,'(A)') 'S/R ALL_PROC_DIE: ending the run'
      CALL PRINT_ERROR( msgBuf, myThid )

C--   Finishes
      eeEndError = .TRUE.
      fatalError = .TRUE.
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
C     better to avoid this call if multi-components set-up ; otherwise will
C     hang here since procs of other comp. are not calling MPI_finalize now.
       IF ( .NOT.( useCoupler
     &        .OR. useNEST_PARENT
     &        .OR. useNEST_CHILD )
     &    ) THEN
#ifdef ALLOW_OASIS
         IF ( useOASIS ) CALL OASIS_ABORT
#endif /* ALLOW_OASIS */        
         CALL MPI_FINALIZE  ( mpiRC )
         IF ( mpiRC .NE. MPI_SUCCESS ) THEN
          WRITE(msgBuf,'(A,I5)')
     &     'S/R FIN_PROCS: MPI_FINALIZE return code', mpiRC
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
       ENDIF
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */

      RETURN
      END
