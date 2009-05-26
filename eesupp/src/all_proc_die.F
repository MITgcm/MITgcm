C $Header: /u/gcmpack/MITgcm/eesupp/src/all_proc_die.F,v 1.1 2009/05/26 22:56:32 jmc Exp $
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
C     errCount     :: error counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
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
         CALL MPI_FINALIZE  ( mpiRC )
         IF ( mpiRC .NE. MPI_SUCCESS ) THEN
          WRITE(msgBuf,'(A,I5)')
     &     'S/R FIN_PROCS: MPI_FINALIZE return code', mpiRC
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */

      RETURN
      END
