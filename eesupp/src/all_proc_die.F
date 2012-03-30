C $Header: /u/gcmpack/MITgcm/eesupp/src/all_proc_die.F,v 1.6 2012/03/30 18:14:35 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: ALL_PROC_DIE

C     !INTERFACE:
      SUBROUTINE ALL_PROC_DIE( myThArg )

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
C     myThArg      :: thread argument (= my Thread Id number
C                  :: or = 0 if called within single-thread section)
      INTEGER myThArg
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
C     msgBuf       :: I/O Buffer
C     myThid       :: my Thread Id number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER myThid
#ifdef ALLOW_USE_MPI
C     mpiRC        :: Error code reporting variable used with MPI.
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

      myThid = MAX(myThArg,1)
C--   Print message
      WRITE(msgBuf,'(A)') 'S/R ALL_PROC_DIE: ending the run'
      CALL PRINT_ERROR( msgBuf, myThid )

      _BEGIN_MASTER(myThid)

C--   Finishes
      eeEndError = .TRUE.
      fatalError = .TRUE.
#ifdef ALLOW_USE_MPI
      IF ( usingMPI ) THEN
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
      ENDIF
#endif /* ALLOW_USE_MPI */

C-    Some systems do not always flush the IO buffer to disk.
C     To fix this, can either close these files (implies not to write
C     anything after) or flush the io-unit.
C     Note: function "flush" is not standard fortran and might not
C     be available (genmake2 should check for this)
c       CLOSE( errorMessageUnit )
c       CALL FLUSH( errorMessageUnit )
c     IF ( usingMPI ) THEN
c       CLOSE( standardMessageUnit )
c       CALL FLUSH( standardMessageUnit )
c     ENDIF

      _END_MASTER(myThid)

      IF ( myThArg.GE.1 ) THEN
        _BARRIER
      ENDIF

      RETURN
      END
