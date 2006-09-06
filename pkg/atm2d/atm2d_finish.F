#include "ctrparam.h"
#ifdef OCEAN_3D
#  include "ATM2D_OPTIONS.h"
#endif
C
      SUBROUTINE ATM2D_FINISH(myThid)
C     |==========================================================|
C     | Called from forward_step_atm2d the last time through the |
C     | main loop -- thus, and opportunity to shut down MPI, do  |
C     | cleanup/finish routines.                                 |
C     \==========================================================/
      IMPLICIT NONE

#  ifdef ATM2D_MPI_ON
#    include "mpif.h"
#  endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter - iteration counter for this thread
C     myTime - time counter for this thread
C     myThid - thread number for this instance of the routine.
      INTEGER myThid

C     === Local variables ===
#ifdef OCEAN_3D
      INTEGER rc
#endif

#ifdef ATM2D_MPI_ON
      CALL MPI_BARRIER( MPI_COMM_WORLD, rc )
      CALL MPI_FINALIZE(rc)
#endif

      RETURN
      END
