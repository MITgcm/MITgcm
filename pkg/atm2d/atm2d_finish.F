C $Header: /u/gcmpack/MITgcm/pkg/atm2d/atm2d_finish.F,v 1.3 2007/11/05 21:15:53 jscott Exp $
C $Name:  $

#include "ctrparam.h"
#ifdef OCEAN_3D
#  include "ATM2D_OPTIONS.h"
#endif
C
      SUBROUTINE ATM2D_FINISH( myTime, myIter, myThid)
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
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     === Local variables ===
#ifdef OCEAN_3D
      INTEGER rc
#endif


      CALL ATM2D_WRITE_PICKUP(.TRUE.,myTime, myIter, myThid)

#ifdef ATM2D_MPI_ON
      CALL MPI_BARRIER( MPI_COMM_WORLD, rc )
      CALL MPI_FINALIZE(rc)
#endif

      RETURN
      END
