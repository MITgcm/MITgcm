#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: MASTER_CPU_THREAD

C     !INTERFACE:
      SUBROUTINE OAD_S_MASTER_CPU_THREAD( myThid, isMasterCpuThread )

C     !DESCRIPTION:
C     return TRUE if master CPU & master thread

C     !USES:
      IMPLICIT NONE

C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_USE_MPI
# include "EESUPPORT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C   myThid            :: my Thread Id number
C   isMasterCpuThread :: true if master CPU & master thread
      INTEGER myThid
      LOGICAL isMasterCpuThread
CEOP

C     !LOCAL VARIABLES:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      isMasterCpuThread = .FALSE.

#ifdef ALLOW_USE_MPI
      IF ( mpiMyId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        isMasterCpuThread = .TRUE.
        _END_MASTER( myThid )
      ENDIF
#else /* ALLOW_USE_MPI */
        _BEGIN_MASTER( myThid )
        isMasterCpuThread = .TRUE.
        _END_MASTER( myThid )
#endif /* ALLOW_USE_MPI */

      RETURN
      END
