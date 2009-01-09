C $Header: /u/gcmpack/MITgcm/eesupp/src/master_cpu_io.F,v 1.2 2009/01/09 22:51:12 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: MASTER_CPU_IO

C     !INTERFACE:
      LOGICAL FUNCTION MASTER_CPU_IO( myThid )

C     !DESCRIPTION:
C     return TRUE if master thread, and if using SingleCPUIO,
C                 if master CPU & master thread

C     !USES:
      IMPLICIT NONE

C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_USE_MPI
# include "EESUPPORT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C   myThid        :: my Thread Id number
C   MASTER_CPU_IO :: true if master thread
C                    and, if using SingleCPUIOS, if master CPU
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      MASTER_CPU_IO = .FALSE.

#ifdef ALLOW_USE_MPI
      IF ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        MASTER_CPU_IO = .TRUE.
        _END_MASTER( myThid )
      ENDIF
#else /* ALLOW_USE_MPI */
        _BEGIN_MASTER( myThid )
        MASTER_CPU_IO = .TRUE.
        _END_MASTER( myThid )
#endif /* ALLOW_USE_MPI */

      RETURN
      END
