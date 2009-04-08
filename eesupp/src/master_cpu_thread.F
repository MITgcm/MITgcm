C $Header: /u/gcmpack/MITgcm/eesupp/src/master_cpu_thread.F,v 1.2 2009/04/08 20:43:54 dimitri Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: MASTER_CPU_THREAD

C     !INTERFACE:
      LOGICAL FUNCTION MASTER_CPU_THREAD( myThid )

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
C   MASTER_CPU_THREAD :: true if master CPU & master thread
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      MASTER_CPU_THREAD = .FALSE.

#ifdef ALLOW_USE_MPI
      IF ( mpiMyId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        MASTER_CPU_THREAD = .TRUE.
        _END_MASTER( myThid )
      ENDIF
#else /* ALLOW_USE_MPI */
        _BEGIN_MASTER( myThid )
        MASTER_CPU_THREAD = .TRUE.
        _END_MASTER( myThid )
#endif /* ALLOW_USE_MPI */

      RETURN
      END
