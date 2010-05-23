C $Header: /u/gcmpack/MITgcm/eesupp/src/master_cpu_io.F,v 1.4 2010/05/23 01:35:51 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: MASTER_CPU_IO

C     !INTERFACE:
      LOGICAL FUNCTION MASTER_CPU_IO( myThid )

C     !DESCRIPTION:
C     return TRUE if master thread (case not using SingleCpuIO) and
C                 if master CPU & master thread (case using SingleCpuIO)

C     !USES:
      IMPLICIT NONE

C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C   myThid        :: my Thread Id number
C   MASTER_CPU_IO :: true if master thread
C                    and, if using SingleCpuIO, if master CPU
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      MASTER_CPU_IO = .FALSE.

      IF ( .NOT.useSingleCPUIO .OR. myProcId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        MASTER_CPU_IO = .TRUE.
        _END_MASTER( myThid )
      ENDIF

      RETURN
      END
