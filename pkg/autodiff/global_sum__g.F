C--   File global_g_sum.F: Routines that perform tangent linear of
C                          global sum on an array of thread values.
C      Contents
C      o global_g_sum_r4
C      o global_g_sum_r8
#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE GLOBAL_g_SUM_R4( 
     I                            myThid,
     U                            g_sumPhi
     &                          )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_g_SUM_R4                               |
C     | o Handle sum for real*4 data.                            |
C     |==========================================================|
C     \==========================================================/

C     == Global data ==
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     == Routine arguments ==
C     sumPhi - Result of sum.
C     myThid - My thread id.
      Real*4 g_sumPhi
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     g_phi  - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      Real*4 g_phi(lShare4,MAX_NO_THREADS)
      INTEGER I
      Real*4  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      STOP 'ROUTINE NEEDS TO BE WRITTEN'

      _END_MASTER( myThid )
C--
      _BARRIER
C
      RETURN
      END


#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE GLOBAL_g_SUM_R8( 
     I                            myThid,
     U                            g_sumPhi
     &                          )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_g_SUM_R4                               |
C     | o Handle sum for real*8 data.                            |
C     |==========================================================|
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     == Routine arguments ==
C     sumPhi - Result of sum.
C     myThid - My thread id.
      Real*8 g_sumPhi
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     g_phi  - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      Real*8 g_phi(lShare8,MAX_NO_THREADS)
      INTEGER I
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      STOP 'ROUTINE NEEDS TO BE WRITTEN'

      _END_MASTER( myThid )
C--
      _BARRIER
C
      RETURN
      END
