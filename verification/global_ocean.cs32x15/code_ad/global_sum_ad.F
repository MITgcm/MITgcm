C--   File global_adsum.F: Routines that perform adjoint of
C                          global sum on an array of thread values.
C      Contents
C      o global_adsum_r4
C      o global_adsum_r8
#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE GLOBAL_ADSUM_R4( 
     U                            adsumPhi,
     I                            myThid
     &                          )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_ADSUM_R4                               |
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
      Real*4 adsumPhi
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     adphi  - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      Real*4 adphi(lShare4,MAX_NO_THREADS)
      INTEGER I
      Real*4  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      tmp = adsumPhi

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
         CALL MPI_Bcast(tmp, 1, MPI_REAL, myThid
     &        , MPI_COMM_WORLD, mpiRC
     &        ) 
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

      DO I=1,nThreads
         adphi(1,I) = tmp
      ENDDO

      _END_MASTER( myThid )
C--
      _BARRIER
C
C--   every thread takes its adjoint sum
      adsumPhi = adphi(1,myThid)

      RETURN
      END


#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE GLOBAL_ADSUM_R8( 
     U                            adsumPhi,
     I                            myThid
     &                          )
C     /==========================================================\
C     | SUBROUTINE GLOBAL_ADSUM_R4                               |
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
      Real*8 adsumPhi
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     adphi  - Array to be summed.
C     I      - Loop counters
C     mpiRC  - MPI return code
      Real*8 adphi(lShare8,MAX_NO_THREADS)
      INTEGER I
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      tmp = adsumPhi

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
         CALL MPI_Bcast(tmp, 1, MPI_DOUBLE_PRECISION, myThid
     &        , MPI_COMM_WORLD, mpiRC
     &        ) 
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

      DO I=1,nThreads
         adphi(1,I) = tmp
      ENDDO

      _END_MASTER( myThid )
C--
      _BARRIER
C
C--   every thread takes its adjoint sum
      adsumPhi = adphi(1,myThid)

      RETURN
      END
