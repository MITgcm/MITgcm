C $Header: /u/gcmpack/MITgcm/eesupp/src/global_vec_sum.F,v 1.5 2012/05/11 20:07:51 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

C--  File global_vec_sum.F: Perform a global sum on a tiled-array of vectors.
C--   Contents
C--   o GLOBAL_VEC_SUM_R4
C--   o GLOBAL_VEC_SUM_R8
C--   o GLOBAL_VEC_SUM_INT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_R4(
     I     ndim, nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over tiles and then sum the result over all MPI
C     processes. Within a process only one thread (Master) does the sum.
C     The same thread also does the inter-process sum for example with MPI
C     and then writes the result into a shared location. All threads wait
C     until the sum is available.
C     Warning: Only works if argument array "sumPhi" is shared by all threads.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
c#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     sumPhi   :: input/output array
C     myThid   :: thread ID
      INTEGER ndim, nval, myThid
      Real*4 sumPhi(ndim,nSx,nSy)
CEOP

C     !LOCAL VARIABLES:
C     mpiRC    :: MPI return code
      INTEGER i, bi,bj
      Real*4  tmp1(nval), tmp2(nval)
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

      _BARRIER
      _BEGIN_MASTER( myThid )

C     Sum over all tiles
      DO i = 1,nval
        tmp1(i) = 0.
      ENDDO
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj )
          ENDDO
        ENDDO
      ENDDO

C     Invoke MPI if necessary
      IF ( usingMPI ) THEN

#ifdef ALLOW_USE_MPI
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_REAL,
     &       MPI_SUM,MPI_COMM_MODEL,mpiRC)
#endif /* ALLOW_USE_MPI */

C     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp2(i)
        ENDDO

      ELSE
C     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp1(i)
        ENDDO

      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_R8

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_R8(
     I     ndim, nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over tiles and then sum the result over all MPI
C     processes. Within a process only one thread (Master) does the sum.
C     The same thread also does the inter-process sum for example with MPI
C     and then writes the result into a shared location. All threads wait
C     until the sum is available.
C     Warning: Only works if argument array "sumPhi" is shared by all threads.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
c#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     sumPhi   :: input/output array
C     myThid   :: thread ID
      INTEGER ndim, nval, myThid
      Real*8 sumPhi(ndim,nSx,nSy)
CEOP

C     !LOCAL VARIABLES:
C     mpiRC    :: MPI return code
      INTEGER i, bi,bj
      Real*8  tmp1(nval), tmp2(nval)
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

      _BARRIER
      _BEGIN_MASTER( myThid )

C     Sum over all tiles
      DO i = 1,nval
        tmp1(i) = 0.
      ENDDO
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj )
          ENDDO
        ENDDO
      ENDDO

C     Invoke MPI if necessary
      IF ( usingMPI ) THEN

#ifdef ALLOW_USE_MPI
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_DOUBLE_PRECISION,
     &       MPI_SUM,MPI_COMM_MODEL,mpiRC)
#endif /* ALLOW_USE_MPI */

C     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp2(i)
        ENDDO

      ELSE
C     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp1(i)
        ENDDO

      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_INT

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_INT(
     I     ndim, nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over tiles and then sum the result over all MPI
C     processes. Within a process only one thread (Master) does the sum.
C     The same thread also does the inter-process sum for example with MPI
C     and then writes the result into a shared location. All threads wait
C     until the sum is available.
C     Warning: Only works if argument array "sumPhi" is shared by all threads.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
c#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     sumPhi   :: input/output array
C     myThid   :: thread ID
      INTEGER ndim, nval, myThid
      INTEGER sumPhi(ndim,nSx,nSy)
CEOP

C     !LOCAL VARIABLES:
C     mpiRC    :: MPI return code
      INTEGER i, bi,bj
      INTEGER tmp1(nval), tmp2(nval)
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

      _BARRIER
      _BEGIN_MASTER( myThid )

C     Sum over all tiles
      DO i = 1,nval
        tmp1(i) = 0
      ENDDO
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj )
          ENDDO
        ENDDO
      ENDDO

C     Invoke MPI if necessary
      IF ( usingMPI ) THEN

#ifdef  ALLOW_USE_MPI
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_INTEGER,
     &       MPI_SUM,MPI_COMM_MODEL,mpiRC)
#endif /*  ALLOW_USE_MPI */

C     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp2(i)
        ENDDO

      ELSE
C     Copy the results to the first location of the input array
        DO i = 1,nval
          sumPhi( i, 1,1 ) = tmp1(i)
        ENDDO

      ENDIF

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
