C $Header: /u/gcmpack/MITgcm/eesupp/src/global_vec_sum.F,v 1.4 2006/08/12 03:10:26 edhill Exp $
C $Name:  $

C     Perform a global sum on an array of threaded vectors.
C     
C     Contents
C     o global_sum_r4
C     o global_sum_r8

#include "CPP_EEOPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_VEC_SUM_R4

C     !INTERFACE:
      SUBROUTINE GLOBAL_VEC_SUM_R4( 
     I     ndim, nval,
     U     sumPhi,
     I     myThid )

C     !DESCRIPTION:
C     Sum the vector over threads and then sum the result over all MPI
C     processes.  Within a process only one thread does the sum, each
C     thread is assumed to have already summed its local data.  The same
C     thread also does the inter-process sum for example with MPI and
C     then writes the result into a shared location. All threads wait
C     until the sum is available.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT PARAMETERS:
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

C     Empty the temp arrays
      DO i = 1,nval
        tmp1(i) = 0. _d 0
        tmp2(i) = 0. _d 0
      ENDDO

      _BEGIN_MASTER( myThid )

C     Sum over all threads
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj ) 
          ENDDO
        ENDDO
      ENDDO

C     Copy to the first temp array to the second temp array to handle
C     the case where MPI is not used
      DO i = 1,nval
        tmp2(i) = tmp1(i)
      ENDDO

C     Invoke MPI if necessary
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_REAL,
     &       MPI_SUM,MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
      
C     Copy the results to the first location of the input array
      DO i = 1,nval
        sumPhi( i, 1,1 ) = tmp2(i)
      ENDDO

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
C     Sum the vector over threads and then sum the result over all MPI
C     processes.  Within a process only one thread does the sum, each
C     thread is assumed to have already summed its local data.  The same
C     thread also does the inter-process sum for example with MPI and
C     then writes the result into a shared location. All threads wait
C     until the sum is avaiailable.

C     !USES:
      IMPLICIT NONE 
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT PARAMETERS:
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

C     Empty the temp arrays
      DO i = 1,nval
        tmp1(i) = 0. _d 0
        tmp2(i) = 0. _d 0
      ENDDO

      _BEGIN_MASTER( myThid )

C     Sum over all threads
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj ) 
          ENDDO
        ENDDO
      ENDDO

C     Copy to the first temp array to the second temp array to handle
C     the case where MPI is not used
      DO i = 1,nval
        tmp2(i) = tmp1(i)
      ENDDO

C     Invoke MPI if necessary
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_DOUBLE_PRECISION,
     &       MPI_SUM,MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
      
C     Copy the results to the first location of the input array
      DO i = 1,nval
        sumPhi( i, 1,1 ) = tmp2(i)
      ENDDO

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
C     Sum the vector over threads and then sum the result over all MPI
C     processes.  Within a process only one thread does the sum, each
C     thread is assumed to have already summed its local data.  The same
C     thread also does the inter-process sum for example with MPI and
C     then writes the result into a shared location. All threads wait
C     until the sum is avaiailable.

C     !USES:
      IMPLICIT NONE 
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT PARAMETERS:
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


C     Empty the temp arrays
      DO i = 1,nval
        tmp1(i) = 0. _d 0
        tmp2(i) = 0. _d 0
      ENDDO

      _BEGIN_MASTER( myThid )

C     Sum over all threads
      DO bj = 1,nSy
        DO bi = 1,nSx
          DO i = 1,nval
            tmp1(i) = tmp1(i) + sumPhi( i, bi,bj ) 
          ENDDO
        ENDDO
      ENDDO

C     Copy to the first temp array to the second temp array to handle
C     the case where MPI is not used
      DO i = 1,nval
        tmp2(i) = tmp1(i)
      ENDDO

C     Invoke MPI if necessary
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
        CALL MPI_Allreduce(tmp1,tmp2,nval,MPI_INTEGER,
     &       MPI_SUM,MPI_COMM_MODEL,mpiRC)
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */
      
C     Copy the results to the first location of the input array
      DO i = 1,nval
        sumPhi( i, 1,1 ) = tmp2(i)
      ENDDO

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
