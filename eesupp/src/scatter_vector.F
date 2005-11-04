#include "CPP_OPTIONS.h"

      SUBROUTINE SCATTER_VECTOR( length, global, local, myThid )
C     Scatter elements of a 2-D array from mpi process 0 to all processes.
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C     mythid - thread number for this instance of the routine.
C     global,local - working arrays used to transfer 2-D fields
      INTEGER mythid 
      INTEGER length
      Real*8  global(length*nPx*nPy)
      _RL     local(length)

      INTEGER iG,jG,lG, l
#ifdef ALLOW_USE_MPI
      COMMON /GlobalLo/ mpi_myXGlobalLo, mpi_myYGlobalLo
      INTEGER mpi_myXGlobalLo(nPx*nPy)
      INTEGER mpi_myYGlobalLo(nPx*nPy)

      _RL     temp(length)
      INTEGER istatus(MPI_STATUS_SIZE), ierr
      INTEGER isource, itag, npe
      INTEGER lbuff
#endif /* ALLOW_USE_MPI */

C--   Make everyone wait except for master thread.
      _BARRIER
      _BEGIN_MASTER( myThid )

#ifndef ALLOW_USE_MPI

      DO l=1,length
         iG=1+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=1+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         lG= (jG-1)*nPx*length + (iG-1)*length + l
         local(l) = global(lG)
      ENDDO

#else /* ALLOW_USE_MPI */

      lbuff   = length
      isource = 0
      itag    = 0

      IF( mpiMyId .EQ. 0 ) THEN

C--   Process 0 fills-in its local data
         npe = 0
         iG=mpi_myXGlobalLo(npe+1)/sNx+1
         jG=mpi_myYGlobalLo(npe+1)/sNy+1
         DO l=1,length
            lG= (jG-1)*nPx*length + (iG-1)*length + l
            local(l) = global(lG)
         ENDDO

C--   Process 0 sends local arrays to all other processes
         DO npe = 1, numberOfProcs-1
            iG = mpi_myXGlobalLo(npe+1)/sNx+1
            jG = mpi_myYGlobalLo(npe+1)/sNy+1
            DO l=1,length
               lG= (jG-1)*nPx*length + (iG-1)*length + l
               temp(l) = global(lG)
            ENDDO
            CALL MPI_SEND (temp, lbuff, MPI_DOUBLE_PRECISION,
     &           npe, itag, MPI_COMM_MODEL, ierr)
         ENDDO

      ELSE

C--   All proceses except 0 receive local array from process 0
         CALL MPI_RECV (local, lbuff, MPI_DOUBLE_PRECISION,
     &        isource, itag, MPI_COMM_MODEL, istatus, ierr)

      ENDIF

#endif /* ALLOW_USE_MPI */

      _END_MASTER( myThid )
      _BARRIER

C--   Fill in edges.
cph      _EXCH_XY_R8( local, myThid )

      RETURN
      END
