#include "CPP_OPTIONS.h"

      SUBROUTINE SCATTER_YZ( global, local, myThid )
C     Scatter elements of a y-z array from mpi process 0 to all processes.
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C     mythid - thread number for this instance of the routine.
C     global,local - working arrays used to transfer 2-D fields
      INTEGER mythid 
      Real*8  global(Ny)
      _RL     local(1-OLy:sNy+OLy,nSx,nSy)

      INTEGER jG, j, bi, bj
#ifdef ALLOW_USE_MPI

      _RL     temp(1-OLy:sNy+OLy,nSx,nSy)

      INTEGER istatus(MPI_STATUS_SIZE), ierr
      INTEGER isource, itag, npe
      INTEGER lbuff
#endif /* ALLOW_USE_MPI */

C--   Make everyone wait except for master thread.
      _BARRIER
      _BEGIN_MASTER( myThid )

#ifndef ALLOW_USE_MPI

      DO bj=1,nSy
         DO bi=1,nSx
               DO j=1,sNy
                  jG = myYGlobalLo-1+(bi-1)*sNy+j
                  local(j,bi,bj) = global(jG)
               ENDDO
         ENDDO
      ENDDO

#else /* ALLOW_USE_MPI */

      lbuff=(sNy+2*OLy)*nSx*nSy
      isource = 0
      itag = 0

      IF( mpiMyId .EQ. 0 ) THEN

C--   Process 0 fills-in its local data
         npe = 0
         DO bj=1,nSy
            DO bi=1,nSx
                  DO j=1,sNy
                     jG = mpi_myYGlobalLo(npe+1)-1+(bi-1)*sNy+j
                     local(j,bi,bj) = global(jG)
                  ENDDO
            ENDDO
         ENDDO

C--   Process 0 sends local arrays to all other processes
         DO npe = 1, numberOfProcs-1
            DO bj=1,nSy
               DO bi=1,nSx
                     DO j=1,sNy
                        jG = mpi_myYGlobalLo(npe+1)-1+(bi-1)*sNy+j
                        temp(j,bi,bj) = global(jG)
                     ENDDO
               ENDDO
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
CMM      _EXCH_XY_R8( local, myThid )

      RETURN
      END
