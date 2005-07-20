C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/gather_vector.F,v 1.1 2005/07/20 20:01:46 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE GATHER_VECTOR( lprint, length, global, local, myThid )
C     Gather elements of a vector from all mpi processes to process 0.
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C     mythid - thread number for this instance of the routine.
C     global,local - working arrays used to transfer 2-D fields
      logical lprint
      INTEGER mythid 
      INTEGER length
      Real*8  global(length*nPx*nPy)
      _RL     local(length)

      INTEGER iG,jG,lG, i, j, l
#ifdef ALLOW_USE_MPI
      COMMON /GlobalLo/ mpi_myXGlobalLo, mpi_myYGlobalLo
      INTEGER mpi_myXGlobalLo(nPx*nPy)
      INTEGER mpi_myYGlobalLo(nPx*nPy)

      _RL     temp(length)
      INTEGER istatus(MPI_STATUS_SIZE), ierr
      INTEGER lbuff, idest, itag, npe, ready_to_receive
#endif /* ALLOW_USE_MPI */

C--   Make everyone wait except for master thread.
      _BARRIER
      _BEGIN_MASTER( myThid )

#ifndef ALLOW_USE_MPI

      DO l=1,length
         iG=1+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=1+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         lG= (jG-1)*nPx*length + (iG-1)*length + l
         global(lG) = local(l)
      ENDDO

#else /* ALLOW_USE_MPI */

      lbuff = length
      idest = 0
      itag  = 0
      ready_to_receive = 0

      IF( mpiMyId .EQ. 0 ) THEN

C--   Process 0 fills-in its local data
         npe = 0
         iG=mpi_myXGlobalLo(npe+1)/sNx+1
         jG=mpi_myYGlobalLo(npe+1)/sNy+1
         DO l=1,length
            lG= (jG-1)*nPx*length + (iG-1)*length + l
            global(lG) = local(l)
         ENDDO

C--   Process 0 polls and receives data from each process in turn
         DO npe = 1, numberOfProcs-1
            CALL MPI_SEND (ready_to_receive, 1, MPI_INTEGER,
     &           npe, itag, MPI_COMM_MODEL, ierr)
            CALL MPI_RECV (temp, lbuff, MPI_DOUBLE_PRECISION,
     &           npe, itag, MPI_COMM_MODEL, istatus, ierr)

C--   Process 0 gathers the local arrays into a global array.
            iG=mpi_myXGlobalLo(npe+1)/sNx+1
            jG=mpi_myYGlobalLo(npe+1)/sNy+1
cph(
cph            if (lprint) then
cph               print *, 'ph-gather A ', npe, 
cph     &              mpi_myXGlobalLo(npe+1), mpi_myYGlobalLo(npe+1)
cph               print *, 'ph-gather B ', npe, iG, jG
cph            endif
cph)
            DO l=1,length
               lG= (jG-1)*nPx*length + (iG-1)*length + l
               global(lG) = temp(l)
            ENDDO


         ENDDO

      ELSE

C--   All proceses except 0 wait to be polled then send local array
         CALL MPI_RECV (ready_to_receive, 1, MPI_INTEGER,
     &        idest, itag, MPI_COMM_MODEL, istatus, ierr)
         CALL MPI_SEND (local, lbuff, MPI_DOUBLE_PRECISION,
     &        idest, itag, MPI_COMM_MODEL, ierr)

      ENDIF

#endif /* ALLOW_USE_MPI */

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
