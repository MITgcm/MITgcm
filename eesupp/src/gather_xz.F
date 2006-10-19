C $Header: /u/gcmpack/MITgcm/eesupp/src/gather_xz.F,v 1.2 2006/10/19 06:54:23 dimitri Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE GATHER_XZ( global, local, myThid )
C     Gather elements of a x-z array from all mpi processes to process 0.
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C     mythid - thread number for this instance of the routine.
C     global,local - working arrays used to transfer 2-D fields
      INTEGER mythid 
      Real*8  global(Nx)
      _RL     local(1-OLx:sNx+OLx,nSx,nSy)

      INTEGER iG, i, bi, bj
#ifdef ALLOW_USE_MPI

      _RL     temp(1-OLx:sNx+OLx,nSx,nSy)

      INTEGER istatus(MPI_STATUS_SIZE), ierr
      INTEGER lbuff, idest, itag, npe, ready_to_receive
#endif /* ALLOW_USE_MPI */

C--   Make everyone wait except for master thread.
      _BARRIER
      _BEGIN_MASTER( myThid )

#ifndef ALLOW_USE_MPI

      DO bj=1,nSy
         DO bi=1,nSx
               DO i=1,sNx
                  iG = myXGlobalLo-1+(bi-1)*sNx+i
                  global(iG) = local(i,bi,bj)
               ENDDO
         ENDDO
      ENDDO

#else /* ALLOW_USE_MPI */

      lbuff = (sNx+2*OLx)*nSx*nSy
      idest = 0
      itag  = 0
      ready_to_receive = 0

      IF( mpiMyId .EQ. 0 ) THEN

C--   Process 0 fills-in its local data
         npe = 0
         DO bj=1,nSy
            DO bi=1,nSx
                  DO i=1,sNx
                     iG = mpi_myXGlobalLo(npe+1)-1+(bi-1)*sNx+i
                     global(iG) = local(i,bi,bj)
                  ENDDO
            ENDDO
         ENDDO

C--   Process 0 polls and receives data from each process in turn
         DO npe = 1, numberOfProcs-1
#ifndef DISABLE_MPI_READY_TO_RECEIVE
            CALL MPI_SEND (ready_to_receive, 1, MPI_INTEGER,
     &           npe, itag, MPI_COMM_MODEL, ierr)
#endif
            CALL MPI_RECV (temp, lbuff, MPI_DOUBLE_PRECISION,
     &           npe, itag, MPI_COMM_MODEL, istatus, ierr)

C--   Process 0 gathers the local arrays into a global array.
            DO bj=1,nSy
               DO bi=1,nSx
                     DO i=1,sNx
                        iG = mpi_myXGlobalLo(npe+1)-1+(bi-1)*sNx+i
                        global(iG) = temp(i,bi,bj)
                     ENDDO
                  ENDDO
            ENDDO
         ENDDO

      ELSE

C--   All proceses except 0 wait to be polled then send local array
#ifndef DISABLE_MPI_READY_TO_RECEIVE
         CALL MPI_RECV (ready_to_receive, 1, MPI_INTEGER,
     &        idest, itag, MPI_COMM_MODEL, istatus, ierr)
#endif
         CALL MPI_SEND (local, lbuff, MPI_DOUBLE_PRECISION,
     &        idest, itag, MPI_COMM_MODEL, ierr)

      ENDIF

#endif /* ALLOW_USE_MPI */

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
