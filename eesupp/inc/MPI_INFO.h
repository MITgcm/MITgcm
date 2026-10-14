!BOP
 ! !ROUTINE: MPI_INFO.h
 ! !INTERFACE:
 ! include "MPI_INFO.h"
 ! !DESCRIPTION:
 ! Parameters used with MPI.
!EOP

       COMMON /MPI_INFO/                                                          &
     &       mpi_pid,     mpi_np,                                                 &
     &       mpi_northId, mpi_southId
       INTEGER :: mpi_pid
       INTEGER :: mpi_np
       INTEGER :: mpi_northId
       INTEGER :: mpi_southId
