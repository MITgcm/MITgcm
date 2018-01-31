CBOP
C      !ROUTINE: MPI_INFO.h
C      !INTERFACE:
C      include "MPI_INFO.h"
C      !DESCRIPTION:
C      Parameters used with MPI.
CEOP

       COMMON /MPI_INFO/
     &  mpi_pid,     mpi_np,
     &  mpi_northId, mpi_southId
       INTEGER mpi_pid
       INTEGER mpi_np
       INTEGER mpi_northId
       INTEGER mpi_southId
