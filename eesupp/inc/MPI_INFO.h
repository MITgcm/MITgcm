C $Header: /u/gcmpack/MITgcm/eesupp/inc/MPI_INFO.h,v 1.2 2001/02/04 14:38:42 cnh Exp $
C $Name:  $
C      Parameters used with MPI.

       COMMON /MPI_INFO/
     &  mpi_pid,     mpi_np,
     &  mpi_northId, mpi_southId
       INTEGER mpi_pid
       INTEGER mpi_np
       INTEGER mpi_northId
       INTEGER mpi_southId
