C $Header: /u/gcmpack/MITgcm/eesupp/inc/MPI_INFO.h,v 1.3 2001/09/21 03:54:36 cnh Exp $
C $Name:  $
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
