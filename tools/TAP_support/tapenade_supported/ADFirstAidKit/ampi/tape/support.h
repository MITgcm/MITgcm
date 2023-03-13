/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_TAPE_SUPPORT_H_
#define _AMPI_TAPE_SUPPORT_H_

#include <mpi.h>
#if defined(__cplusplus)
extern "C" {
#endif

#include "ampi/userIF/request.h"

/**
 * \file 
 * \brief interface to a reusable tape to read and write in particular the opaque MPI types
 */ 

void TAPE_AMPI_init();
void TAPE_AMPI_resetBottom();
void TAPE_AMPI_resetTop();

void TAPE_AMPI_push_int(int an_int);
void TAPE_AMPI_pop_int(int *an_int);
void TAPE_AMPI_read_int(int* an_int);

void TAPE_AMPI_push_MPI_Aint(MPI_Aint an_MPI_Aint);
void TAPE_AMPI_pop_MPI_Aint(MPI_Aint *an_MPI_Aint);
void TAPE_AMPI_read_MPI_Aint(MPI_Aint* an_MPI_Aint);

void TAPE_AMPI_push_ptr(void *ptr);
void TAPE_AMPI_pop_ptr(void **ptr);
void TAPE_AMPI_read_ptr(void **ptr);

void TAPE_AMPI_push_MPI_Datatype(MPI_Datatype an_MPI_Datatype);
void TAPE_AMPI_pop_MPI_Datatype(MPI_Datatype *an_MPI_Datatype);
void TAPE_AMPI_read_MPI_Datatype(MPI_Datatype* an_MPI_Datatype);

void TAPE_AMPI_push_MPI_Comm(MPI_Comm an_MPI_Comm);
void TAPE_AMPI_pop_MPI_Comm(MPI_Comm *an_MPI_Comm);
void TAPE_AMPI_read_MPI_Comm(MPI_Comm* an_MPI_Comm);

void TAPE_AMPI_push_MPI_Request(MPI_Request an_MPI_Request);
void TAPE_AMPI_pop_MPI_Request(MPI_Request *an_MPI_Request);
void TAPE_AMPI_read_MPI_Request(MPI_Request* an_MPI_Request);

void TAPE_AMPI_push_MPI_Op(MPI_Op an_MPI_Op);
void TAPE_AMPI_pop_MPI_Op(MPI_Op *an_MPI_Op);
void TAPE_AMPI_read_MPI_Op(MPI_Op* an_MPI_Op);

void TAPE_AMPI_push_double(double a_double);
void TAPE_AMPI_pop_double(double *a_double);
void TAPE_AMPI_read_double(double* a_double);

void TAPE_AMPI_push_MPI_Win(MPI_Win an_MPI_Win);
void TAPE_AMPI_pop_MPI_Win(MPI_Win *an_MPI_Win);
void TAPE_AMPI_read_MPI_Win(MPI_Win* an_MPI_Win);

#if defined(__cplusplus)
}
#endif

#endif

