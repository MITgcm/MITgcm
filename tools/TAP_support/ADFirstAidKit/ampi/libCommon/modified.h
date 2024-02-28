/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_LIBCOMMON_MODIFIED_H_
#define _AMPI_LIBCOMMON_MODIFIED_H_

/**
 * \file 
 * common AD implementation portion of AMPI routines from ampi/userIF/modifief.h
 */ 

#include <mpi.h>

#if defined(__cplusplus)
extern "C" {
#endif

#include "ampi/userIF/pairedWith.h"
#include "ampi/userIF/request.h"
#include "ampi/userIF/window.h"
#include "ampi/adTool/support.h"

/** 
 * forward sweep variant of \ref AMPI_Recv 
 */
int FW_AMPI_Recv(void* buf, 
		 int count,
		 MPI_Datatype datatype, 
		 int src, 
		 int tag, 
		 AMPI_PairedWith pairedWith,
		 MPI_Comm comm,
		 MPI_Status* status);

/** 
 * backward sweep variant of \ref AMPI_Recv 
 */
int BW_AMPI_Recv(void* buf, 
		 int count,
		 MPI_Datatype datatype, 
		 int src, 
		 int tag, 
		 AMPI_PairedWith pairedWith,
		 MPI_Comm comm,
		 MPI_Status* status);

/** 
 * TLM variant of \ref AMPI_Recv
 */
int TLM_AMPI_Recv(void* buf,
                  int count,
                  MPI_Datatype datatype,
                  int src,
                  int tag,
                  AMPI_PairedWith pairedWith,
                  MPI_Comm comm,
                  MPI_Status* status);

/**
 * Tangent Recv, with separate shadow (i.e. tangent) buffer.
 */
int TLS_AMPI_Recv(void* buf, void* shadowbuf,
                  int count,
                  MPI_Datatype datatype, MPI_Datatype shadowdatatype,
                  int src,
                  int tag,
                  AMPI_PairedWith pairedWith,
                  MPI_Comm comm,
                  MPI_Status* status) ;

/**
 * forward sweep variant of \ref AMPI_Irecv 
 */
int FW_AMPI_Irecv (void* buf, 
		   int count, 
		   MPI_Datatype datatype, 
		   int source, 
		   int tag,
		   AMPI_PairedWith pairedWith,
		   MPI_Comm comm, 
		   AMPI_Request* request);

/** 
 * backward sweep variant of \ref AMPI_Irecv 
 */
int BW_AMPI_Irecv (void* buf, 
		   int count, 
		   MPI_Datatype datatype, 
		   int source, 
		   int tag,
		   AMPI_PairedWith pairedWith,
		   MPI_Comm comm, 
		   AMPI_Request* request);

/** 
 * TLM variant of \ref AMPI_Irecv
 */
int TLM_AMPI_Irecv (void* buf,
                    int count,
                    MPI_Datatype datatype,
                    int source,
                    int tag,
                    AMPI_PairedWith pairedWith,
                    MPI_Comm comm,
                    AMPI_Request* request);

/**
 * Tangent Irecv, with separate shadow (i.e. tangent) buffer.
 */
int TLS_AMPI_Irecv (void* buf, void* shadowbuf,
                    int count,
                    MPI_Datatype datatype, MPI_Datatype shadowdatatype,
                    int source,
                    int tag,
                    AMPI_PairedWith pairedWith,
                    MPI_Comm comm,
                    AMPI_Request* request) ;

/**
 * forward sweep variant of \ref AMPI_Send
 */
int FW_AMPI_Send (void* buf, 
                  int count, 
                  MPI_Datatype datatype, 
                  int dest, 
                  int tag,
                  AMPI_PairedWith pairedWith,
                  MPI_Comm comm);

/** 
 * backward sweep variant of \ref AMPI_Send
 */
int BW_AMPI_Send (void* buf,
                  int count, 
                  MPI_Datatype datatype, 
                  int dest, 
                  int tag,
                  AMPI_PairedWith pairedWith,
                  MPI_Comm comm);

/** 
 * TLM variant of \ref AMPI_Send
 */
int TLM_AMPI_Send (void* buf,
                   int count,
                   MPI_Datatype datatype,
                   int dest,
                   int tag,
                   AMPI_PairedWith pairedWith,
                   MPI_Comm comm);

/**
 * Tangent Send, with separate shadow (i.e. tangent) buffer.
 */
int TLS_AMPI_Send (void* buf,  void* shadowbuf,
                   int count,
                   MPI_Datatype datatype, MPI_Datatype shadowdatatype,
                   int dest,
                   int tag,
                   AMPI_PairedWith pairedWith,
                   MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Isend 
 */
int FW_AMPI_Isend (void* buf, 
		   int count, 
		   MPI_Datatype datatype, 
		   int dest, 
		   int tag, 
		   AMPI_PairedWith pairedWith,
		   MPI_Comm comm, 
		   AMPI_Request* request);

/** 
 * backward sweep variant of \ref AMPI_Isend 
 */
int BW_AMPI_Isend (void* buf, 
		   int count, 
		   MPI_Datatype datatype, 
		   int dest, 
		   int tag, 
		   AMPI_PairedWith pairedWith,
		   MPI_Comm comm, 
		   AMPI_Request* request);

/** 
 * TLM variant of \ref AMPI_Isend
 */
int TLM_AMPI_Isend (void* buf,
                    int count,
                    MPI_Datatype datatype,
                    int dest,
                    int tag,
                    AMPI_PairedWith pairedWith,
                    MPI_Comm comm,
                    AMPI_Request* request);

/**
 * Tangent Isend, with separate shadow (i.e. tangent) buffer.
 */
int TLS_AMPI_Isend (void* buf, void* shadowbuf,
                    int count,
                    MPI_Datatype datatype, MPI_Datatype shadowdatatype,
                    int dest,
                    int tag,
                    AMPI_PairedWith pairedWith,
                    MPI_Comm comm,
                    AMPI_Request* request);

/**
 * forward sweep variant of \ref AMPI_Wait 
 */
int FW_AMPI_Wait(AMPI_Request *request, 
		 MPI_Status *status);

/** 
 * backward sweep variant of \ref AMPI_Wait 
 */
int BW_AMPI_Wait(AMPI_Request *request, 
		 MPI_Status *status);

/**
 * TLM variant of \ref AMPI_Wait
 */
int TLM_AMPI_Wait(AMPI_Request *request,
                 MPI_Status *status);

/**
 * Tangent Wait, with separate shadow (i.e. tangent) buffer.
 */
int TLS_AMPI_Wait(AMPI_Request *request,
                  MPI_Status *status);

/**
 * forward sweep variant of \ref AMPI_Barrier
 */
int FW_AMPI_Barrier(MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Barrier
 */
int BW_AMPI_Barrier(MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Barrier
 */
int TLM_AMPI_Barrier(MPI_Comm comm);

/**
 * TLS variant of \ref AMPI_Barrier
 */
int TLS_AMPI_Barrier(MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Gather
 */
int FW_AMPI_Gather(void *sendbuf,
                   int sendcnt,
                   MPI_Datatype sendtype,
                   void *recvbuf,
                   int recvcnt,
                   MPI_Datatype recvtype,
                   int root,
                   MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Gather
 */
int BW_AMPI_Gather(void *sendbuf,
                   int sendcnt,
                   MPI_Datatype sendtype,
                   void *recvbuf,
                   int recvcnt,
                   MPI_Datatype recvtype,
                   int root,
                   MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Gather. Bundled (Association-by-Address).
 */
int TLM_AMPI_Gather(void *sendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype,
                    void *recvbuf,
                    int recvcnt,
                    MPI_Datatype recvtype,
                    int root,
                    MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Gather. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Gather(void *sendbuf, void *shadowsendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype, MPI_Datatype shadowsendtype,
                    void *recvbuf, void *shadowrecvbuf,
                    int recvcnt,
                    MPI_Datatype recvtype, MPI_Datatype shadowrecvtype,
                    int root,
                    MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Scatter
 */
int FW_AMPI_Scatter(void *sendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype,
                    void *recvbuf,
                    int recvcnt,
                    MPI_Datatype recvtype,
                    int root,
                    MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Scatter
 */
int BW_AMPI_Scatter(void *sendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype,
                    void *recvbuf,
                    int recvcnt,
                    MPI_Datatype recvtype,
                    int root,
                    MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Scatter. Bundled (Association-by-Address)
 */
int TLM_AMPI_Scatter(void *sendbuf,
                     int sendcnt,
                     MPI_Datatype sendtype,
                     void *recvbuf,
                     int recvcnt,
                     MPI_Datatype recvtype,
                     int root,
                     MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Scatter. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Scatter(void *sendbuf, void *shadowsendbuf,
                     int sendcnt,
                     MPI_Datatype sendtype, MPI_Datatype shadowsendtype,
                     void *recvbuf, void *shadowrecvbuf,
                     int recvcnt,
                     MPI_Datatype recvtype, MPI_Datatype shadowrecvtype,
                     int root,
                     MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Allgather
 */
int FW_AMPI_Allgather(void *sendbuf,
                      int sendcount,
                      MPI_Datatype sendtype,
                      void *recvbuf,
                      int recvcount,
                      MPI_Datatype recvtype,
                      MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Allgather
 */
int BW_AMPI_Allgather(void *sendbuf,
                      int sendcount,
                      MPI_Datatype sendtype,
                      void *recvbuf,
                      int recvcount,
                      MPI_Datatype recvtype,
                      MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Allgather. Bundled (Association-by-Address)
 */
int TLM_AMPI_Allgather(void *sendbuf,
                       int sendcount,
                       MPI_Datatype sendtype,
                       void *recvbuf,
                       int recvcount,
                       MPI_Datatype recvtype,
                       MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Allgather. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Allgather(void *sendbuf, void *shadowsendbuf,
                       int sendcount,
                       MPI_Datatype sendtype, MPI_Datatype shadowsendtype,
                       void *recvbuf, void *shadowrecvbuf,
                       int recvcount,
                       MPI_Datatype recvtype, MPI_Datatype shadowrecvtype,
                       MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Gatherv
 */
int FW_AMPI_Gatherv(void *sendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype,
                    void *recvbuf,
                    int *recvcnts,
                    int *displs,
                    MPI_Datatype recvtype,
                    int root,
                    MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Gatherv
 * NOTE: recvcnts and displs are passed with a non-NULL pointer then they must be allocated to the correct size
 */
int BW_AMPI_Gatherv(void *sendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype,
                    void *recvbuf,
                    int *recvcnts,
                    int *displs,
                    MPI_Datatype recvtype,
                    int root,
                    MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Gatherv. Bundled (Association-by-Address)
 */
int TLM_AMPI_Gatherv(void *sendbuf,
                     int sendcnt,
                     MPI_Datatype sendtype,
                     void *recvbuf,
                     int *recvcnts,
                     int *displs,
                     MPI_Datatype recvtype,
                     int root,
                     MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Gatherv. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Gatherv(void *sendbuf, void *shadowsendbuf,
                     int sendcnt,
                     MPI_Datatype sendtype, MPI_Datatype shadowsendtype,
                     void *recvbuf, void *shadowrecvbuf,
                     int *recvcnts,
                     int *displs, int *shadowdispls,
                     MPI_Datatype recvtype, MPI_Datatype shadowrecvtype,
                     int root,
                     MPI_Comm comm) ;

/**
 * forward sweep variant of \ref AMPI_Scatterv
 */
int FW_AMPI_Scatterv(void *sendbuf,
                     int *sendcnts,
                     int *displs,
                     MPI_Datatype sendtype,
                     void *recvbuf,
                     int recvcnt,
                     MPI_Datatype recvtype,
                     int root, MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Scatterv
 * NOTE: sendcnts and displs are passed with a non-NULL pointer then they must be allocated to the correct size
 */
int BW_AMPI_Scatterv(void *sendbuf,
                     int *sendcnts,
                     int *displs,
                     MPI_Datatype sendtype,
                     void *recvbuf,
                     int recvcnt,
                     MPI_Datatype recvtype,
                     int root, MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Scatterv. Bundled (Association-by-Address)
 * NOTE: sendcnts and displs are passed with a non-NULL pointer then they must be allocated to the correct size
 */
int TLM_AMPI_Scatterv(void *sendbuf,
                      int *sendcnts,
                      int *displs,
                      MPI_Datatype sendtype,
                      void *recvbuf,
                      int recvcnt,
                      MPI_Datatype recvtype,
                      int root, MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Scatterv. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Scatterv(void *sendbuf, void *shadowsendbuf,
                      int *sendcnts,
                      int *displs, int *shadowdispls,
                      MPI_Datatype sendtype, MPI_Datatype shadowsendtype,
                      void *recvbuf, void *shadowrecvbuf,
                      int recvcnt,
                      MPI_Datatype recvtype, MPI_Datatype shadowrecvtype,
                      int root, MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Allgatherv
 */
int FW_AMPI_Allgatherv(void *sendbuf,
                       int sendcnt,
                       MPI_Datatype sendtype,
                       void *recvbuf,
                       int *recvcnts,
                       int *displs,
                       MPI_Datatype recvtype,
                       MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Allgatherv
 * NOTE: recvcnts and displs are passed with a non-NULL pointer then they must be allocated to the correct size
 */
int BW_AMPI_Allgatherv(void *sendbuf,
                       int sendcnt,
                       MPI_Datatype sendtype,
                       void *recvbuf,
                       int *recvcnts,
                       int *displs,
                       MPI_Datatype recvtype,
                       MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Allgatherv. Bundled (Association-by-Address)
 * NOTE: recvcnts and displs are passed with a non-NULL pointer then they must be allocated to the correct size
 */
int TLM_AMPI_Allgatherv(void *sendbuf,
                        int sendcnt,
                        MPI_Datatype sendtype,
                        void *recvbuf,
                        int *recvcnts,
                        int *displs,
                        MPI_Datatype recvtype,
                        MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Allgatherv. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Allgatherv(void *sendbuf, void *shadowsendbuf,
                        int sendcnt,
                        MPI_Datatype sendtype, MPI_Datatype shadowsendtype,
                        void *recvbuf, void *shadowrecvbuf,
                        int *recvcnts,
                        int *displs, int *shadowdispls,
                        MPI_Datatype recvtype, MPI_Datatype shadowrecvtype,
                        MPI_Comm comm);

/**
 * forward sweep variant of \ref AMPI_Bcast
 */
int FW_AMPI_Bcast(void* buf,
		  int count,
		  MPI_Datatype datatype,
		  int root,
		  MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Bcast
 */
int BW_AMPI_Bcast(void* buf,
		  int count,
		  MPI_Datatype datatype,
		  int root,
		  MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Bcast. Bundled (Association-by-Address)
 */
int TLM_AMPI_Bcast(void* buf,
                   int count,
                   MPI_Datatype datatype,
                   int root,
                   MPI_Comm comm);

/**
 * Tangent diff variant of \ref AMPI_Bcast. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Bcast(void* buf, void* shadowbuf,
                   int count,
                   MPI_Datatype datatype, MPI_Datatype shadowdatatype,
                   int root,
                   MPI_Comm comm);

/**
 * Adjoint forward sweep of \ref AMPI_Reduce. Bundled (Association-by-Address)
 */
int FWB_AMPI_Reduce(void* sbuf,
		   void* rbuf,
		   int count,
		   MPI_Datatype datatype,
		   MPI_Op op,
		   int root,
		   MPI_Comm comm);

/**
 * Adjoint forward sweep of \ref AMPI_Reduce. Shadowed (Association-by-Name).
 * NOTE: in the forward sweep, shadowed mode passes only the primal values.
 */
int FW_AMPI_Reduce(void* sbuf,
                    void* rbuf,
                    int count,
                    MPI_Datatype datatype,
                    MPI_Op op,
                    int root,
                    MPI_Comm comm) ;


/**
 * Adjoint backward sweep of \ref AMPI_Reduce. Bundled (Association-by-Address)
 */
int BWB_AMPI_Reduce(void* sbuf,
		   void* rbuf,
		   int count,
		   MPI_Datatype datatype,
		   MPI_Op op,
		   int root,
		   MPI_Comm comm);

/**
 * Adjoint backward sweep of \ref AMPI_Reduce. Shadowed (Association-by-Name)
 */
int BWS_AMPI_Reduce(void* sbuf, void* sbufb,
		   void* rbuf, void* rbufb,
		   int count,
		   MPI_Datatype datatype, MPI_Datatype datatypeb,
		   MPI_Op op, TLM_userFunctionF* uopd,
                   int root,
                   MPI_Comm comm) ;

/**
 * Tangent diff of \ref AMPI_Reduce. Bundled (Association-by-Address)
 */
int TLB_AMPI_Reduce(void* sbuf,
                    void* rbuf,
                    int count,
                    MPI_Datatype datatype,
                    MPI_Op op,
                    int root,
                    MPI_Comm comm);

/**
 * Tangent diff of \ref AMPI_Reduce. Shadowed (Association-by-Name)
 */
int TLS_AMPI_Reduce(void* sbuf, void* sbufd,
                    void* rbuf, void* rbufd,
                    int count,
                    MPI_Datatype datatype, MPI_Datatype datatyped,
                    MPI_Op op, TLM_userFunctionF* uopd,
                    int root,
                    MPI_Comm comm) ;

/**
 * forward sweep variant of \ref AMPI_Allreduce
 */
int FWB_AMPI_Allreduce(void* sbuf,
                      void* rbuf,
                      int count,
                      MPI_Datatype datatype,
                      MPI_Op op,
                      MPI_Comm comm);

/**
 * Adjoint forward sweep of \ref AMPI_Allreduce, shadowed (i.e. Association-by-Name)
 * NOTE: in the forward sweep, shadowed mode passes only the primal values.
 */
int FW_AMPI_Allreduce(void* sbuf,
                       void* rbuf,
                       int count,
                       MPI_Datatype datatype,
                       MPI_Op op,
                       MPI_Comm comm);

/**
 * backward sweep variant of \ref AMPI_Allreduce
 */
int BWB_AMPI_Allreduce(void* sbuf,
                      void* rbuf,
                      int count,
                      MPI_Datatype datatype,
                      MPI_Op op,
                      MPI_Comm comm);

/**
 * Adjoint forward sweep of \ref AMPI_Allreduce, shadowed (i.e. Association-by-Name)
 */
int BWS_AMPI_Allreduce(void* sbuf, void* sbufb,
                       void* rbuf, void* rbufb,
                       int count,
                       MPI_Datatype datatype, MPI_Datatype datatypeb,
                       MPI_Op op, TLM_userFunctionF* uopb,
                       MPI_Comm comm);

/**
 * TLM variant of \ref AMPI_Allreduce
 */
int TLB_AMPI_Allreduce(void* sbuf,
                       void* rbuf,
                       int count,
                       MPI_Datatype datatype,
                       MPI_Op op,
                       MPI_Comm comm);

/**
 * Adjoint forward sweep of \ref AMPI_Allreduce, shadowed (i.e. Association-by-Name)
 */
int TLS_AMPI_Allreduce(void* sbuf, void* sbufd,
                       void* rbuf, void* rbufd,
                       int count,
                       MPI_Datatype datatype, MPI_Datatype datatyped,
                       MPI_Op op, TLM_userFunctionF* uopd,
                       MPI_Comm comm) ;

/**
 * create struct, calls MPI_Type_create_struct twice (second time for packed typemap) and stores info
 */
int AMPI_Type_create_struct (int count,
			     int array_of_blocklengths[],
			     MPI_Aint array_of_displacements[],
			     MPI_Datatype array_of_types[],
			     MPI_Datatype *newtype);

int AMPI_Type_create_resized (MPI_Datatype oldtype,
			      MPI_Aint lb,
			      MPI_Aint extent,
			      MPI_Datatype *newtype);

/**
 * create reduction op, calls MPI_Op_create, stores info
 */
int AMPI_Op_create(MPI_User_function *function,
		   int commute,
		   MPI_Op *op);

/**
 * One-sided MPI
 */


int FW_AMPI_Win_create( void *base,
    MPI_Aint size,
    int disp_unit,
    MPI_Info info,
    MPI_Comm comm,
    AMPI_Win *win
    );

int BW_AMPI_Win_create( void *base,
    MPI_Aint size,
    int disp_unit,
    MPI_Info info,
    MPI_Comm comm,
    AMPI_Win *win
    );

int FW_AMPI_Win_free( AMPI_Win *win );

int BW_AMPI_Win_free( AMPI_Win *win );

int FW_AMPI_Get( void *origin_addr,
    int origin_count,
    MPI_Datatype origin_datatype,
    int target_rank,
    MPI_Aint target_disp,
    int target_count,
    MPI_Datatype target_datatype,
    AMPI_Win win
    ); 

int BW_AMPI_Get( void *origin_addr,
    int origin_count,
    MPI_Datatype origin_datatype,
    int target_rank,
    MPI_Aint target_disp,
    int target_count,
    MPI_Datatype target_datatype,
    AMPI_Win win
    ); 

int FW_AMPI_Get( void *origin_addr,
    int origin_count,
    MPI_Datatype origin_datatype,
    int target_rank,
    MPI_Aint target_disp,
    int target_count,
    MPI_Datatype target_datatype,
    AMPI_Win win
    ); 

int BW_AMPI_Put( void *origin_addr,
    int origin_count,
    MPI_Datatype origin_datatype,
    int target_rank,
    MPI_Aint target_disp,
    int target_count,
    MPI_Datatype target_datatype,
    AMPI_Win win
    ); 

int FW_AMPI_Win_fence( int assert,
    AMPI_Win win ); 

int BW_AMPI_Win_fence( int assert,
    AMPI_Win win ); 



#if defined(__cplusplus)
}
#endif

#endif
