/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_MODIFIED_H_
#define _AMPI_MODIFIED_H_

/**
 * \file 
 * \ingroup UserInterfaceHeaders
 * AMPI routines that have adjoint functionality and do no merely pass through to the MPI originals; the routines may signatures with additional parameters compared to their original MPI counterparts
 */ 

#include "ampi/userIF/pairedWith.h"
#include "ampi/userIF/request.h"
#include "ampi/userIF/window.h"

/**
 * \todo move out of userIF
 * user-defined type data
 * only one instance of derivedTypeData exists at once
 * get pointer from getDTypeData, add new stuff with addDTypeData
 */
typedef struct {
  int size;
  int preAlloc;
  int* num_actives;
  /* displacements of first/last active blocks */
  MPI_Aint* first_active_blocks;
  MPI_Aint* last_active_blocks;
  /* need to know last active block length to find last active element */
  int* last_active_block_lengths;
  MPI_Datatype* derived_types;
  int* counts;
  int** arrays_of_blocklengths;
  MPI_Aint** arrays_of_displacements;
  MPI_Datatype** arrays_of_types;
  MPI_Aint* lbs;
  MPI_Aint* extents;
  /* corresponding typemaps packed for sending */
  MPI_Datatype* packed_types;
  int** arrays_of_p_blocklengths;
  MPI_Aint** arrays_of_p_displacements;
  MPI_Datatype** arrays_of_p_types;
  MPI_Aint* p_extents;
} derivedTypeData;

/**
 * \todo move out of userIF
 *
 */
derivedTypeData* getDTypeData();

/**
 * \todo move out of userIF
 *
 */
void releaseDTypeData();
/**
 * \todo move out of userIF
 *
 * @param dat
 * @param count
 * @param array_of_blocklengths
 * @param array_of_displacements
 * @param array_of_types
 * @param lower_bound
 * @param extent
 * @param array_of_p_blocklengths
 * @param array_of_p_displacements
 * @param array_of_p_types
 * @param p_extent
 * @param newtype
 * @param packed_type
 * addDTypeData takes derived type data and adds a new entry; returns
 * position of new type in data struct; returns -1 if struct contains no active types;
 * doubles data struct size every time there's overflow
 */
void addDTypeData(derivedTypeData* dat,
		  int count,
		  int array_of_blocklengths[],
		  MPI_Aint array_of_displacements[],
		  MPI_Datatype array_of_types[],
		  MPI_Aint lower_bound,
		  MPI_Aint extent,
		  int array_of_p_blocklengths[],
		  MPI_Aint array_of_p_displacements[],
		  MPI_Datatype array_of_p_types[],
		  MPI_Aint p_extent,
		  MPI_Datatype* newtype,
		  MPI_Datatype* packed_type);
int derivedTypeIdx(MPI_Datatype datatype);
/**
 * \todo move out of userIF
 * @param dt_idx
 * @return
 */
int isDerivedType(int dt_idx);

/**
 * \todo move out of userIF
 * user-defined reduction op data
 * only one instance of userDefinedOpData exists at once
 * get pointer from getUOpData, add new stuff with addUOpData
 */
typedef struct {
  int size;
  int preAlloc;
  MPI_Op* ops;
  MPI_User_function** functions;
  int* commutes;
} userDefinedOpData;

/**
 * \todo move out of userIF
 * @return
 */
userDefinedOpData* getUOpData();

/**
 * \todo move out of userIF
 * @param dat
 * @param op a user-defined operation
 * @param function
 * @param commute
 * takes user-defined op  and adds a new entry;
 * doubles data struct size every
 * time there's overflow
 */
void addUOpData(userDefinedOpData* dat,
		MPI_Op* op,
		MPI_User_function* function,
		int commute);
/**
 * \todo move out of userIF
 * @param op
 * @return
 */
int userDefinedOpIdx(MPI_Op op);
/**
 * \todo move out of userIF
 * @param uop_idx
 * @return
 */
int isUserDefinedOp(int uop_idx);
/**
 * \todo move out of userIF
 */
void releaseUOpData();

/** \ingroup UserInterfaceDeclarations
 * @{
 */

/**
 * active variant of the predefined  MPI_DOUBLE
 */
extern MPI_Datatype AMPI_ADOUBLE;

/**
 * active variant of the predefined  MPI_FLOAT
 */
extern MPI_Datatype AMPI_AFLOAT;

#ifdef AMPI_FORTRANCOMPATIBLE

/**
 * active variant of the predefined  MPI_DOUBLE_PRECISION
 */
extern MPI_Datatype AMPI_ADOUBLE_PRECISION;

/**
 * active variant of the predefined  MPI_REAL
 */
extern MPI_Datatype AMPI_AREAL;

#endif

/**
 * adjoint needs to MPI_Finalize; signature identical to original MPI call
 */
int AMPI_Init(int* argc, 
	      char*** argv);

/**
 * adjoint needs to MPI_Init; signature identical to AMPI_Init (adjoint symmetry)
 */
int AMPI_Finalize(int* argc, 
		  char*** argv);

/**
 * adjoint needs to detach; signature identical to original MPI call
 */ 
int AMPI_Buffer_attach(void *buffer, 
		       int size); 

/**
 * adjoint needs to attach; signature identical to original MPI call
 */ 
int AMPI_Buffer_detach(void *buffer, 
		       int *size);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param dest
 * @param tag
 * @param pairedWith see \ref pairings
 * @param comm
 * @return
 */
int AMPI_Send(void* buf, 
	      int count, 
	      MPI_Datatype datatype, 
	      int dest, 
	      int tag, 
	      AMPI_PairedWith pairedWith,
	      MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param src
 * @param tag
 * @param pairedWith see \ref pairings
 * @param comm
 * @param status
 * @return
 */
int AMPI_Recv(void* buf, 
	      int count,
	      MPI_Datatype datatype, 
	      int src, 
	      int tag, 
	      AMPI_PairedWith pairedWith,
	      MPI_Comm comm,
	      MPI_Status* status);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param dest
 * @param tag
 * @param pairedWith see \ref pairings
 * @param comm
 * @param request see \ref requests
 * @return
 */
int AMPI_Isend (void* buf, 
		int count, 
		MPI_Datatype datatype, 
		int dest, 
		int tag, 
		AMPI_PairedWith pairedWith,
		MPI_Comm comm, 
		AMPI_Request* request);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param src
 * @param tag
 * @param pairedWith see \ref pairings
 * @param comm
 * @param request see \ref requests
 * @return
 */
int AMPI_Irecv (void* buf, 
		int count, 
		MPI_Datatype datatype, 
		int src, 
		int tag,
		AMPI_PairedWith pairedWith, 
		MPI_Comm comm, 
		AMPI_Request* request);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param dest
 * @param tag
 * @param pairedWith see \ref pairings
 * @param comm
 * @return
 */
int AMPI_Bsend(void *buf, 
	       int count, 
	       MPI_Datatype datatype, 
	       int dest, 
	       int tag,
	       AMPI_PairedWith pairedWith, 
	       MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param dest
 * @param tag
 * @param pairedWith see \ref pairings
 * @param comm
 * @return
 */
int AMPI_Rsend(void *buf, 
	       int count, 
	       MPI_Datatype datatype, 
	       int dest, 
	       int tag,
	       AMPI_PairedWith pairedWith,
	       MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param buf
 * @param count
 * @param datatype see \ref datatypes
 * @param root
 * @param comm
 * @return
 */
int AMPI_Bcast (void* buf,
		int count,
		MPI_Datatype datatype,
		int root,
		MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sbuf
 * @param rbuf
 * @param count
 * @param datatype see \ref datatypes
 * @param op
 * @param root
 * @param comm
 * @return
 */
int AMPI_Reduce (void* sbuf, 
		 void* rbuf, 
		 int count, 
		 MPI_Datatype datatype, 
		 MPI_Op op, 
		 int root, 
		 MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sbuf
 * @param rbuf
 * @param count
 * @param datatype see \ref datatypes
 * @param op
 * @param comm
 * @return
 */
int AMPI_Allreduce (void* sbuf,
                    void* rbuf,
                    int count,
                    MPI_Datatype datatype,
                    MPI_Op op,
                    MPI_Comm comm);


/**
 * before we start reverse we need to make sure there are no pending requests in our userIF bookkeeping 
 */
int AMPI_Wait(AMPI_Request *request, 
	      MPI_Status *status);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param count
 * @param requests see \ref requests
 * @param statuses
 * @return
 */
int AMPI_Waitall (int count, 
		  AMPI_Request requests[], 
		  MPI_Status statuses[]);

/**
 * @param count
 * @param requests see \ref requests
 * @param statuses
 * @return
 */
int AMPI_Awaitall (int count, 
		   AMPI_Request requests[], 
		   MPI_Status statuses[]);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param comm
 * @return
 */
int AMPI_Barrier(MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sendbuf
 * @param sendcnt
 * @param sendtype see \ref datatypes
 * @param recvbuf
 * @param recvcnt
 * @param recvtype see \ref datatypes
 * @param root
 * @param comm
 * @return
 */
int AMPI_Gather(void *sendbuf,
		int sendcnt,
		MPI_Datatype sendtype,
		void *recvbuf,
		int recvcnt,
		MPI_Datatype recvtype,
		int root,
		MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sendbuf
 * @param sendcnt
 * @param sendtype see \ref datatypes
 * @param recvbuf
 * @param recvcnt
 * @param recvtype see \ref datatypes
 * @param root
 * @param comm
 * @return
 */
int AMPI_Scatter(void *sendbuf,
		 int sendcnt,
		 MPI_Datatype sendtype,
		 void *recvbuf,
		 int recvcnt,
		 MPI_Datatype recvtype,
		 int root, 
		 MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sendbuf
 * @param sendcount
 * @param sendtype see \ref datatypes
 * @param recvbuf
 * @param recvcount
 * @param recvtype see \ref datatypes
 * @param comm
 * @return
 */
int AMPI_Allgather(void *sendbuf,
                   int sendcount,
                   MPI_Datatype sendtype,
                   void *recvbuf,
                   int recvcount,
                   MPI_Datatype recvtype,
                   MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sendbuf
 * @param sendcnt
 * @param sendtype see \ref datatypes
 * @param recvbuf
 * @param recvcnts
 * @param displs
 * @param recvtype see \ref datatypes
 * @param root
 * @param comm
 * @return
 */
int AMPI_Gatherv(void *sendbuf,
                 int sendcnt,
                 MPI_Datatype sendtype,
                 void *recvbuf,
                 int *recvcnts,
                 int *displs,
                 MPI_Datatype recvtype,
                 int root,
                 MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sendbuf
 * @param sendcnts
 * @param displs
 * @param sendtype see \ref datatypes
 * @param recvbuf
 * @param recvcnt
 * @param recvtype see \ref datatypes
 * @param root
 * @param comm
 * @return
 */
int AMPI_Scatterv(void *sendbuf,
                  int *sendcnts,
                  int *displs,
                  MPI_Datatype sendtype,
                  void *recvbuf,
                  int recvcnt,
                  MPI_Datatype recvtype,
                  int root, MPI_Comm comm);

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param sendbuf
 * @param sendcnt
 * @param sendtype see \ref datatypes
 * @param recvbuf
 * @param recvcnts
 * @param displs
 * @param recvtype see \ref datatypes
 * @param comm
 * @return
 */
int AMPI_Allgatherv(void *sendbuf,
                    int sendcnt,
                    MPI_Datatype sendtype,
                    void *recvbuf,
                    int *recvcnts,
                    int *displs,
                    MPI_Datatype recvtype,
                    MPI_Comm comm);


int AMPI_Comm_dup(MPI_Comm comm, MPI_Comm *dupComm) ;

/**
 * Same as \ref AMPI_Comm_dup but manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade)
 */
int TLS_AMPI_Comm_dup(MPI_Comm comm, MPI_Comm *dupComm) ;

int AMPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *dupComm) ;

/**
 * Same as \ref AMPI_Comm_split but manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade)
 */
int TLS_AMPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *dupComm) ;

int AMPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *dupComm) ;

/**
 * Same as \ref AMPI_Comm_create but manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade)
 */
int TLS_AMPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *dupComm) ;

int AMPI_Comm_free(MPI_Comm *comm) ;

/**
 * Same as \ref AMPI_Comm_free but manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade)
 */
int TLS_AMPI_Comm_free(MPI_Comm *comm) ;

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param base
 * @param size
 * @param disp_unit
 * @param info
 * @param comm
 * @param win active window
 *
 * @return 
 */
int AMPI_Win_create( void *base,
		     MPI_Aint size,
		     int disp_unit,
		     MPI_Info info,
		     MPI_Comm comm,
		     AMPI_Win *win );

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param assert
 * @param win active window
 *
 * @return 
 */
int AMPI_Win_fence( int assert,
                    AMPI_Win win );

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param win active window
 *
 * @return 
 */
int AMPI_Win_free( AMPI_Win *win );

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param origin_addr
 * @param origin_count
 * @param origin_datatype see \ref datatypes
 * @param target_rank
 * @param target_disp
 * @param target_count
 * @param target_datatype see \ref datatypes
 * @param win active window
 *
 * @return 
 */
int AMPI_Get( void *origin_addr,
	      int origin_count,
	      MPI_Datatype origin_datatype, 
	      int target_rank,
	      MPI_Aint target_disp,
	      int target_count,
	      MPI_Datatype target_datatype,
	      AMPI_Win win );

/**
 * all parameters as in the MPI standard with exceptions as listed
 * @param origin_addr
 * @param origin_count
 * @param origin_datatype see \ref datatypes
 * @param target_rank
 * @param target_disp
 * @param target_count
 * @param target_datatype see \ref datatypes
 * @param win active window
 *
 * @return 
 */
int AMPI_Put( void *origin_addr,
	      int origin_count,
	      MPI_Datatype origin_datatype, 
	      int target_rank,
	      MPI_Aint target_disp,
	      int target_count,
	      MPI_Datatype target_datatype,
	      AMPI_Win win );

#endif
