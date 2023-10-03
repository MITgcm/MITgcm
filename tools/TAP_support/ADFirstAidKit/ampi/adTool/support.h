/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_ADTOOL_SUPPORT_H_
#define _AMPI_ADTOOL_SUPPORT_H_

#include <mpi.h>
#if defined(__cplusplus)
extern "C" {
#endif

#include "ampi/userIF/request.h"
#include "ampi/userIF/activity.h"
#include "ampi/userIF/modified.h"

/**
 * \file 
 * \brief methods that an AD tool needs to implement in order to use the implementation in Common
 */ 

/**
 * The implementation of pushing the required elements for Bcast calls.
 * Might rework for conciseness. Wrote this to avoid pushing too much stuff with _pushSRinfo.
 */
void ADTOOL_AMPI_pushBcastInfo(void* buf,
			       int count,
			       MPI_Datatype datatype,
			       int root,
			       MPI_Comm comm);
typedef void (ADTOOL_AMPI_pushBcastInfoF) (void*,int, MPI_Datatype, int, MPI_Comm);


/**
 * Popping the required elements for Bcast calls.
 */
void ADTOOL_AMPI_popBcastInfo(void** buf,
			      int* count,
			      MPI_Datatype* datatype,
			      int* root,
			      MPI_Comm* comm,
			      void **idx);
typedef void (ADTOOL_AMPI_popBcastInfoF) (void**, int*, MPI_Datatype*, int*, MPI_Comm*, void**);


/**
 * Pushing and popping a block of double values, specifically for reduction results.
 */
void ADTOOL_AMPI_pushDoubleArray(void* buf,
				 int count);
typedef void (ADTOOL_AMPI_pushDoubleArrayF) (void*, int);

void ADTOOL_AMPI_popDoubleArray(double* buf,
				int* count);
typedef void (ADTOOL_AMPI_popDoubleArrayF) (double*, int*);

/**
 * The implementation of pushing the required elements for Reduce calls.
 * Might rework for conciseness. Note that we require a separate TAPE_AMPI_push_MPI_Op
 * function to push the reduce operation. I defined _push_MPI_Op in
 * AdjoinableMPI/Tape/support.c w/ header AdjoinableMPI/ampi/tape/support.h.
 */
void ADTOOL_AMPI_pushReduceInfo(void* sbuf,
				void* rbuf,
				void* resultData,
				int pushResultData,
				int count,
				MPI_Datatype datatype,
				MPI_Op op,
				int root,
				MPI_Comm comm);
typedef void (ADTOOL_AMPI_pushReduceInfoF) (void*, void*, void*, int, int, MPI_Datatype, MPI_Op, int, MPI_Comm);

/**
 * Popping the required elements for Reduce calls.
 */
void ADTOOL_AMPI_popReduceCountAndType(int* count,
				       MPI_Datatype* datatype);
typedef void (ADTOOL_AMPI_popReduceCountAndTypeF) (int*, MPI_Datatype*);

void ADTOOL_AMPI_popReduceInfo(void** sbuf,
			       void** rbuf,
			       void** prevData,
			       void** resultData,
			       int* count,
			       MPI_Op* op,
			       int* root,
			       MPI_Comm* comm,
			       void **idx);
typedef void (ADTOOL_AMPI_popReduceInfoF) (void**, void**, void**, void**, int*, MPI_Op*, int*, MPI_Comm*, void **);


/**
 * the implementation of pushing the required elements for send/recv
 * to the AD-tool-internal stack;
 * For source transformation this may remain unimplemented provided all the parameters
 * are recovered by TBR and <tt>buf</tt> is mapped explicitly.
 * the operator overloading implementation maps <tt>buf</tt> to the adjoint address space.
 * The source transformation implementation ignores <tt>buf</tt> 
 */
void ADTOOL_AMPI_pushSRinfo(void* buf, 
			    int count,
			    MPI_Datatype datatype, 
			    int src, 
			    int tag,
			    AMPI_PairedWith pairedWith,
			    MPI_Comm comm);
typedef void (ADTOOL_AMPI_pushSRinfoF) (void*, int, MPI_Datatype, int, int, AMPI_PairedWith, MPI_Comm);

/**
 * the implementation of popping the required elements for send/recv
 * from the AD-tool-internal stack;
 * See comments of \ref ADTOOL_AMPI_pushSRinfo.
 */
void ADTOOL_AMPI_popSRinfo(void** buf,
			   int* count,
			   MPI_Datatype* datatype, 
			   int* src, 
			   int* tag,
			   AMPI_PairedWith* pairedWith,
			   MPI_Comm* comm,
			   void **idx);
typedef void (ADTOOL_AMPI_popSRinfoF) (void**, int*, MPI_Datatype*, int*, int*, AMPI_PairedWith*, MPI_Comm*, void**);

/**
 * the implementation of pushing the required elements for one-sided
 * communication
 * to the AD-tool-internal stack;
 * For source transformation this may remain unimplemented provided all the parameters
 * are recovered by TBR and <tt>buf</tt> is mapped explicitly.
 * the operator overloading implementation maps <tt>buf</tt> to the adjoint address space.
 * The source transformation implementation ignores <tt>buf</tt> 
 */
void ADTOOL_AMPI_pushOSinfo(void* buf, 
			    int count,
			    MPI_Datatype datatype, 
			    int src, 
			    int tag,
			    AMPI_PairedWith pairedWith,
			    MPI_Comm comm);
typedef void (ADTOOL_AMPI_pushOSinfoF) (void*, int, MPI_Datatype, int, int, AMPI_PairedWith, MPI_Comm);

/**
 * the implementation of popping the required elements for one-sided
 * communication
 * from the AD-tool-internal stack;
 * See comments of \ref ADTOOL_AMPI_pushOSinfo.
 */
void ADTOOL_AMPI_popOSinfo(void** buf,
			   int* count,
			   MPI_Datatype* datatype, 
			   int* src, 
			   int* tag,
			   AMPI_PairedWith* pairedWith,
			   MPI_Comm* comm,
			   void **idx);
typedef void (ADTOOL_AMPI_popOSinfoF) (void**, int*, MPI_Datatype*, int*, int*, AMPI_PairedWith*, MPI_Comm*, void**);

/**
 * the implementation of pushing the required elements for gather/scatter
 * to the AD-tool-internal stack;
 * the implementation rationale follows \ref  ADTOOL_AMPI_pushSRinfo
 * NOTE: for non-root ranks the root specific parameters are ignored
 * which implies in particular that the pointers passed may not be valid
 * therefore we use commSizeForRootOrNull to discriminate
 * \param commSizeForRootOrNull is the communicator size for rank root or 0
 * \param rbuf the buffer on rank root
 * \param rcnt the count on rank root 
 * \param rtype the data type on rank root
 * \param buf the buffer on non-root ranks
 * \param count the counter for buf on non-root ranks
 * \param type the data type on non-root ranks
 * \param root the root rank
 * \param comm the communicator
 */
void ADTOOL_AMPI_pushGSinfo(int commSizeForRootOrNull,
			    void *rbuf,
			    int rcnt,
			    MPI_Datatype rtype,
			    void *buf,
			    int count,
			    MPI_Datatype type,
			    int  root,
			    MPI_Comm comm);
typedef void (ADTOOL_AMPI_pushGSinfoF) (int, void*, int, MPI_Datatype, void*, int, MPI_Datatype, int, MPI_Comm);

/**
 * this must be called before \ref ADTOOL_AMPI_popGSinfo and \ref ADTOOL_AMPI_popGSVinfo
 * \param commSizeForRootOrNull this is popped so that we may allocate buffers for
 * rcnts and displs in the subsequent call to \ref ADTOOL_AMPI_popGSVinfo
 */
void ADTOOL_AMPI_popGScommSizeForRootOrNull(int *commSizeForRootOrNull);
typedef void (ADTOOL_AMPI_popGScommSizeForRootOrNullF) (int*);

/**
 * the implementation of popping the required elements for gather/scatter
 * from the AD-tool-internal stack;
 * see comments of \ref ADTOOL_AMPI_pushGSinfo;
 * following the note there we will not be setting the values for root specific
 * arguments on non-root ranks
 * \param commSizeForRootOrNull retrieved via \ref ADTOOL_AMPI_popGScommSizeForRootOrNull
 * \param rbuf the buffer on rank rook, set if commSizeForRootOrNull>0
 * \param rcnt the size  for rank root, set if commSizeForRootOrNull>0
 * \param rtype the data type for rank root, set if commSizeForRootOrNull>0
 * \param buf the buffer for all ranks
 * \param count the count for all ranks
 * \param type the type for all ranks
 * \param root the root rank
 * \param comm the communicator for all ranks
 */
void ADTOOL_AMPI_popGSinfo(int commSizeForRootOrNull,
			   void **rbuf,
			   int *rcnt,
			   MPI_Datatype *rtype,
			   void **buf,
			   int *count,
			   MPI_Datatype *type,
			   int *root,
			   MPI_Comm *comm);
typedef void (ADTOOL_AMPI_popGSinfoF) (int, void**, int*, MPI_Datatype*, void**, int*, MPI_Datatype*, int*, MPI_Comm*);

/**
 * the implementation of pushing the required elements for gatherv/scatterv
 * to the AD-tool-internal stack;
 * the implementation rationale follows \ref  ADTOOL_AMPI_pushSRinfo
 * NOTE: for non-root ranks the root specific parameters are ignored
 * which implies in particular that the pointers passed may not be valid
 * therefore we use commSizeForRootOrNull to discriminate
 * \param commSizeForRootOrNull is the communicator size for rank root or 0
 * \param rbuf the buffer on rank root
 * \param rcnts the counters per rank on rank root
 * \param displs the displacements for rbuf on rank root
 * \param rtype the data type on rank root
 * \param buf the buffer on non-root ranks
 * \param count the counter for buf on non-root ranks
 * \param type the data type on non-root ranks
 * \param root the root rank
 * \param comm the communicator
 */
void ADTOOL_AMPI_pushGSVinfo(int commSizeForRootOrNull,
                             void *rbuf,
                             int *rcnts,
                             int *displs,
                             MPI_Datatype rtype,
                             void *buf,
                             int  count,
                             MPI_Datatype type,
                             int  root,
                             MPI_Comm comm);
typedef void (ADTOOL_AMPI_pushGSVinfoF) (int, void*, int*, int*, MPI_Datatype, void*, int, MPI_Datatype, int, MPI_Comm);

/**
 * the implementation of popping the required elements for gatherv/scatterv
 * from the AD-tool-internal stack;
 * see comments of \ref ADTOOL_AMPI_pushGSVinfo;
 * following the note there we will not be setting the values for root specific
 * arguments on non-root ranks
 * \param commSizeForRootOrNull retrieved via \ref ADTOOL_AMPI_popGScommSizeForRootOrNull
 * \param rbuf the buffer on rank rook, set if commSizeForRootOrNull>0
 * \param rcnts the array of size commSizeForRootOrNull for rank root, set if commSizeForRootOrNull>0
 * \param displs the array of size commSizeForRootOrNull  for rank root, set if commSizeForRootOrNull>0
 * \param rtype the data type for rank root, set if commSizeForRootOrNull>0
 * \param buf the buffer for all ranks
 * \param count the count for all ranks
 * \param type the type for all ranks
 * \param root the root rank
 * \param comm the communicator for all ranks
 */
void ADTOOL_AMPI_popGSVinfo(int commSizeForRootOrNull,
                            void **rbuf,
                            int *rcnts,
                            int *displs,
                            MPI_Datatype *rtype,
                            void **buf,
                            int *count,
                            MPI_Datatype *type,
                            int *root,
                            MPI_Comm *comm);
typedef void (ADTOOL_AMPI_popGSVinfoF) (int, void**, int*, int*, MPI_Datatype*, void**, int*, MPI_Datatype*, int*, MPI_Comm*);

/**
 * the implementation of pushing an operation code to the 
 * to the AD-tool-internal stack for an operator overloading tool;
 * the source transformation implementation will leave this empty;
 * this method is called in the respective <tt>FW_</tt>  variant 
 * implemented in <tt>Common</tt>
 */
void ADTOOL_AMPI_push_CallCode(enum AMPI_CallCode_E thisCall);
typedef void (ADTOOL_AMPI_push_CallCodeF) (enum AMPI_CallCode_E);

/**
 * the implementation of pushing an operation code to the 
 * to the AD-tool-internal stack for an operator overloading tool where a number
 * of locations have to be reserved on the trace, currently only called in
 * Scatterv and Gatherv;
 * the source transformation implementation will leave this empty;
 * this method is called in the respective <tt>FW_</tt>  variant 
 * implemented in <tt>Common</tt>
 */
void ADTOOL_AMPI_push_CallCodeReserve(enum AMPI_CallCode_E thisCall, unsigned int);
typedef void (ADTOOL_AMPI_push_CallCodeReserveF) (enum AMPI_CallCode_E, unsigned int);

/**
 * the implementation of popping an operation code from the 
 * to the AD-tool-internal stack for an operator overloading tool;
 * See comments of \ref ADTOOL_AMPI_push_CallCode.
 * the operator overloading tool needs to pop the code from its operation 
 * stack first and then call (with dummy parameters) the respect <tt>BW_</tt>
 * variant of the operatiorn represented by <tt>thisCall</tt> 
 */
void ADTOOL_AMPI_pop_CallCode(enum AMPI_CallCode_E *thisCall);
typedef void (ADTOOL_AMPI_pop_CallCodeF) (enum AMPI_CallCode_E*);
  


/**
 * the implementation of pushing the required elements of an \ref  AMPI_Request_S
 * to the AD-tool-internal stack
 */
void ADTOOL_AMPI_push_AMPI_Request(struct AMPI_Request_S  *ampiRequest);
typedef void (ADTOOL_AMPI_push_AMPI_RequestF) (struct AMPI_Request_S*);

/**
 * the implementation of popping the required elements of an \ref  AMPI_Request_S
 * from the AD-tool-internal stack
 */
void ADTOOL_AMPI_pop_AMPI_Request(struct AMPI_Request_S  *ampiRequest);
typedef void (ADTOOL_AMPI_pop_AMPI_RequestF) (struct AMPI_Request_S*);

  
/** 
 * Push the MPI_Request on the AD tool internal stack.
 * This is used as a key to the request bookkeeping 
 * to keep correspondence between the request Id of the FW sweep
 * to the request Id in BW sweep.
 * if we need to trace requests for a pure (operator overloading) trace evaluation
 * the Common implementation uses this to push the request 
 * See \ref bookkeeping.
 */
void ADTOOL_AMPI_push_request(MPI_Request request);
typedef void (ADTOOL_AMPI_push_requestF) (MPI_Request);


/**
 * Push a window request for one-sided communication using a specific window
 */
void ADTOOL_AMPI_push_AMPI_WinRequest(AMPI_WinRequest *winRequest);
typedef void (ADTOOL_AMPI_push_WinRequestF) (AMPI_WinRequest*);

/**
 * Pop a window request for one-sided communication using a specific window
 */
void ADTOOL_AMPI_pop_AMPI_WinRequest(AMPI_WinRequest *winRequest);
typedef void (ADTOOL_AMPI_pop_WinRequestF) (AMPI_WinRequest*);

/**
 * Push a window for one-sided communication using a specific window
 */
void ADTOOL_AMPI_push_AMPI_Win(AMPI_Win *win);
typedef void (ADTOOL_AMPI_push_AMPI_WinF) (AMPI_Win*);

/**
 * Pop a window for one-sided communication using a specific window
 */
void ADTOOL_AMPI_pop_AMPI_Win(AMPI_Win *win);
typedef void (ADTOOL_AMPI_pop_AMPI_WinF) (AMPI_Win*);

/**
 * the companion to \ref ADTOOL_AMPI_push_request.
 * See \ref bookkeeping.
 */
MPI_Request ADTOOL_AMPI_pop_request();
typedef MPI_Request (ADTOOL_AMPI_pop_requestF) ();

void ADTOOL_AMPI_push_comm(MPI_Comm comm);
typedef void (ADTOOL_AMPI_push_commF) (MPI_Comm);

/**
 * the companion to \ref ADTOOL_AMPI_push_request
 */
MPI_Comm ADTOOL_AMPI_pop_comm();
typedef MPI_Comm (ADTOOL_AMPI_pop_commF) ();

/**
 * map active data to raw data; this is to be implemented for the forward 
 * execution by tools using association-by-address; 
 * for tools using association-by-name the same address should be returned;   
 */
void * ADTOOL_AMPI_rawData(void* activeData, int *size);
typedef void* (ADTOOL_AMPI_rawDataF) (void*, int*);

/**
 * map active data to raw data; functionality similar to \ref ADTOOL_AMPI_rawData
 * except it is handling vector buffers with arrays of counts and displacements as
 * used in MPI_Gatherv or MPI_Scatterv
 */
void * ADTOOL_AMPI_rawDataV(void* activeData, int commSize, int *counts, int* displs);
typedef void * (ADTOOL_AMPI_rawDataVF) (void*, int, int*, int*);

/**
 * serialize user-defined struct for sending in forward execution in
 * association-by-address tools
 */
void * ADTOOL_AMPI_packDType(void* indata, void* outdata, int count, int idx);
typedef void * (ADTOOL_AMPI_packDTypeF) (void*, void*, int, int);

/**
 * unpack serialized user-defined struct data into its original form
 */
void * ADTOOL_AMPI_unpackDType(void* indata, void* outdata, int count, int idx);
typedef void * (ADTOOL_AMPI_unpackDTypeF) (void*, void*, int, int);

/** 
 * \todo add description
 */
void ADTOOL_AMPI_writeData(void* activeData, int *size);
typedef void (ADTOOL_AMPI_writeDataF) (void*, int*);

/** 
 * \todo add description
 */
void ADTOOL_AMPI_writeDataV(void* activeData, int *counts, int* displs);
typedef void (ADTOOL_AMPI_writeDataVF) (void*, int*, int*);

/**
 * map active data to adjoint data; this is to be implemented for the backward
 * execution by tools using association-by-address; 
 * for tools using association-by-name the same address should be returned;   
 */
void * ADTOOL_AMPI_rawAdjointData(void* activeData);
typedef void * (ADTOOL_AMPI_rawAdjointDataF) (void*);

/**
 * Declares correspondence between a buffer and its counterpart adjoint buffer
 * Adds correspondence into the request-to-buffer association list
 * This is necessary for association-by-name transfo tools.
 * should be done upon turn from FW sweep to BW sweep.
 * \param buf the original, non-differentiated buffer.
 * \param adjointBuf the corresponding adjoint buffer.
 */
void ADTOOL_AMPI_Turn(void* buf, void* adjointBuf) ;
typedef void (ADTOOL_AMPI_TurnF) (void*, void*) ;

/**
 * set it on the request; 
 * \param buf is forward sweep buffer (for source transformation tools)
 * \param ampiRequest is the request to be pushed and popped for the adjoint communication
 */
void ADTOOL_AMPI_mapBufForAdjoint(struct AMPI_Request_S  *ampiRequest,
				  void* buf);
typedef void (ADTOOL_AMPI_mapBufForAdjointF) (struct AMPI_Request_S*, void*);

/**
 * Map buffer in a one-sided communication
 */

void ADTOOL_AMPI_mapWinBufForAdjoint(AMPI_WinRequest *winRequest,
				  void* buf);
typedef void (ADTOOL_AMPI_mapWinBufForAdjointF) (AMPI_WinRequest*, void*);

/**
 * an operator overloading tool should not do anything in the implementation but see \ref ADTOOL_AMPI_mapBufForAdjoint;
 * a source transformation tool would receive the adjoint buffer as an argument
 * and set it on the request; 
 * \param buf is the adjoint buffer (for source transformation tools)
 * \param ampiRequest is the request to be used during the adjoint communication
 */
void ADTOOL_AMPI_setBufForAdjoint(struct AMPI_Request_S  *ampiRequest,
				  void* buf);
typedef void (ADTOOL_AMPI_setBufForAdjointF) (struct AMPI_Request_S *, void*);

/**  
 * this method resets \param count member to represent the buffer in terms of 
 * elements of the original MPI \param datatype; this is of particular interest for vector mode; 
 * \todo needs to be expanded for the case where the adjoint data is not represented by the same MPI data type as the 
 * original program data
 */
void ADTOOL_AMPI_getAdjointCount(int *count,
				 MPI_Datatype datatype);
typedef void (ADTOOL_AMPI_getAdjointCountF) (int*, MPI_Datatype);

/**  
 * \param ampiRequest in this instance this method resets the <tt>adjointCount</tt> member; 
 */
void ADTOOL_AMPI_setAdjointCount(struct AMPI_Request_S  *ampiRequest);
typedef void (ADTOOL_AMPI_setAdjointCountF) (struct AMPI_Request_S *);

/**  
 * calls \ref ADTOOL_AMPI_setAdjointCount and sets up a temporary buffer into which the adjoint data is received, see e.g. \ref BW_AMPI_Wait
 * \param ampiRequest is the request instance that is modified
 */
void ADTOOL_AMPI_setAdjointCountAndTempBuf(struct AMPI_Request_S *ampiRequest);
typedef void (ADTOOL_AMPI_setAdjointCountAndTempBufF) (struct AMPI_Request_S*);

/**  
 * \param winRequest in this instance this method resets the <tt>adjointCount</tt> member; 
 */
void ADTOOL_AMPI_setWinAdjointCount(AMPI_WinRequest  *winRequest);
typedef void (ADTOOL_AMPI_setWinAdjointCountF) (AMPI_WinRequest *);

/**  
 * calls \ref ADTOOL_AMPI_setAdjointCount and sets up a temporary buffer into which the adjoint data is received, see e.g. \ref BW_AMPI_Win_fence
 * \param winRequest is the request instance that is modified
 */
void ADTOOL_AMPI_setWinAdjointCountAndTempBuf(AMPI_WinRequest *winRequest);
typedef void (ADTOOL_AMPI_setWinAdjointCountAndTempBufF) (AMPI_WinRequest *);

/**  
 * synchronizes the window with incoming adjoints, applies the corresponding
 * increments and nullifies the adjoints in the window.
 * \param win is the AMPI_Win instance providing the window for the incoming
 * adjoints
 */
void ADTOOL_AMPI_syncAdjointWin(AMPI_Win *win);
typedef void (ADTOOL_AMPI_syncAdjointWinF) (AMPI_Win *);

/**
 * Allocates a temporary buffer needed to receive adjoint
 * data before adding it to the adjoint variable
 */
void* ADTOOL_AMPI_allocateTempBuf(int adjointCount, MPI_Datatype dataType, MPI_Comm comm) ;
typedef void* (ADTOOL_AMPI_allocateTempBufF) (int, MPI_Datatype, MPI_Comm) ;

/**  
 * releases the temporary buffer (allocated by \ref ADTOOL_AMPI_setAdjointCountAndTempBuf)  into which the adjoint data was received 
 */
void ADTOOL_AMPI_releaseAdjointTempBuf(void *tempBuf);
typedef void (ADTOOL_AMPI_releaseAdjointTempBufF) (void *);

/**
 * allocates buffer with active variables (needed as a temporary in Reduce)
 */
void* ADTOOL_AMPI_allocateTempActiveBuf(int count, MPI_Datatype datatype, MPI_Comm comm);
typedef void*  (ADTOOL_AMPI_allocateTempActiveBufF) (int, MPI_Datatype, MPI_Comm);

/**
 * releases buffer with active variables (used as a temporary in Reduce)
 */
void ADTOOL_AMPI_releaseTempActiveBuf(void *buf, int count, MPI_Datatype datatype);
typedef void  (ADTOOL_AMPI_releaseTempActiveBufF) (void *, int, MPI_Datatype);

/**
 * copies contents of buffer including real values of active variables
 */
void * ADTOOL_AMPI_copyActiveBuf(void* source, void* target, int count, MPI_Datatype datatype, MPI_Comm comm);
typedef void* (ADTOOL_AMPI_copyActiveBufF) (void*, void*, int, MPI_Datatype, MPI_Comm);

/**
 * Adjoint of assignment target=source*target
 */
void ADTOOL_AMPI_adjointMultiply(int count, MPI_Datatype datatype, MPI_Comm comm,
                                 void *source, void *adjointSource,
                                 void* target, void* adjointTarget) ;
typedef void (ADTOOL_AMPI_adjointMultiplyF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*);

/**
 * Adjoint of assignment target=MIN(source,target)
 */
void ADTOOL_AMPI_adjointMin(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void *source, void *adjointSource,
                            void* target, void* adjointTarget) ;
typedef void (ADTOOL_AMPI_adjointMinF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*);

/**
 * Adjoint of assignment target=MAX(source,target)
 */
void ADTOOL_AMPI_adjointMax(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void *source, void *adjointSource,
                            void* target, void* adjointTarget) ;
typedef void (ADTOOL_AMPI_adjointMaxF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*);

/**
 * Multiply the given buffer target, which holds an adjoint, with the given source value
 * \param adjointCount is the number of items in the buffer we will increment
 * \param datatype the data type of the buffer to be incremented
 * \param comm the communicator to be passed to MPI_Abort for failures
 * \param target the adjoint buffer to be multiplied
 * \param source the value to multiply by.
 * \param idx tape index for each element of the non contiguous buffer
 */
void ADTOOL_AMPI_multiplyAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source, void* idx);
typedef void (ADTOOL_AMPI_multiplyAdjointF)(int, MPI_Datatype, MPI_Comm, void*, void*, void*);

/**
 * Divide the given buffer target, which holds an adjoint, with the given source value
 * \param adjointCount is the number of items in the buffer we will increment
 * \param datatype the data type of the buffer to be incremented
 * \param comm the communicator to be passed to MPI_Abort for failures
 * \param target the adjoint buffer to be divided
 * \param source the value to divide by.
 * \param idx tape index for each element of the non contiguous buffer
 */
void ADTOOL_AMPI_divideAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source, void* idx);
typedef void (ADTOOL_AMPI_divideAdjointF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*);

/**
 * Check equality of the given buffers source1 and source2, which hold adjoints,
 * and return the result in the given target buffer.
 * \param adjointCount is the number of items in the buffer we will increment
 * \param datatype the data type of the buffer to be incremented
 * \param comm the communicator to be passed to MPI_Abort for failures
 * \param target the buffer that will hold the result (0==difference)
 * \param source1 the one buffer to compare
 * \param source2 the other buffer to compare
 * \param idx tape index for each element of the non contiguous buffer
 */
void ADTOOL_AMPI_equalAdjoints(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source1, void *source2, void* idx);
typedef void (ADTOOL_AMPI_equalAdjointsF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*);

/**
 * Increment the given buffer "target", which holds an adjoint variable,
 * with the given additional adjoint value found in "source".
 * \param adjointCount is the number of items in the buffer we will increment
 * \param datatype the data type of the buffer to be incremented
 * \param comm the communicator to be passed to MPI_Abort for failures
 * \param target the adjoint buffer to be incremented
 * \param source the adjoint value that must be added into the adjoint buffer.
 * \param idx tape index for each element of the non contiguous buffer
 */
  void ADTOOL_AMPI_incrementAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source, void* idx);
  typedef void (ADTOOL_AMPI_incrementAdjointF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*);

/**
 * Reset to zero the given buffer "target", which holds an adjoint variable.
 * \param adjointCount is the number of items in the buffer we will nullify
 * \param datatype the data type of the buffer to be nullified
 * \param comm the communicator to be passed to MPI_Abort for failures
 * \param target the adjoint buffer to be nullified
 */ 
void ADTOOL_AMPI_nullifyAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target);
typedef void (ADTOOL_AMPI_nullifyAdjointF) (int, MPI_Datatype, MPI_Comm, void*);

/**
 * create predefined active types; to be cleaned up with \ref ADTOOL_AMPI_cleanupTypes
 */
void ADTOOL_AMPI_setupTypes();
typedef void (ADTOOL_AMPI_setupTypesF)();

/**
 * cleanup types created with \ref ADTOOL_AMPI_setupTypes
 */
void ADTOOL_AMPI_cleanupTypes();
typedef void (ADTOOL_AMPI_cleanupTypesF)();

#ifdef AMPI_FORTRANCOMPATIBLE
/**
 * Fortran routine to figure out what the proper types are on the Fortran side
 * \param adouble returns the integer representation for the Fortran version of AMPI_ADOUBLE_PRECISION
 * \param real returns the integer representation for the Fortran version of AMPI_AREAL
 */
void adtool_ampi_fortransetuptypes_(MPI_Fint* adouble, MPI_Fint* areal);
typedef void (adtool_ampi_fortransetuptypes_F) (MPI_Fint*, MPI_Fint*);

void adtool_ampi_fortrancleanuptypes_(MPI_Fint* adouble, MPI_Fint* areal);
typedef void (adtool_ampi_fortrancleanuptypes_F) (MPI_Fint*, MPI_Fint*);

/**
 * Fortran routine to ask Fortran to tell the C side about the Fortran-side values of the
 * "binding" variables i.e. MPI_IN_PLACE, MPI_BOTTOM, etc.
 */
void adtool_ampi_fortransetupbindings_() ;

#endif

/**
 * Take datatype for forward mode, return datatype for transfer.
 */
MPI_Datatype ADTOOL_AMPI_FW_rawType(MPI_Datatype datatype);
typedef MPI_Datatype (ADTOOL_AMPI_FW_rawTypeF) (MPI_Datatype);

/**
 * Take datatype for reverse mode, return datatype for transfer.
 */
MPI_Datatype ADTOOL_AMPI_BW_rawType(MPI_Datatype datatype);
typedef MPI_Datatype (ADTOOL_AMPI_BW_rawTypeF) (MPI_Datatype);


/**
 * test types for activity
 * \param datatype any data type but particularly also the active data type(s) created by the tool (see \ref AMPI_ADOUBLE etc.)
 * \returns the respective enum value based on the type's activity
 */
AMPI_Activity ADTOOL_AMPI_isActiveType(MPI_Datatype datatype);
typedef AMPI_Activity (ADTOOL_AMPI_isActiveTypeF) (MPI_Datatype);

/** The global MPI_COMM_WORLD_D */
extern MPI_Comm ADTOOL_AMPI_COMM_WORLD_SHADOW;

/**
 * Maps the active buffer on a mapped buffer for a MPI_Win
 */

void *ADTOOL_AMPI_createWinMap(void *active_buf, MPI_Aint size);
typedef void *(ADTOOL_AMPI_createWinMapF) (void *active_buf, MPI_Aint size);

/**
 * Maps the active buffer on a mapped buffer for a MPI_Win
 */

void ADTOOL_AMPI_writeWinData(void *map, void *buf, MPI_Aint size);
typedef void (ADTOOL_AMPI_writeWinDataF) (void *map, void *buf, MPI_Aint size);

/**
 * Gets the size of the mapped buffer for a window at its creation. It returns
 * the size of the mapped window (may be equal to the active window).
 * \param size of the active window.
 */

MPI_Aint ADTOOL_AMPI_getWinSize(MPI_Aint size);
typedef MPI_Aint (ADTOOL_AMPI_getWinSizeF) (MPI_Aint size);
/** The functions that perform the tangent of standard reduction operations: */
void ADTOOL_AMPI_tangentMultiply(int count, MPI_Datatype datatype, MPI_Comm comm,
                                 void* target, void* tangentTarget,
                                 void *source, void *tangentSource) ;
typedef void (ADTOOL_AMPI_tangentMultiplyF) (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*) ;
void ADTOOL_AMPI_tangentMin(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void* target, void* tangentTarget,
                            void *source, void *tangentSource) ;
typedef void (ADTOOL_AMPI_tangentMinF)      (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*) ;
void ADTOOL_AMPI_tangentMax(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void* target, void* tangentTarget,
                            void *source, void *tangentSource) ;
typedef void (ADTOOL_AMPI_tangentMaxF)      (int, MPI_Datatype, MPI_Comm, void*, void*, void*, void*) ;

/**
 * Push/Pop the contents of buffer somewhere
 */
void ADTOOL_AMPI_pushBuffer(int count, MPI_Datatype datatype, MPI_Comm comm, void* buffer) ;
typedef void (ADTOOL_AMPI_pushBufferF) (int, MPI_Datatype, MPI_Comm, void*) ;
void ADTOOL_AMPI_popBuffer(int count, MPI_Datatype datatype, MPI_Comm comm, void* buffer) ;
typedef void (ADTOOL_AMPI_popBufferF) (int, MPI_Datatype, MPI_Comm, void*) ;

/** Memo mechanism for the shadow communicators used in tangent ST-AD association-by-name (e.g. Tapenade) */
void ADTOOL_AMPI_addShadowComm(MPI_Comm comm, MPI_Comm dupComm) ;
typedef void (ADTOOL_AMPI_addShadowCommF) (MPI_Comm, MPI_Comm) ;
MPI_Comm ADTOOL_AMPI_getShadowComm(MPI_Comm comm) ;
typedef MPI_Comm (ADTOOL_AMPI_getShadowCommF) (MPI_Comm) ;
void ADTOOL_AMPI_delShadowComm(MPI_Comm comm);
typedef void (ADTOOL_AMPI_delShadowCommF) (MPI_Comm) ;

struct ADTOOL_AMPI_FPCollection{
  ADTOOL_AMPI_pushBcastInfoF *pushBcastInfo_fp;
  ADTOOL_AMPI_popBcastInfoF *popBcastInfo_fp;
  ADTOOL_AMPI_pushDoubleArrayF *pushDoubleArray_fp;
  ADTOOL_AMPI_popDoubleArrayF *popDoubleArray_fp;
  ADTOOL_AMPI_pushReduceInfoF *pushReduceInfo_fp;
  ADTOOL_AMPI_popReduceCountAndTypeF *popReduceCountAndType_fp;
  ADTOOL_AMPI_popReduceInfoF *popReduceInfo_fp;
  ADTOOL_AMPI_pushSRinfoF *pushSRinfo_fp;
  ADTOOL_AMPI_popSRinfoF *popSRinfo_fp;
  ADTOOL_AMPI_pushOSinfoF *pushOSinfo_fp;
  ADTOOL_AMPI_popOSinfoF *popOSinfo_fp;
  ADTOOL_AMPI_pushGSinfoF *pushGSinfo_fp;
  ADTOOL_AMPI_popGScommSizeForRootOrNullF *popGScommSizeForRootOrNull_fp;
  ADTOOL_AMPI_popGSinfoF *popGSinfo_fp;
  ADTOOL_AMPI_pushGSVinfoF *pushGSVinfo_fp;
  ADTOOL_AMPI_popGSVinfoF *popGSVinfo_fp;
  ADTOOL_AMPI_push_CallCodeF *push_CallCode_fp;
  ADTOOL_AMPI_push_CallCodeReserveF *push_CallCodeReserve_fp;
  ADTOOL_AMPI_pop_CallCodeF *pop_CallCode_fp;
  ADTOOL_AMPI_push_AMPI_RequestF *push_AMPI_Request_fp;
  ADTOOL_AMPI_pop_AMPI_RequestF *pop_AMPI_Request_fp;
  ADTOOL_AMPI_push_AMPI_WinF *push_AMPI_Win_fp;
  ADTOOL_AMPI_pop_AMPI_WinF *pop_AMPI_Win_fp;
  ADTOOL_AMPI_push_WinRequestF *push_AMPI_WinRequest_fp;
  ADTOOL_AMPI_pop_WinRequestF *pop_AMPI_WinRequest_fp;
  ADTOOL_AMPI_push_requestF *push_request_fp;
  ADTOOL_AMPI_pop_requestF *pop_request_fp;
  ADTOOL_AMPI_push_commF *push_comm_fp;
  ADTOOL_AMPI_pop_commF *pop_comm_fp;
  ADTOOL_AMPI_rawDataF *rawData_fp;
  ADTOOL_AMPI_rawDataVF *rawDataV_fp;
  ADTOOL_AMPI_packDTypeF *packDType_fp;
  ADTOOL_AMPI_unpackDTypeF *unpackDType_fp;
  ADTOOL_AMPI_writeDataF *writeData_fp;
  ADTOOL_AMPI_writeDataVF *writeDataV_fp;
  ADTOOL_AMPI_rawAdjointDataF *rawAdjointData_fp;
  ADTOOL_AMPI_TurnF *Turn_fp;
  ADTOOL_AMPI_mapBufForAdjointF *mapBufForAdjoint_fp;
  ADTOOL_AMPI_mapWinBufForAdjointF *mapWinBufForAdjoint_fp;
  ADTOOL_AMPI_setBufForAdjointF *setBufForAdjoint_fp;
  ADTOOL_AMPI_getAdjointCountF *getAdjointCount_fp;
  ADTOOL_AMPI_setAdjointCountF *setAdjointCount_fp;
  ADTOOL_AMPI_setAdjointCountAndTempBufF *setAdjointCountAndTempBuf_fp;
  ADTOOL_AMPI_setWinAdjointCountF *setWinAdjointCount_fp;
  ADTOOL_AMPI_setWinAdjointCountAndTempBufF *setWinAdjointCountAndTempBuf_fp;
  ADTOOL_AMPI_allocateTempBufF *allocateTempBuf_fp;
  ADTOOL_AMPI_releaseAdjointTempBufF *releaseAdjointTempBuf_fp;
  ADTOOL_AMPI_allocateTempActiveBufF *allocateTempActiveBuf_fp;
  ADTOOL_AMPI_releaseTempActiveBufF *releaseTempActiveBuf_fp;
  ADTOOL_AMPI_copyActiveBufF *copyActiveBuf_fp;
  ADTOOL_AMPI_adjointMultiplyF *adjointMultiply_fp ;
  ADTOOL_AMPI_adjointMinF *adjointMin_fp ;
  ADTOOL_AMPI_adjointMaxF *adjointMax_fp ;
  ADTOOL_AMPI_multiplyAdjointF *multiplyAdjoint_fp;
  ADTOOL_AMPI_divideAdjointF *divideAdjoint_fp;
  ADTOOL_AMPI_equalAdjointsF *equalAdjoints_fp;
  ADTOOL_AMPI_incrementAdjointF *incrementAdjoint_fp;
  ADTOOL_AMPI_nullifyAdjointF *nullifyAdjoint_fp;
  ADTOOL_AMPI_setupTypesF *setupTypes_fp;
  ADTOOL_AMPI_cleanupTypesF *cleanupTypes_fp;
  ADTOOL_AMPI_FW_rawTypeF *FW_rawType_fp;
  ADTOOL_AMPI_BW_rawTypeF *BW_rawType_fp;
  ADTOOL_AMPI_createWinMapF *createWinMap_fp;
  ADTOOL_AMPI_writeWinDataF *writeWinData_fp;
  ADTOOL_AMPI_getWinSizeF *getWinSize_fp;
  ADTOOL_AMPI_syncAdjointWinF *syncAdjointWin_fp;
  ADTOOL_AMPI_isActiveTypeF *isActiveType_fp ;
  ADTOOL_AMPI_tangentMultiplyF *tangentMultiply_fp ;
  ADTOOL_AMPI_tangentMinF *tangentMin_fp ;
  ADTOOL_AMPI_tangentMaxF *tangentMax_fp ;
  ADTOOL_AMPI_pushBufferF *pushBuffer_fp ;
  ADTOOL_AMPI_popBufferF *popBuffer_fp ;
  ADTOOL_AMPI_addShadowCommF *addShadowComm_fp ;
  ADTOOL_AMPI_getShadowCommF *getShadowComm_fp ;
  ADTOOL_AMPI_delShadowCommF *delShadowComm_fp ;
#ifdef AMPI_FORTRANCOMPATIBLE
  adtool_ampi_fortransetuptypes_F *fortransetuptypes__fp;
  adtool_ampi_fortrancleanuptypes_F *fortrancleanuptypes__fp;
#endif
};

/**
 * the single instance of ADTOOL_AMPI_FPCollection
 */
extern struct ADTOOL_AMPI_FPCollection ourADTOOL_AMPI_FPCollection;

/** The type required for TANGENT user-given reduction functions,
 * that are passed e.g. to TLM_AMPI_Reduce in Tapenade-style diff AMPI code. */
typedef void (TLM_userFunctionF) (void*, void*, void*, void*, int*, MPI_Datatype*, MPI_Datatype*) ;

/** The type required for ADJOINT user-given reduction functions,
 * that are passed e.g. to BW_AMPI_Reduce in Tapenade-style diff AMPI code. */
typedef void (ADJ_userFunctionF) (void*, void*, void*, void*, int*, MPI_Datatype*, MPI_Datatype*) ;


#if defined(__cplusplus)
}
#endif

#endif

