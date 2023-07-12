/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/

/* The following to set the AMPI_FORTRANCOMPATIBLE
 * if it was placed in the configure command. */
#include "ampi/userIF/libConfig.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "ampi/adTool/support.h"
#include <mpi.h>
#include "ampi/ampi.h"
#include "ampi/libCommon/modified.h"

struct AMPI_ShadowComm_list {
  struct AMPI_ShadowComm_list *next_p;
  MPI_Comm comm ;
  MPI_Comm shadowComm ;
} ;

struct AMPI_ShadowComm_list * ADTOOL_AMPI_SHADOWCOMM_LIST = NULL ;

struct AMPI_Request_stack {
  struct AMPI_Request_stack *next_p;
  void *buf ;
  void *adjointBuf ;
  int count ;
  MPI_Datatype datatype ;
  int endPoint ;
  int tag ;
  enum AMPI_PairedWith_E pairedWith;
  MPI_Comm comm;
  enum AMPI_Activity_E isActive;
  enum AMPI_Request_origin_E origin;
} ;

int AMPI_Init_NT(int* argc, char*** argv) {
  int rc = MPI_Init(argc, argv);
  ADTOOL_AMPI_setupTypes() ;
  MPI_Comm worldDup ;
  int rc2 = MPI_Comm_dup(MPI_COMM_WORLD, &worldDup) ;
  assert(rc2==MPI_SUCCESS);
  ADTOOL_AMPI_SHADOWCOMM_LIST = NULL ;
  ADTOOL_AMPI_addShadowComm(MPI_COMM_WORLD, worldDup) ;
  ourADTOOL_AMPI_FPCollection.pushBcastInfo_fp=&ADTOOL_AMPI_pushBcastInfo;
  ourADTOOL_AMPI_FPCollection.popBcastInfo_fp=&ADTOOL_AMPI_popBcastInfo;
  ourADTOOL_AMPI_FPCollection.pushDoubleArray_fp=&ADTOOL_AMPI_pushDoubleArray;
  ourADTOOL_AMPI_FPCollection.popDoubleArray_fp=&ADTOOL_AMPI_popDoubleArray;
  ourADTOOL_AMPI_FPCollection.pushReduceInfo_fp=&ADTOOL_AMPI_pushReduceInfo; 
  ourADTOOL_AMPI_FPCollection.popReduceCountAndType_fp=&ADTOOL_AMPI_popReduceCountAndType;
  ourADTOOL_AMPI_FPCollection.popReduceInfo_fp=&ADTOOL_AMPI_popReduceInfo; 
  ourADTOOL_AMPI_FPCollection.pushSRinfo_fp=&ADTOOL_AMPI_pushSRinfo;
  ourADTOOL_AMPI_FPCollection.popSRinfo_fp=&ADTOOL_AMPI_popSRinfo;
  ourADTOOL_AMPI_FPCollection.pushGSinfo_fp=&ADTOOL_AMPI_pushGSinfo;
  ourADTOOL_AMPI_FPCollection.popGScommSizeForRootOrNull_fp=&ADTOOL_AMPI_popGScommSizeForRootOrNull;
  ourADTOOL_AMPI_FPCollection.popGSinfo_fp=&ADTOOL_AMPI_popGSinfo;
  ourADTOOL_AMPI_FPCollection.pushGSVinfo_fp=&ADTOOL_AMPI_pushGSVinfo;
  ourADTOOL_AMPI_FPCollection.popGSVinfo_fp=&ADTOOL_AMPI_popGSVinfo;
  ourADTOOL_AMPI_FPCollection.push_CallCode_fp=&ADTOOL_AMPI_push_CallCode;
  ourADTOOL_AMPI_FPCollection.pop_CallCode_fp=&ADTOOL_AMPI_pop_CallCode;
  ourADTOOL_AMPI_FPCollection.push_AMPI_Request_fp=&ADTOOL_AMPI_push_AMPI_Request;
  ourADTOOL_AMPI_FPCollection.pop_AMPI_Request_fp=&ADTOOL_AMPI_pop_AMPI_Request;
  ourADTOOL_AMPI_FPCollection.push_request_fp=&ADTOOL_AMPI_push_request;
  ourADTOOL_AMPI_FPCollection.pop_request_fp=&ADTOOL_AMPI_pop_request;
  ourADTOOL_AMPI_FPCollection.push_comm_fp=&ADTOOL_AMPI_push_comm;
  ourADTOOL_AMPI_FPCollection.pop_comm_fp=&ADTOOL_AMPI_pop_comm;
  ourADTOOL_AMPI_FPCollection.rawData_fp=&ADTOOL_AMPI_rawData;
  ourADTOOL_AMPI_FPCollection.rawDataV_fp=&ADTOOL_AMPI_rawDataV;
  ourADTOOL_AMPI_FPCollection.packDType_fp=&ADTOOL_AMPI_packDType;
  ourADTOOL_AMPI_FPCollection.unpackDType_fp=&ADTOOL_AMPI_unpackDType;
  ourADTOOL_AMPI_FPCollection.writeData_fp=&ADTOOL_AMPI_writeData;
  ourADTOOL_AMPI_FPCollection.writeDataV_fp=&ADTOOL_AMPI_writeDataV;
  ourADTOOL_AMPI_FPCollection.rawAdjointData_fp=&ADTOOL_AMPI_rawAdjointData;
  ourADTOOL_AMPI_FPCollection.Turn_fp=&ADTOOL_AMPI_Turn;
  ourADTOOL_AMPI_FPCollection.mapBufForAdjoint_fp=&ADTOOL_AMPI_mapBufForAdjoint;
  ourADTOOL_AMPI_FPCollection.setBufForAdjoint_fp=&ADTOOL_AMPI_setBufForAdjoint;
  ourADTOOL_AMPI_FPCollection.getAdjointCount_fp=&ADTOOL_AMPI_getAdjointCount;
  ourADTOOL_AMPI_FPCollection.setAdjointCount_fp=&ADTOOL_AMPI_setAdjointCount;
  ourADTOOL_AMPI_FPCollection.setAdjointCountAndTempBuf_fp=&ADTOOL_AMPI_setAdjointCountAndTempBuf;
  ourADTOOL_AMPI_FPCollection.allocateTempBuf_fp=&ADTOOL_AMPI_allocateTempBuf;
  ourADTOOL_AMPI_FPCollection.releaseAdjointTempBuf_fp=&ADTOOL_AMPI_releaseAdjointTempBuf;
  ourADTOOL_AMPI_FPCollection.adjointMultiply_fp=&ADTOOL_AMPI_adjointMultiply;
  ourADTOOL_AMPI_FPCollection.adjointMin_fp=&ADTOOL_AMPI_adjointMin;
  ourADTOOL_AMPI_FPCollection.adjointMax_fp=&ADTOOL_AMPI_adjointMax;
  ourADTOOL_AMPI_FPCollection.multiplyAdjoint_fp=&ADTOOL_AMPI_multiplyAdjoint;
  ourADTOOL_AMPI_FPCollection.divideAdjoint_fp=&ADTOOL_AMPI_divideAdjoint;
  ourADTOOL_AMPI_FPCollection.equalAdjoints_fp=&ADTOOL_AMPI_equalAdjoints;
  ourADTOOL_AMPI_FPCollection.incrementAdjoint_fp=&ADTOOL_AMPI_incrementAdjoint;
  ourADTOOL_AMPI_FPCollection.nullifyAdjoint_fp=&ADTOOL_AMPI_nullifyAdjoint;
  ourADTOOL_AMPI_FPCollection.setupTypes_fp=&ADTOOL_AMPI_setupTypes;
  ourADTOOL_AMPI_FPCollection.cleanupTypes_fp=&ADTOOL_AMPI_cleanupTypes;
  ourADTOOL_AMPI_FPCollection.FW_rawType_fp=&ADTOOL_AMPI_FW_rawType;
  ourADTOOL_AMPI_FPCollection.BW_rawType_fp=&ADTOOL_AMPI_BW_rawType;
  ourADTOOL_AMPI_FPCollection.isActiveType_fp=&ADTOOL_AMPI_isActiveType;
  ourADTOOL_AMPI_FPCollection.allocateTempActiveBuf_fp=&ADTOOL_AMPI_allocateTempActiveBuf;
  ourADTOOL_AMPI_FPCollection.releaseTempActiveBuf_fp=&ADTOOL_AMPI_releaseTempActiveBuf;
  ourADTOOL_AMPI_FPCollection.copyActiveBuf_fp=&ADTOOL_AMPI_copyActiveBuf;
  ourADTOOL_AMPI_FPCollection.tangentMultiply_fp=&ADTOOL_AMPI_tangentMultiply ;
  ourADTOOL_AMPI_FPCollection.tangentMin_fp=&ADTOOL_AMPI_tangentMin ;
  ourADTOOL_AMPI_FPCollection.tangentMax_fp=&ADTOOL_AMPI_tangentMax ;
  ourADTOOL_AMPI_FPCollection.pushBuffer_fp=&ADTOOL_AMPI_pushBuffer ;
  ourADTOOL_AMPI_FPCollection.popBuffer_fp=&ADTOOL_AMPI_popBuffer ;
  ourADTOOL_AMPI_FPCollection.addShadowComm_fp=&ADTOOL_AMPI_addShadowComm ;
  ourADTOOL_AMPI_FPCollection.getShadowComm_fp=&ADTOOL_AMPI_getShadowComm ;
  ourADTOOL_AMPI_FPCollection.delShadowComm_fp=&ADTOOL_AMPI_delShadowComm ;
#ifdef AMPI_FORTRANCOMPATIBLE
  ourADTOOL_AMPI_FPCollection.fortransetuptypes__fp=&adtool_ampi_fortransetuptypes_;
  ourADTOOL_AMPI_FPCollection.fortrancleanuptypes__fp=&adtool_ampi_fortrancleanuptypes_;
#endif
  return rc ;
}

static struct AMPI_Request_stack* requestStackTop=0 ;
void ADTOOL_AMPI_pushBcastInfo(void* buf,
			       int count,
			       MPI_Datatype datatype,
			       int root,
			       MPI_Comm comm) {
}

void ADTOOL_AMPI_popBcastInfo(void** buf,
			      int* count,
			      MPI_Datatype* datatype,
			      int* root,
			      MPI_Comm* comm,
			      void **idx) {
}

void ADTOOL_AMPI_pushDoubleArray(void* buf,
				 int count) {
}

void ADTOOL_AMPI_popDoubleArray(double* buf,
				int* count) {
}

void ADTOOL_AMPI_pushReduceInfo(void* sbuf,
				void* rbuf,
				void* resultData,
				int pushResultData, /* push resultData if true */
				int count,
				MPI_Datatype datatype,
				MPI_Op op,
				int root,
				MPI_Comm comm) {
}

void ADTOOL_AMPI_popReduceCountAndType(int* count,
				       MPI_Datatype* datatype) {
}

void ADTOOL_AMPI_popReduceInfo(void** sbuf,
			       void** rbuf,
			       void** prevData,
			       void** resultData,
			       int* count,
			       MPI_Op* op,
			       int* root,
			       MPI_Comm* comm,
			       void **idx) {
}

extern void pushInteger4Array(int *x, int n) ;
extern void popInteger4Array(int *x, int n) ;

void ADTOOL_AMPI_pushSRinfo(void* buf, 
			    int count,
			    MPI_Datatype datatype, 
			    int src, 
			    int tag,
			    AMPI_PairedWith pairedWith,
			    MPI_Comm comm) {
  /* [llh] TODO: this is not nice: we should call pushInteger4() instead ! */
  pushInteger4Array(&src,1) ;
  pushInteger4Array(&tag,1) ;
}

void ADTOOL_AMPI_popSRinfo(void** buf,
			   int* count,
			   MPI_Datatype* datatype, 
			   int* src, 
			   int* tag,
			   AMPI_PairedWith* pairedWith,
			   MPI_Comm* comm,
			   void **idx) { 
  /* [llh] TODO: this is not nice: we should call popInteger4() instead ! */
  popInteger4Array(tag,1) ;
  popInteger4Array(src,1) ;
}

void ADTOOL_AMPI_pushGSinfo(int commSizeForRootOrNull,
                            void *rbuf,
                            int rcnt,
                            MPI_Datatype rtype,
                            void *buf,
                            int count,
                            MPI_Datatype type,
                            int  root,
                            MPI_Comm comm) {
  pushInteger4Array(&commSizeForRootOrNull,1) ;
}

void ADTOOL_AMPI_popGScommSizeForRootOrNull(int *commSizeForRootOrNull) {
  popInteger4Array(commSizeForRootOrNull,1) ;
}

void ADTOOL_AMPI_popGSinfo(int commSizeForRootOrNull,
                           void **rbuf,
                           int *rcnt,
                           MPI_Datatype *rtype,
                           void **buf,
                           int *count,
                           MPI_Datatype *type,
                           int *root,
                           MPI_Comm *comm) {
}

void ADTOOL_AMPI_pushGSVinfo(int commSizeForRootOrNull,
                             void *rbuf,
                             int *rcnts,
                             int *displs,
                             MPI_Datatype rtype,
                             void *buf,
                             int  count,
                             MPI_Datatype type,
                             int  root,
                             MPI_Comm comm) {
  pushInteger4Array(&commSizeForRootOrNull,1) ;
}

void ADTOOL_AMPI_popGSVinfo(int commSizeForRootOrNull,
                            void **rbuf,
                            int *rcnts,
                            int *displs,
                            MPI_Datatype *rtype,
                            void **buf,
                            int *count,
                            MPI_Datatype *type,
                            int *root,
                            MPI_Comm *comm) {
}

void ADTOOL_AMPI_push_CallCode(enum AMPI_CallCode_E thisCall) { 
}

void ADTOOL_AMPI_pop_CallCode(enum AMPI_CallCode_E *thisCall) { 
}

void ADTOOL_AMPI_push_AMPI_Request(struct AMPI_Request_S  *ampiRequest) { 
  struct AMPI_Request_stack* newTop =
    (struct AMPI_Request_stack*)malloc(sizeof(struct AMPI_Request_stack)) ;
  newTop->next_p = requestStackTop ;
  newTop->buf = ampiRequest->buf ;
  newTop->adjointBuf = ampiRequest->adjointBuf ;
  newTop->count = ampiRequest->count ;
  newTop->datatype = ampiRequest->datatype ;
  newTop->endPoint = ampiRequest->endPoint ;
  newTop->tag = ampiRequest->tag ;
  newTop->pairedWith = ampiRequest->pairedWith ;
  newTop->comm = ampiRequest->comm ;
  newTop->origin = ampiRequest->origin ;
  requestStackTop = newTop ;
}

void ADTOOL_AMPI_pop_AMPI_Request(struct AMPI_Request_S  *ampiRequest) { 
  struct AMPI_Request_stack* oldTop = requestStackTop ;
  ampiRequest->buf = oldTop->buf ;
  ampiRequest->adjointBuf = oldTop->adjointBuf ;
  ampiRequest->count = oldTop->count ;
  ampiRequest->datatype = oldTop->datatype ;
  ampiRequest->endPoint = oldTop->endPoint ;
  ampiRequest->tag = oldTop->tag ;
  ampiRequest->pairedWith = oldTop->pairedWith ;
  ampiRequest->comm = oldTop->comm ;
  ampiRequest->origin = oldTop->origin ;
  requestStackTop = oldTop->next_p ;
  free(oldTop) ;
}

void ADTOOL_AMPI_push_request(MPI_Request request) { 
} 

MPI_Request ADTOOL_AMPI_pop_request() { 
  return 0;
}

void ADTOOL_AMPI_push_AMPI_WinRequest(AMPI_WinRequest *winRequest) {
}

void ADTOOL_AMPI_pop_AMPI_WinRequest(AMPI_WinRequest *winRequest) {
}

void ADTOOL_AMPI_push_comm(MPI_Comm comm) {
}

MPI_Comm ADTOOL_AMPI_pop_comm() {
  return 0;
}

/**
 * Register the info that the shadow communicator "dupComm"
 * has been created for the new communicator "comm"
 */
void ADTOOL_AMPI_addShadowComm(MPI_Comm comm, MPI_Comm dupComm) {
  struct AMPI_ShadowComm_list *newCell =
    (struct AMPI_ShadowComm_list *)malloc(sizeof(struct AMPI_ShadowComm_list)) ;
  newCell->next_p = ADTOOL_AMPI_SHADOWCOMM_LIST ;
  newCell->comm = comm;
  newCell->shadowComm = dupComm;
  ADTOOL_AMPI_SHADOWCOMM_LIST = newCell;
}

/**
 * Get the shadow communicator used to separate the communication graph of
 * (tangent-)diff variables from the communication graph of original variables
 */
MPI_Comm ADTOOL_AMPI_getShadowComm(MPI_Comm comm) {
  struct AMPI_ShadowComm_list * inShadowCommList = ADTOOL_AMPI_SHADOWCOMM_LIST ;
  while (inShadowCommList!=NULL && inShadowCommList->comm!=comm) {
    inShadowCommList = inShadowCommList->next_p ;
  }
  if (inShadowCommList) {
    return inShadowCommList->shadowComm ;
  } else {
    /* Nothing found about "comm": this is wrong!! fallback return comm */
    return comm ;
  }
}

/**
 * Removes the info about the shadow communicator associated to "comm".
 */
void ADTOOL_AMPI_delShadowComm(MPI_Comm comm) {
  struct AMPI_ShadowComm_list ** toinShadowCommList = &ADTOOL_AMPI_SHADOWCOMM_LIST ;
  while (*toinShadowCommList!=NULL && (*toinShadowCommList)->comm!=comm) {
    toinShadowCommList = &((*toinShadowCommList)->next_p) ;
  }
  if (*toinShadowCommList!=NULL) {
    struct AMPI_ShadowComm_list *cell = *toinShadowCommList ;
    toinShadowCommList = &(cell->next_p) ;
    free(cell);
  }
}

/** Returns the non-diff part of a communication buffer
 * passed to AMPI send or recv. For Tapenade, this is
 * the communication buffer itself (association by name) */
void* ADTOOL_AMPI_rawData(void* activeData, int *size) { 
  return activeData ;
}

/**
 * see \ref ADTOOL_AMPI_rawData
 */
void* ADTOOL_AMPI_rawDataV(void* activeData, int commSize,  int *counts, int* displs) {
  return activeData;
}

/**
 * returns contiguous data from indata
 */
void * ADTOOL_AMPI_packDType(void* indata, void* outdata, int count, int idx) {
  return indata;
}

/**
 * unpacks contiguous data back into structure
 */
void * ADTOOL_AMPI_unpackDType(void* indata, void* outdata, int count, int idx) {
  return indata;
}

/** Returns the diff part of the adjoint of a communication buffer
 * passed to AMPI send or recv. For Tapenade, this is
 * the adjoint communication buffer itself (association by name) */
void* ADTOOL_AMPI_rawAdjointData(void* activeData) { 
  return activeData ;
}

/** Remembers the association from a request <tt>ampiRequest</tt> to its
 * associated non-diff buffer <tt>buf</tt> */
void ADTOOL_AMPI_mapBufForAdjoint(struct AMPI_Request_S  *ampiRequest,
				  void* buf) { 
  ampiRequest->buf = buf ;
  ampiRequest->adjointBuf = NULL ;
}

/** Adds into the request-to-buffer association list the associated
 * adjoint buffer <tt>adjointBuf</tt> of non-diff buffer <tt>buf</tt>
 * This should be done upon turn from FW sweep to BW sweep. */
void ADTOOL_AMPI_Turn(void* buf, void* adjointBuf) {
  struct AMPI_Request_stack* inStack = requestStackTop ;
  while (inStack!=NULL) {
    if (inStack->buf==buf) {
      inStack->adjointBuf = adjointBuf ;
    }
    inStack = inStack->next_p ;
  }
}

/*[llh] not used ? redundant with mapBufForAdjoint? */
void ADTOOL_AMPI_setBufForAdjoint(struct AMPI_Request_S  *ampiRequest,
				  void* buf) { 
  /* an overloading tool would not do this but rather leave the buffer as traced 
     because the memory mapping happens already at FW time */
  ampiRequest->buf=buf;
}

void ADTOOL_AMPI_getAdjointCount(int *count,
				 MPI_Datatype datatype) {
  /* for now we keep the count as is but for example in vector mode one would have to multiply by vector length */
}

void ADTOOL_AMPI_setAdjointCount(struct AMPI_Request_S  *ampiRequest) { 
  ampiRequest->adjointCount=ampiRequest->count;
  ADTOOL_AMPI_getAdjointCount(&(ampiRequest->adjointCount),ampiRequest->datatype);
}

void ADTOOL_AMPI_setAdjointCountAndTempBuf(struct AMPI_Request_S *ampiRequest) { 
  ADTOOL_AMPI_setAdjointCount(ampiRequest);
  ampiRequest->adjointTempBuf =
    ADTOOL_AMPI_allocateTempBuf(ampiRequest->adjointCount,
                                ampiRequest->datatype,
                                ampiRequest->comm) ;
  assert(ampiRequest->adjointTempBuf);
}

void* ADTOOL_AMPI_allocateTempBuf(int adjointCount, MPI_Datatype datatype, MPI_Comm comm) {
  size_t s=0;
  int dt_idx = derivedTypeIdx(datatype);
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION)
    s=sizeof(double);
  else if (datatype==MPI_FLOAT || datatype==MPI_REAL)
    s=sizeof(float);
  else if (isDerivedType(dt_idx))
    s = getDTypeData()->p_extents[dt_idx];
  else
    MPI_Abort(comm, MPI_ERR_TYPE);
  return (void*)malloc(adjointCount*s);
}

void ADTOOL_AMPI_releaseAdjointTempBuf(void *tempBuf) { 
  free(tempBuf) ;
}

void* ADTOOL_AMPI_allocateTempActiveBuf(int count,
					MPI_Datatype datatype,
					MPI_Comm comm) {
/*   void* ptr = NULL; */
/*   if (datatype==MPI_DOUBLE) */
/*     ptr = malloc(count*sizeof(MPI_DOUBLE)); */
/*   else if (datatype==MPI_FLOAT) */
/*     ptr = malloc(count*sizeof(MPI_FLOAT)); */
  MPI_Aint lb, extent ;
  int rc = MPI_Type_get_extent(datatype, &lb, &extent) ;
  assert(rc==MPI_SUCCESS);
  void* ptr = NULL ;
  int size = ((char*)extent)-((char*)lb) ;
  ptr = malloc(count*size) ;
  assert(ptr);
  return ptr;
}

void ADTOOL_AMPI_releaseTempActiveBuf(void *buf,
				      int count,
				      MPI_Datatype datatype) {
  free(buf);
}

void *ADTOOL_AMPI_copyActiveBuf(void* source,
                                void* target,
                                int count,
                                MPI_Datatype datatype,
                                MPI_Comm comm) {
  MPI_Aint lb,extent ;
  int rc = MPI_Type_get_extent(datatype, &lb, &extent) ;
  assert(rc==MPI_SUCCESS);
  int size = extent - lb ;
  memcpy(target, source, count*size) ;
  return source;
}

/** This is the tangent of assignment target=source*target.
 * If targetd is NULL, does only the original assignment */
void ADTOOL_AMPI_tangentMultiply(int count, MPI_Datatype datatype, MPI_Comm comm,
                                 void *source, void *tangentSource,
                                 void* target, void* tangentTarget) {
  int i ;
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double* tgt = (double*)target ;
    double* tgtd = (double*)tangentTarget ;
    double* src = (double*)source ;
    double* srcd = (double*)tangentSource ;
    for (i=0 ; i<count ; ++i) {
      if (tgtd) tgtd[i] = tgtd[i]*src[i] + tgt[i]*srcd[i] ;
      tgt[i] = tgt[i]*src[i] ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float* tgt = (float*)target ;
    float* tgtd = (float*)tangentTarget ;
    float* src = (float*)source ;
    float* srcd = (float*)tangentSource ;
    for (i=0 ; i<count ; ++i) {
      if (tgtd) tgtd[i] = tgtd[i]*src[i] + tgt[i]*srcd[i] ;
      tgt[i] = tgt[i]*src[i] ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/** This is the tangent of assignment target=MIN(source,target).
 * If targetd is NULL, does only the original assignment */
void ADTOOL_AMPI_tangentMin(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void *source, void *tangentSource,
                            void* target, void* tangentTarget) {
  int i ;
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double* tgt = (double*)target ;
    double* tgtd = (double*)tangentTarget ;
    double* src = (double*)source ;
    double* srcd = (double*)tangentSource ;
    for (i=0 ; i<count ; ++i) {
      if (tgt[i] > src[i]) {
        if (tgtd) tgtd[i] = srcd[i] ;
        tgt[i] = src[i] ;
      }
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float* tgt = (float*)target ;
    float* tgtd = (float*)tangentTarget ;
    float* src = (float*)source ;
    float* srcd = (float*)tangentSource ;
    for (i=0 ; i<count ; ++i) {
      if (tgt[i] > src[i]) {
        if (tgtd) tgtd[i] = srcd[i] ;
        tgt[i] = src[i] ;
      }
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/** This is the tangent of assignment target=MAX(source,target).
 * If targetd is NULL, does only the original assignment */
void ADTOOL_AMPI_tangentMax(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void *source, void *tangentSource,
                            void* target, void* tangentTarget) {
  int i ;
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double* tgt = (double*)target ;
    double* tgtd = (double*)tangentTarget ;
    double* src = (double*)source ;
    double* srcd = (double*)tangentSource ;
    for (i=0 ; i<count ; ++i) {
      if (tgt[i] < src[i]) {
        if (tgtd) tgtd[i] = srcd[i] ;
        tgt[i] = src[i] ;
      }
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float* tgt = (float*)target ;
    float* tgtd = (float*)tangentTarget ;
    float* src = (float*)source ;
    float* srcd = (float*)tangentSource ;
    for (i=0 ; i<count ; ++i) {
      if (tgt[i] < src[i]) {
        if (tgtd) tgtd[i] = srcd[i] ;
        tgt[i] = src[i] ;
      }
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * This is the adjoint of assignment target=source*target
 */
void ADTOOL_AMPI_adjointMultiply(int count, MPI_Datatype datatype, MPI_Comm comm,
                                 void *source, void *adjointSource,
                                 void* target, void* adjointTarget) {
  int i ;
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double* tgt = (double*)target ;
    double* tgtb = (double*)adjointTarget ;
    double* src = (double*)source ;
    double* srcb = (double*)adjointSource ;
    for (i=0 ; i<count ; ++i) {
      srcb[i] += tgt[i]*tgtb[i] ;
      tgtb[i] *= src[i] ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float* tgt = (float*)target ;
    float* tgtb = (float*)adjointTarget ;
    float* src = (float*)source ;
    float* srcb = (float*)adjointSource ;
    for (i=0 ; i<count ; ++i) {
      srcb[i] += tgt[i]*tgtb[i] ;
      tgtb[i] *= src[i] ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * This is the adjoint of assignment target=MIN(source,target)
 */
void ADTOOL_AMPI_adjointMin(int count, MPI_Datatype datatype, MPI_Comm comm,
                                 void *source, void *adjointSource,
                                 void* target, void* adjointTarget) {
  int i ;
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double* tgt = (double*)target ;
    double* tgtb = (double*)adjointTarget ;
    double* src = (double*)source ;
    double* srcb = (double*)adjointSource ;
    for (i=0 ; i<count ; ++i) {
      if (src[i]<tgt[i]) {
        srcb[i] += tgtb[i] ;
        tgtb[i] = 0.0 ;
      }
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float* tgt = (float*)target ;
    float* tgtb = (float*)adjointTarget ;
    float* src = (float*)source ;
    float* srcb = (float*)adjointSource ;
    for (i=0 ; i<count ; ++i) {
      if (src[i]<tgt[i]) {
        srcb[i] += tgtb[i] ;
        tgtb[i] = 0.0 ;
      }
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * This is the adjoint of assignment target=MAX(source,target)
 */
void ADTOOL_AMPI_adjointMax(int count, MPI_Datatype datatype, MPI_Comm comm,
                                 void *source, void *adjointSource,
                                 void* target, void* adjointTarget) {
  int i ;
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double* tgt = (double*)target ;
    double* tgtb = (double*)adjointTarget ;
    double* src = (double*)source ;
    double* srcb = (double*)adjointSource ;
    for (i=0 ; i<count ; ++i) {
      if (src[i]>tgt[i]) {
        srcb[i] += tgtb[i] ;
        tgtb[i] = 0.0 ;
      }
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float* tgt = (float*)target ;
    float* tgtb = (float*)adjointTarget ;
    float* src = (float*)source ;
    float* srcb = (float*)adjointSource ;
    for (i=0 ; i<count ; ++i) {
      if (src[i]>tgt[i]) {
        srcb[i] += tgtb[i] ;
        tgtb[i] = 0.0 ;
      }
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * Multiply the given buffer target, which holds an adjoint, with the given source value
 */
void ADTOOL_AMPI_multiplyAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source, void *idx) { 
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double *vb = (double *)target ;
    double *nb = (double *)source ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *vb * (*nb) ;
      ++vb ;
      ++nb ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float *vb = (float *)target ;
    float *nb = (float *)source ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *vb * (*nb) ;
      ++vb ;
      ++nb ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * Divide the given buffer target, which holds an adjoint, with the given source value
 */
void ADTOOL_AMPI_divideAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source, void *idx) { 
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double *vb = (double *)target ;
    double *nb = (double *)source ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *vb / *nb ;
      ++vb ;
      ++nb ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float *vb = (float *)target ;
    float *nb = (float *)source ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *vb / *nb ;
      ++vb ;
      ++nb ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * Check equality of the given buffers source1 and source2, which hold adjoints,
 * and return the result in the given target buffer.
 */
void ADTOOL_AMPI_equalAdjoints(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source1, void *source2, void *idx) { 
  if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double *vb = (double *)target ;
    double *nb = (double *)source1 ;
    double *fb = (double *)source2 ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *nb == *fb ;
      ++vb ;
      ++nb ;
      ++fb ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float *vb = (float *)target ;
    float *nb = (float *)source1 ;
    float *fb = (float *)source2 ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *nb == *fb ;
      ++vb ;
      ++nb ;
      ++fb ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * Increment the given buffer "target", which holds an adjoint variable,
 * with the given additional adjoint value found in "source".
 */
void ADTOOL_AMPI_incrementAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm, void* target, void *source, void *idx) { 
  int dt_idx = derivedTypeIdx(datatype);
  if (isUserDefinedOp(dt_idx)) {
    derivedTypeData* dat = getDTypeData();
    MPI_Aint lb, extent ;
    MPI_Type_get_extent(datatype,&lb,&extent);
    MPI_Aint*   fieldOffsets = dat->arrays_of_displacements[dt_idx] ;
    int*   fieldBlocklengths = dat->arrays_of_blocklengths[dt_idx] ;
    MPI_Datatype* fieldTypes = dat->arrays_of_types[dt_idx] ;
    int nbfields = dat->counts[dt_idx] ;
    int i,j ;
    for (i=0 ; i<adjointCount ; ++i) {
      for (j=0 ; j<nbfields ; ++j) {
        ADTOOL_AMPI_incrementAdjoint(fieldBlocklengths[j], fieldTypes[j], comm,
                                     target+fieldOffsets[j], source+fieldOffsets[j], idx) ;
      }
      target += extent ;
      source += extent ;
    }
  } else if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double *vb = (double *)target ;
    double *nb = (double *)source ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *vb + *nb ;
      ++vb ;
      ++nb ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float *vb = (float *)target ;
    float *nb = (float *)source ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = *vb + *nb ;
      ++vb ;
      ++nb ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

/**
 * Reset to zero the given buffer "target", which holds an adjoint variable.
 */
void ADTOOL_AMPI_nullifyAdjoint(int adjointCount, MPI_Datatype datatype, MPI_Comm comm,
                                void* target) {
  int dt_idx = derivedTypeIdx(datatype);
  if (isUserDefinedOp(dt_idx)) {
    derivedTypeData* dat = getDTypeData();
    MPI_Aint lb, extent ;
    MPI_Type_get_extent(datatype,&lb,&extent);
    MPI_Aint*   fieldOffsets = dat->arrays_of_displacements[dt_idx] ;
    int*   fieldBlocklengths = dat->arrays_of_blocklengths[dt_idx] ;
    MPI_Datatype* fieldTypes = dat->arrays_of_types[dt_idx] ;
    int nbfields = dat->counts[dt_idx] ;
    int i,j ;
    for (i=0 ; i<adjointCount ; ++i) {
      for (j=0 ; j<nbfields ; ++j) {
        ADTOOL_AMPI_nullifyAdjoint(fieldBlocklengths[j], fieldTypes[j], comm,
                                   target+fieldOffsets[j]) ;
      }
      target += extent ;
    }
  } else if (datatype==MPI_DOUBLE || datatype==MPI_DOUBLE_PRECISION) {
    double *vb = (double *)target ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = 0.0 ;
      ++vb ;
    }
  } else if (datatype==MPI_FLOAT || datatype==MPI_REAL) {
    float *vb = (float *)target ;
    int i ;
    for (i=0 ; i<adjointCount ; ++i) {
      *vb = 0.0 ;
      ++vb ;
    }
  } else
    MPI_Abort(comm, MPI_ERR_TYPE);
}

extern void pushNArray(void *x, unsigned int nbChars) ;
extern void popNArray(void *x, unsigned int nbChars) ;

/**
 * Push the contents of buffer somewhere
 */
void ADTOOL_AMPI_pushBuffer(int count, MPI_Datatype datatype, MPI_Comm comm,
                            void* buffer) {
    MPI_Aint lb, extent ;
    MPI_Type_get_extent(datatype,&lb,&extent);
    int length = count*(int)extent ;
    pushNArray((char*)buffer, length) ;
}

/**
 * Pop the contents of buffer from somewhere
 */
void ADTOOL_AMPI_popBuffer(int count, MPI_Datatype datatype, MPI_Comm comm,
                           void* buffer) {
    MPI_Aint lb, extent ;
    MPI_Type_get_extent(datatype,&lb,&extent);
    int length = count*(int)extent ;
    popNArray((char*)buffer, length) ;
}

void ADTOOL_AMPI_writeData(void *buf,int *count) {}

void ADTOOL_AMPI_writeDataV(void* activeData, int *counts, int* displs) {}

void ADTOOL_AMPI_setupTypes() {
#ifdef AMPI_FORTRANCOMPATIBLE
  MPI_Fint adouble;
  MPI_Fint areal;
#endif
  /* Change AMPI_ADOUBLE to something else? Need AMPI_ADOUBLE!=MPI_DOUBLE for derived types. */
  AMPI_ADOUBLE=MPI_DOUBLE;
  AMPI_AFLOAT=MPI_FLOAT;
#ifdef AMPI_FORTRANCOMPATIBLE
  adtool_ampi_fortransetuptypes_(&adouble, &areal);
  AMPI_ADOUBLE_PRECISION=MPI_Type_f2c(adouble);
  AMPI_AREAL=MPI_Type_f2c(areal);
#endif
}

void ADTOOL_AMPI_cleanupTypes() {
#ifdef AMPI_FORTRANCOMPATIBLE
  MPI_Fint adouble=MPI_Type_c2f(AMPI_ADOUBLE_PRECISION);
  MPI_Fint areal=MPI_Type_c2f(AMPI_AREAL);
  adtool_ampi_fortrancleanuptypes_(&adouble, &areal);
#endif
}

MPI_Datatype ADTOOL_AMPI_FW_rawType(MPI_Datatype datatype) {
  int dt_idx = derivedTypeIdx(datatype);
  if (datatype==AMPI_ADOUBLE) return MPI_DOUBLE;
  else if (datatype==AMPI_AFLOAT) return MPI_FLOAT;
  else if (isDerivedType(dt_idx)) return getDTypeData()->packed_types[dt_idx];
  else return datatype;
}

MPI_Datatype ADTOOL_AMPI_BW_rawType(MPI_Datatype datatype) {
  int dt_idx = derivedTypeIdx(datatype);
  if (datatype==AMPI_ADOUBLE) return MPI_DOUBLE;
  else if (datatype==AMPI_AFLOAT) return MPI_FLOAT;
  else if (isDerivedType(dt_idx)) return MPI_DOUBLE;
  else return datatype;
}

AMPI_Activity ADTOOL_AMPI_isActiveType(MPI_Datatype datatype) {
  if (datatype==AMPI_ADOUBLE
      ||
      datatype==AMPI_AFLOAT
#ifdef AMPI_FORTRANCOMPATIBLE
      ||
      datatype==AMPI_ADOUBLE_PRECISION
      ||
      datatype==AMPI_AREAL
#endif
      ) return AMPI_ACTIVE;
  return AMPI_PASSIVE;
}

void *ADTOOL_AMPI_createWinMap(void *active_buf, MPI_Aint size){
  return NULL;
}

void ADTOOL_AMPI_writeWinData(void *map, void *buf, MPI_Aint size){
}

MPI_Aint ADTOOL_AMPI_getWinSize(MPI_Aint size) {
   return 0;
}
