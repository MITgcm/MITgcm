#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "admm.h"

/** TEMPORARY: FOR DEBUGGING AND STATISTICS. */
int traceADMM = 0 ; // if nonZero, addm will print trace messages on stdOut.
int numberOfRegisterings = 0 ;
int numberOfUnregisterings = 0 ;
int numberOfChunks = 0 ;
int peakNumberOfChunks = 0 ;
int numberOfRebases = 0 ;
int numberOfWaitings = 0 ;
int peakNumberOfWaitings = 0 ;
#include <time.h>
double timeSpentSearching = 0.0 ;
clock_t startSearchTime, endSearchTime ;
double timeSpentInADMM = 0.0 ;
clock_t startADMMTime, endADMMTime ;

/** Cell of a chained list of void* */
typedef struct _ADMM_List {
  void* head ;
  struct _ADMM_List* tail ;
} ADMM_List ;

typedef enum {BYBASE, BYOBASE, BYBASEB, BYOBASEB} ADMM_AddrSort ;

/** All info about one memory chunk */
typedef struct {
  // 0:base, 1:obase, 2:baseb, 3:obaseb
  void* bases[4] ;
  int   sizeInBytes ;
  int   sizeInBytesb ;
  int   nbElements ;
} ADMM_ChunkInfo ;

typedef struct {
  void** pp ;
  void** ppb ;
} ADMM_WaitingAddress ;

ADMM_List admm_chunksByBaseCell = {NULL, NULL} ;
ADMM_List admm_chunksByObaseCell = {NULL, NULL} ;
ADMM_List admm_chunksByBasebCell = {NULL, NULL} ;
ADMM_List admm_chunksByObasebCell = {NULL, NULL} ;
ADMM_List* admm_chunksLists[] =
  {&admm_chunksByBaseCell, &admm_chunksByObaseCell, &admm_chunksByBasebCell, &admm_chunksByObasebCell};

ADMM_List admm_waitingRebasesRoot = {NULL, NULL} ;
ADMM_List *admm_waitingRebases = &admm_waitingRebasesRoot ;


void ADMM_insertChunkInfo(ADMM_ChunkInfo* newChunkInfo, void* base, ADMM_AddrSort index) {
  ADMM_List *toList = admm_chunksLists[index] ;
  // search for the place where to insert the ADMM_ChunkInfo:
  while (toList->tail && ((ADMM_ChunkInfo*)toList->tail->head)->bases[index]<base) {
    toList = toList->tail ;
  }
  // insert at that place:
  ADMM_List* newCell = (ADMM_List*)malloc(sizeof(ADMM_List)) ;
  newCell->head = newChunkInfo ;
  newCell->tail = toList->tail ;
  toList->tail = newCell ;
}

ADMM_ChunkInfo* ADMM_searchBase(void* base, ADMM_AddrSort index) {
  ADMM_List *inList = admm_chunksLists[index]->tail ;
  ADMM_ChunkInfo* chunk ;
  // search for the ADMM_ChunkInfo about the chunk with the given base:
  while (inList) {
    chunk = (ADMM_ChunkInfo*)inList->head ;
    if (chunk->bases[index]==base) return chunk ;
    inList = inList->tail ;
  }
  return NULL ;
}

ADMM_ChunkInfo* ADMM_searchAddress(void* address, ADMM_AddrSort index) {
  ADMM_List *inList = admm_chunksLists[index]->tail ;
  ADMM_ChunkInfo* chunk ;
  // search for the ADMM_ChunkInfo that contains the address:
  while (inList) {
    chunk = (ADMM_ChunkInfo*)inList->head ;
    if (chunk->bases[index]<=address &&
        address<(chunk->bases[index]+(index<BYBASEB?chunk->sizeInBytes:chunk->sizeInBytesb)))
      return chunk ;
    inList = inList->tail ;
  }
  return NULL ;
}

void ADMM_removeChunkInfo(ADMM_ChunkInfo* chunkInfo, ADMM_AddrSort index) {
  ADMM_List *toList = admm_chunksLists[index] ;
  // search for the ADMM_ChunkInfo in the list:
  while (toList->tail) {
    if (toList->tail->head==chunkInfo) {
      ADMM_List* cell = toList->tail ;
      toList->tail = cell->tail ;
      free(cell) ;
      return ;
    }
    toList = toList->tail ;
  }
}

void ADMM_addWaitingRebase(void** pointer, void** pointerb) {
  // First, check if the same pointer(s) are maybe already waiting for a rebase,
  // (for another, previous address, and rebasing this address has never been possible).
  // In that case this waiting rebase can be reused as the new waiting rebase.
  ADMM_List* inWaitingRebases = admm_waitingRebases->tail ;
  ADMM_WaitingAddress* waitingAddress ;
  int found = 0 ;
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  while (!found && inWaitingRebases) {
    waitingAddress = (ADMM_WaitingAddress*)inWaitingRebases->head ;
    found = (waitingAddress->pp==pointer && waitingAddress->ppb==pointerb) ;
    inWaitingRebases = inWaitingRebases->tail ;
  }
#ifdef TIMESEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  // If these pointers were not already waiting, add them into the waiting list:
  if (!found) {
    ADMM_List *newCell = (ADMM_List*)malloc(sizeof(ADMM_List)) ;
    newCell->tail = admm_waitingRebases->tail ;
    newCell->head = (ADMM_WaitingAddress*)malloc(sizeof(ADMM_WaitingAddress)) ;
    ++numberOfWaitings ;
    if (peakNumberOfWaitings<numberOfWaitings) peakNumberOfWaitings = numberOfWaitings ;
    ((ADMM_WaitingAddress*)newCell->head)->pp = pointer ;
    ((ADMM_WaitingAddress*)newCell->head)->ppb = pointerb ;
    admm_waitingRebases->tail = newCell ;
  }
}

/** The given chunk provides for both the primal and its derivative.
 * The waitingAddress can expect for a primal or a derivative or for both. */
int ADMM_chunkInfoShadowedSolvesWaiting(ADMM_ChunkInfo* chunk, ADMM_WaitingAddress* waitingAddress) {
  int match = 0 ;
  if (waitingAddress->pp && *(waitingAddress->pp))
    match = (chunk->bases[BYOBASE]<=*(waitingAddress->pp)
             && *(waitingAddress->pp)<(chunk->bases[BYOBASE]+chunk->sizeInBytes)) ;
  else if (waitingAddress->ppb && *(waitingAddress->ppb))
    match = (chunk->bases[BYOBASEB]<=*(waitingAddress->ppb)
             && *(waitingAddress->ppb)<(chunk->bases[BYOBASEB]+chunk->sizeInBytesb)) ;
  if (match) {
    if (waitingAddress->pp && *(waitingAddress->pp))
      *(waitingAddress->pp) = chunk->bases[BYBASE]+(*(waitingAddress->pp)-chunk->bases[BYOBASE]) ;
    if (waitingAddress->ppb && *(waitingAddress->ppb))
      *(waitingAddress->ppb) = chunk->bases[BYBASEB]+(*(waitingAddress->ppb)-chunk->bases[BYOBASEB]) ;
    ++numberOfRebases ;
    return 1 ;
  } else
    return 0 ;
}

/** The given chunk is only for the primal.
 * The waitingAddress can only expect for a primal. If it expects a derivative -> error! */
int ADMM_chunkInfoSolvesWaiting(ADMM_ChunkInfo* chunk, ADMM_WaitingAddress* waitingAddress) {
  int match = (waitingAddress->pp && *(waitingAddress->pp)
               && chunk->bases[BYOBASE]<=*(waitingAddress->pp)
               && *(waitingAddress->pp)<(chunk->bases[BYOBASE]+chunk->sizeInBytes)) ;
  if (match) {
    *(waitingAddress->pp) = chunk->bases[BYBASE]+(*(waitingAddress->pp)-chunk->bases[BYOBASE]) ;
    ++numberOfRebases ;
    return 1 ;
  } else
    return 0 ;
}

/** Remove waiting addresses and addressesb that will never be solved
 * because they are contained in pointers that are beeing freed. */
void ADMM_cleanDeadWaitingsShadowed(void *base, int size, void *baseb, int sizeb) {
  ADMM_List* inWaitingRebases = admm_waitingRebases ;
  ADMM_List* waitingCell ;
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  while (inWaitingRebases->tail) {
    waitingCell = inWaitingRebases->tail ;
    if (
        (base <= (void*)((ADMM_WaitingAddress*)waitingCell->head)->pp &&
         (void*)((ADMM_WaitingAddress*)waitingCell->head)->pp < base+size)
        ||
        (baseb && ((ADMM_WaitingAddress*)waitingCell->head)->ppb &&
         baseb <= (void*)((ADMM_WaitingAddress*)waitingCell->head)->ppb &&
         (void*)((ADMM_WaitingAddress*)waitingCell->head)->ppb < baseb+sizeb)
        ) {
      if (traceADMM) printf(" clean waiting [%li]->%li Shadow:[%li]->%li\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
      inWaitingRebases->tail = inWaitingRebases->tail->tail ;
      free(waitingCell->head) ;
      free(waitingCell) ;
      --numberOfWaitings ;
    } else
      inWaitingRebases = inWaitingRebases->tail ;
  }
#ifdef TIMESEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_registerShadowed(void *base, void *obase, int sizeInBytes, void *baseb, void *obaseb, int sizeInBytesb, int nbElements
#ifdef WITH_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef TIMEADMM
  startADMMTime = clock() ;
#endif
  ADMM_ChunkInfo* newChunkInfo = (ADMM_ChunkInfo*)malloc(sizeof(ADMM_ChunkInfo)) ;
  ++numberOfChunks ;
  if (numberOfChunks>peakNumberOfChunks) peakNumberOfChunks = numberOfChunks ;
  ++numberOfRegisterings ;
  if (traceADMM) printf("ADMM_register (old:%li=>)%li+%i    Shadow: (old:%li=>)%li+%i \"%s\"\n", obase, base, sizeInBytes, obaseb, baseb, sizeInBytesb, label);
  if (!obase) obase = base ;
  if (!obaseb) obaseb = baseb ;
  newChunkInfo->bases[0] = base ;
  newChunkInfo->bases[1] = obase ;
  newChunkInfo->bases[2] = baseb ;
  newChunkInfo->bases[3] = obaseb ;
  newChunkInfo->sizeInBytes = sizeInBytes ;
  newChunkInfo->sizeInBytesb = sizeInBytesb ;
  newChunkInfo->nbElements = nbElements ;
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  ADMM_insertChunkInfo(newChunkInfo, base, BYBASE) ;
  ADMM_insertChunkInfo(newChunkInfo, obase, BYOBASE) ;
  ADMM_insertChunkInfo(newChunkInfo, baseb, BYBASEB) ;
  ADMM_insertChunkInfo(newChunkInfo, obaseb, BYOBASEB) ;
  /* Now solve waiting pointers and pointerbs with newChunkInfo: */
  ADMM_List* inWaitingRebases = admm_waitingRebases ;
  ADMM_List* waitingCell ;
  while (inWaitingRebases->tail) {
    waitingCell = inWaitingRebases->tail ;
    if (traceADMM) printf("  retry rebase [%li]->%li Shadow:[%li]->%li ?", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
    if (ADMM_chunkInfoShadowedSolvesWaiting(newChunkInfo, (ADMM_WaitingAddress*)waitingCell->head)) {
      if (traceADMM) printf(" yes => [%li]->%li Shadow:[%li]->%li !\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
      inWaitingRebases->tail = inWaitingRebases->tail->tail ;
      free(waitingCell->head) ;
      free(waitingCell) ;
      --numberOfWaitings ;
    } else {
      if (traceADMM) printf(" no!\n") ;
      inWaitingRebases = inWaitingRebases->tail ;
    }
  }
#ifdef TIMESEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
#ifdef TIMEADMM
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_register(void *base, void *obase, int sizeInBytes, int nbElements
#ifdef WITH_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef TIMEADMM
  startADMMTime = clock() ;
#endif
  ADMM_ChunkInfo* newChunkInfo = (ADMM_ChunkInfo*)malloc(sizeof(ADMM_ChunkInfo)) ;
  ++numberOfChunks ;
  if (numberOfChunks>peakNumberOfChunks) peakNumberOfChunks = numberOfChunks ;
  ++numberOfRegisterings ;
  if (traceADMM) printf("ADMM_register (old:%li=>)%li+%i \"%s\"\n", obase, base, sizeInBytes, label);
  if (!obase) obase = base ;
  newChunkInfo->bases[0] = base ;
  newChunkInfo->bases[1] = obase ;
  newChunkInfo->bases[2] = NULL ;
  newChunkInfo->bases[3] = NULL ;
  newChunkInfo->sizeInBytes = sizeInBytes ;
  newChunkInfo->sizeInBytesb = 0 ;
  newChunkInfo->nbElements = nbElements ;
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  ADMM_insertChunkInfo(newChunkInfo, base, BYBASE) ;
  ADMM_insertChunkInfo(newChunkInfo, obase, BYOBASE) ;
  /* Now solve waiting pointers with newChunkInfo:
   * NOTE: we are in the no-shadow Register case, which means that this
   * memory chunk doesn't need a derivative chunk, and therefore we assume
   * that there cannot be a shadowed waiting address waiting for this chunk. */
  ADMM_List* inWaitingRebases = admm_waitingRebases ;
  ADMM_List* waitingCell ;
  while (inWaitingRebases->tail) {
    waitingCell = inWaitingRebases->tail ;
    if (traceADMM) printf("  retry rebase [%li]->%li ?", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL)) ;
    if (ADMM_chunkInfoSolvesWaiting(newChunkInfo, (ADMM_WaitingAddress*)waitingCell->head)) {
      if (traceADMM) printf(" yes => [%li]->%li !\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL)) ;
      inWaitingRebases->tail = inWaitingRebases->tail->tail ;
      free(waitingCell->head) ;
      free(waitingCell) ;
      --numberOfWaitings ;
    } else {
      if (traceADMM) printf(" no!\n") ;
      inWaitingRebases = inWaitingRebases->tail ;
    }
  }
#ifdef TIMESEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
#ifdef TIMEADMM
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_unregisterShadowed(void *base, void *baseb, int *nbElements
#ifdef WITH_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef TIMEADMM
  startADMMTime = clock() ;
#endif
  if (traceADMM) printf("ADMM_unregister %li   Shadow: %li \"%s\"\n", base, baseb, label);
  ADMM_ChunkInfo* foundChunkInfo = NULL ;
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  if (baseb)
    foundChunkInfo = ADMM_searchBase(baseb, BYBASEB) ;
  if (!foundChunkInfo)
    foundChunkInfo = ADMM_searchBase(base, BYBASE) ;
#ifdef TIMESEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  if (foundChunkInfo) {
    *nbElements = foundChunkInfo->nbElements ;
#ifdef TIMESEARCHING
    startSearchTime = clock() ;
#endif
    ADMM_removeChunkInfo(foundChunkInfo, BYBASE) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYOBASE) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYBASEB) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYOBASEB) ;
#ifdef TIMESEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
    ADMM_cleanDeadWaitingsShadowed(base, foundChunkInfo->sizeInBytes, baseb, foundChunkInfo->sizeInBytesb) ;
    free(foundChunkInfo) ;
    ++numberOfUnregisterings ;
    --numberOfChunks ;
  }
#ifdef TIMEADMM
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_unregister(void *base, int *nbElements
#ifdef WITH_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef TIMEADMM
  startADMMTime = clock() ;
#endif
  if (traceADMM) printf("ADMM_unregister %li \"%s\"\n", base, label);
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  ADMM_ChunkInfo* foundChunkInfo = ADMM_searchBase(base, BYBASE) ;
#ifdef TIMESEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  if (foundChunkInfo) {
    *nbElements = foundChunkInfo->nbElements ;
#ifdef TIMESEARCHING
    startSearchTime = clock() ;
#endif
    ADMM_removeChunkInfo(foundChunkInfo, BYBASE) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYOBASE) ;
#ifdef TIMESEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
    ADMM_cleanDeadWaitingsShadowed(base, foundChunkInfo->sizeInBytes, NULL, 0) ;
    free(foundChunkInfo) ;
    ++numberOfUnregisterings ;
    --numberOfChunks ;
  }
#ifdef TIMEADMM
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_rebaseShadowed(void** pointer, void** pointerb
#ifdef WITH_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef TIMEADMM
  startADMMTime = clock() ;
#endif
  if (traceADMM) printf("   ADMM_rebase [%li]->%li Shadow:[%li]->%li \"%s\" ?", pointer, (pointer?*pointer:NULL), pointerb, (pointerb?*pointerb:NULL), label) ;
  ADMM_ChunkInfo* foundChunkInfo = NULL ;
#ifdef TIMESEARCHING
  startSearchTime = clock() ;
#endif
  if (*pointerb)
    foundChunkInfo = ADMM_searchAddress(*pointerb, BYOBASEB) ;
  if (!foundChunkInfo && pointer && *pointer)
    foundChunkInfo = ADMM_searchAddress(*pointer, BYOBASE) ;
#ifdef TIMESEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  if (foundChunkInfo) {
    if (pointer && *pointer)
      *pointer = foundChunkInfo->bases[BYBASE]+(*pointer-foundChunkInfo->bases[BYOBASE]) ;
    if (*pointerb)
      *pointerb = foundChunkInfo->bases[BYBASEB]+(*pointerb-foundChunkInfo->bases[BYOBASEB]) ;
    if (traceADMM) printf(" yes => [%li]->%li Shadow:[%li]->%li !\n", pointer, (pointer?*pointer:NULL), pointerb, (pointerb?*pointerb:NULL)) ;
  } else if (*pointer || *pointerb) {
    ADMM_addWaitingRebase(pointer, pointerb) ;
    if (traceADMM) printf(" no!\n") ;
  } else {
    if (traceADMM) printf(".\n") ;
  }
  ++numberOfRebases ;
#ifdef TIMEADMM
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}


void ADMM_rebase(void** pointer
#ifdef WITH_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef TIMEADMM
  startADMMTime = clock() ;
#endif
  if (traceADMM) printf("   ADMM_rebase [%li]->%li \"%s\" ?", pointer, (pointer?*pointer:NULL), label) ;
  ADMM_ChunkInfo* foundChunkInfo = NULL ;
  if (*pointer) {
#ifdef TIMESEARCHING
    startSearchTime = clock() ;
#endif
    foundChunkInfo = ADMM_searchAddress(*pointer, BYOBASE) ;
#ifdef TIMESEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
    if (foundChunkInfo) {
      *pointer = foundChunkInfo->bases[BYBASE]+(*pointer-foundChunkInfo->bases[BYOBASE]) ;
      if (traceADMM) printf(" yes => [%li]->%li !\n", pointer, (pointer?*pointer:NULL)) ;
    } else {
      ADMM_addWaitingRebase(pointer, NULL) ;
      if (traceADMM) printf(" no!\n") ;
    }
  } else {
    if (traceADMM) printf(".\n") ;
  }
  ++numberOfRebases ;
#ifdef TIMEADMM
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_showChunks() {
  ADMM_List* inChunksByBaseCell = admm_chunksByBaseCell.tail ;
  ADMM_ChunkInfo* chunkInfo ;
  printf("CHUNKS LIST:\n") ;
  while (inChunksByBaseCell) {
    chunkInfo = ((ADMM_ChunkInfo*)inChunksByBaseCell->head) ;
    printf("  (old:%li=>)%li+%i    Shadow: (old:%li=>)%li+%i (%i elements)\n",
           chunkInfo->bases[BYOBASE], chunkInfo->bases[BYBASE], chunkInfo->sizeInBytes,
           chunkInfo->bases[BYOBASEB], chunkInfo->bases[BYBASEB], chunkInfo->sizeInBytesb,
           chunkInfo->nbElements);
    inChunksByBaseCell = inChunksByBaseCell->tail ;
  }
}

void ADMM_showWaitings() {
  ADMM_List* inWaitingRebases = admm_waitingRebases->tail ;
  ADMM_WaitingAddress* waitingAddress ;
  printf("WAITING LIST:\n") ;
  while (inWaitingRebases) {
    waitingAddress = (ADMM_WaitingAddress*)inWaitingRebases->head ;
    printf(" [%li]->%li Shadow:[%li]->%li   %x %x \n",
           waitingAddress->pp, (waitingAddress->pp?*(waitingAddress->pp):NULL),
           waitingAddress->ppb, (waitingAddress->ppb?*(waitingAddress->ppb):NULL),
           waitingAddress->pp, waitingAddress->ppb);
    inWaitingRebases = inWaitingRebases->tail ;
  }
  printf("\n");
}

void ADMM_statistics() {
#ifdef TIMEADMM
  printf("Time spent in ADMM:%lf\n", timeSpentInADMM) ;
#endif
#ifdef TIMESEARCHING
  printf("Time spent searching:%lf\n", timeSpentSearching) ;
#endif
  printf("Registerings:%i, Unregisterings:%i, numberOfRebases:%i\n",
         numberOfRegisterings, numberOfUnregisterings, numberOfRebases) ;
  printf("chunks:%i[peak:%i] waiting:%i[peak:%i]\n",
         numberOfChunks, peakNumberOfChunks, numberOfWaitings, peakNumberOfWaitings);
  ADMM_showChunks() ;
  ADMM_showWaitings() ;
}
