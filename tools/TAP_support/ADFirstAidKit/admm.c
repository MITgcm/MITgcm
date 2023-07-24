/*
 * TAPENADE Automatic Differentiation Engine
 * Copyright (C) 1999-2021 Inria
 * See the LICENSE.md file in the project root for more information.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "admm.h"

#include <time.h>

/* DEBUGGING AND STATISTICS:
 * -------------------------
 * Compile with -D ADMM_TRACE          to log calls to ADMM
 * Compile with -D ADMM_TIME           to show the total time spent in ADMM
 * Compile with -D ADMM_TIME_SEARCHING to show the total time spent searching in the ADMM table
 * Compile with -D ADMM_COUNTS         to show the numbers of ADMM operations and objects
 * Compile with -D ADMM_LABELS         to add a last label argument  to ADMM calls
 */

#ifdef ADMM_COUNTS
int numberOfRegisterings = 0 ;
int numberOfUnregisterings = 0 ;
int numberOfChunks = 0 ;
int peakNumberOfChunks = 0 ;
int numberOfRebases = 0 ;
int numberOfWaitings = 0 ;
int peakNumberOfWaitings = 0 ;
#endif

#ifdef ADMM_TIME
double timeSpentInADMM = 0.0 ;
clock_t startADMMTime, endADMMTime ;
#endif

#ifdef ADMM_TIME_SEARCHING
double timeSpentSearching = 0.0 ;
clock_t startSearchTime, endSearchTime ;
#endif

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

void ADMM_findRanksInWaitings(void** pointer, void** pointerb, long* indexInWait, long* numberInWait) {
  // Just for tracing ADMM, returns the length of the waitinglist and the rank in it of pointer/pointerb.
  ADMM_List* inWaitingRebases = admm_waitingRebases->tail ;
  ADMM_WaitingAddress* waitingAddress ;
  *indexInWait = 0 ;
  *numberInWait = 0 ;
  while (inWaitingRebases) {
    waitingAddress = (ADMM_WaitingAddress*)inWaitingRebases->head ;
    ++(*numberInWait);
    if (waitingAddress->pp==pointer && waitingAddress->ppb==pointerb) *indexInWait = *numberInWait ;
    inWaitingRebases = inWaitingRebases->tail ;
  }
}

void ADMM_addWaitingRebase(void** pointer, void** pointerb) {
  // First, check if the same pointer(s) are maybe already waiting for a rebase,
  // (for another, previous address, and rebasing this address has never been possible).
  // In that case this waiting rebase can be reused as the new waiting rebase.
  ADMM_List* inWaitingRebases = admm_waitingRebases->tail ;
  ADMM_WaitingAddress* waitingAddress ;
  int found = 0 ;
#ifdef ADMM_TIME_SEARCHING
  startSearchTime = clock() ;
#endif
  while (!found && inWaitingRebases) {
    waitingAddress = (ADMM_WaitingAddress*)inWaitingRebases->head ;
    found = (waitingAddress->pp==pointer && waitingAddress->ppb==pointerb) ;
    inWaitingRebases = inWaitingRebases->tail ;
  }
#ifdef ADMM_TIME_SEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  // If these pointers were not already waiting, add them into the waiting list:
  if (!found) {
    ADMM_List *newCell = (ADMM_List*)malloc(sizeof(ADMM_List)) ;
    newCell->tail = admm_waitingRebases->tail ;
    newCell->head = (ADMM_WaitingAddress*)malloc(sizeof(ADMM_WaitingAddress)) ;
#ifdef ADMM_COUNTS
    ++numberOfWaitings ;
    if (peakNumberOfWaitings<numberOfWaitings) peakNumberOfWaitings = numberOfWaitings ;
#endif
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
#ifdef ADMM_COUNTS
    ++numberOfRebases ;
#endif
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
#ifdef ADMM_COUNTS
    ++numberOfRebases ;
#endif
    return 1 ;
  } else
    return 0 ;
}

/** Remove waiting addresses and addressesb that will never be solved
 * because they are contained in pointers that are beeing freed. */
void ADMM_cleanDeadWaitingsShadowed(void *base, int size, void *baseb, int sizeb) {
  ADMM_List* inWaitingRebases = admm_waitingRebases ;
  ADMM_List* waitingCell ;
#ifdef ADMM_TIME_SEARCHING
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
#ifdef ADMM_TRACE
      printf(" clean waiting [%li]->%li Shadow:[%li]->%li\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
      fflush(stdout);
#endif
      inWaitingRebases->tail = inWaitingRebases->tail->tail ;
      free(waitingCell->head) ;
      free(waitingCell) ;
#ifdef ADMM_COUNTS
      --numberOfWaitings ;
#endif
    } else
      inWaitingRebases = inWaitingRebases->tail ;
  }
#ifdef ADMM_TIME_SEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_registerShadowed(void *base, void *obase, int sizeInBytes, void *baseb, void **alreadyRebasedb, void *obaseb, int sizeInBytesb, int nbElements
#ifdef ADMM_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef ADMM_TIME
  startADMMTime = clock() ;
#endif
  int rebasePhase = (obaseb!=0) ;
  ADMM_ChunkInfo* newChunkInfo = (ADMM_ChunkInfo*)malloc(sizeof(ADMM_ChunkInfo)) ;
#ifdef ADMM_COUNTS
  ++numberOfChunks ;
  if (numberOfChunks>peakNumberOfChunks) peakNumberOfChunks = numberOfChunks ;
  ++numberOfRegisterings ;
#endif
#ifdef ADMM_TRACE
  if (rebasePhase)
    printf("ADMM_register (old:%li=>)%li+%i    Shadow: (old:%li=>)%li+%i \"%s\"\n", obase, base, sizeInBytes, obaseb, baseb, sizeInBytesb, label);
  else
    printf("ADMM_register %li+%i    Shadow: %li+%i \"%s\"\n", base, sizeInBytes, baseb, sizeInBytesb, label);
  fflush(stdout);
#endif
  if (!rebasePhase) {obase = base; obaseb = baseb;}
  newChunkInfo->bases[0] = base ;
  newChunkInfo->bases[1] = obase ;
  newChunkInfo->bases[2] = baseb ;
  newChunkInfo->bases[3] = obaseb ;
  newChunkInfo->sizeInBytes = sizeInBytes ;
  newChunkInfo->sizeInBytesb = sizeInBytesb ;
  newChunkInfo->nbElements = nbElements ;
#ifdef ADMM_TIME_SEARCHING
  startSearchTime = clock() ;
#endif
  ADMM_insertChunkInfo(newChunkInfo, base, BYBASE) ;
  ADMM_insertChunkInfo(newChunkInfo, obase, BYOBASE) ;
  ADMM_insertChunkInfo(newChunkInfo, baseb, BYBASEB) ;
  ADMM_insertChunkInfo(newChunkInfo, obaseb, BYOBASEB) ;
  if (rebasePhase) {
    /* Now solve waiting pointers and pointerbs with newChunkInfo: */
    ADMM_List* inWaitingRebases = admm_waitingRebases ;
    ADMM_List* waitingCell ;
    while (inWaitingRebases->tail) {
      waitingCell = inWaitingRebases->tail ;
#ifdef ADMM_TRACE
      printf("   ...retry rebase [%li]->%li Shadow:[%li]->%li ?", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
      fflush(stdout);
#endif
      if (alreadyRebasedb==((ADMM_WaitingAddress*)waitingCell->head)->ppb) {
#ifdef ADMM_TRACE
        printf(" now done => [%li]->%li Shadow:[%li]->%li !\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
        fflush(stdout);
#endif
        inWaitingRebases->tail = inWaitingRebases->tail->tail ;
        free(waitingCell->head) ;
        free(waitingCell) ;
      } else if (ADMM_chunkInfoShadowedSolvesWaiting(newChunkInfo, (ADMM_WaitingAddress*)waitingCell->head)) {
#ifdef ADMM_TRACE
        printf(" yes => [%li]->%li Shadow:[%li]->%li !\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL), ((ADMM_WaitingAddress*)waitingCell->head)->ppb, (((ADMM_WaitingAddress*)waitingCell->head)->ppb?*(((ADMM_WaitingAddress*)waitingCell->head)->ppb):NULL)) ;
        fflush(stdout);
#endif
        inWaitingRebases->tail = inWaitingRebases->tail->tail ;
        free(waitingCell->head) ;
        free(waitingCell) ;
#ifdef ADMM_COUNTS
        --numberOfWaitings ;
#endif
      } else {
#ifdef ADMM_TRACE
        printf(" no, try again later!\n") ;
        fflush(stdout);
#endif
        inWaitingRebases = inWaitingRebases->tail ;
      }
    }
  }
#ifdef ADMM_TIME_SEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
#ifdef ADMM_TIME
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_register(void *base, void **alreadyRebased, void *obase, int sizeInBytes, int nbElements
#ifdef ADMM_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef ADMM_TIME
  startADMMTime = clock() ;
#endif
  int rebasePhase = (obase!=0) ;
  ADMM_ChunkInfo* newChunkInfo = (ADMM_ChunkInfo*)malloc(sizeof(ADMM_ChunkInfo)) ;
#ifdef ADMM_COUNTS
  ++numberOfChunks ;
  if (numberOfChunks>peakNumberOfChunks) peakNumberOfChunks = numberOfChunks ;
  ++numberOfRegisterings ;
#endif
#ifdef ADMM_TRACE
  if (rebasePhase)
    printf("ADMM_register (old:%li=>)%li+%i \"%s\"\n", obase, base, sizeInBytes, label);
  else
    printf("ADMM_register %li+%i \"%s\"\n", base, sizeInBytes, label);
  fflush(stdout);
#endif
  if (!rebasePhase) obase = base ;
  newChunkInfo->bases[0] = base ;
  newChunkInfo->bases[1] = obase ;
  newChunkInfo->bases[2] = NULL ;
  newChunkInfo->bases[3] = NULL ;
  newChunkInfo->sizeInBytes = sizeInBytes ;
  newChunkInfo->sizeInBytesb = 0 ;
  newChunkInfo->nbElements = nbElements ;
#ifdef ADMM_TIME_SEARCHING
  startSearchTime = clock() ;
#endif
  ADMM_insertChunkInfo(newChunkInfo, base, BYBASE) ;
  ADMM_insertChunkInfo(newChunkInfo, obase, BYOBASE) ;
  if (rebasePhase) {
    /* Now solve waiting pointers with newChunkInfo:
     * NOTE: we are in the no-shadow Register case, which means that this
     * memory chunk doesn't need a derivative chunk, and therefore we assume
     * that there cannot be a shadowed waiting address waiting for this chunk. */
    ADMM_List* inWaitingRebases = admm_waitingRebases ;
    ADMM_List* waitingCell ;
    while (inWaitingRebases->tail) {
      waitingCell = inWaitingRebases->tail ;
#ifdef ADMM_TRACE
      printf("   ...retry rebase [%li]->%li ?", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL)) ;
      fflush(stdout);
#endif
      if (alreadyRebased==((ADMM_WaitingAddress*)waitingCell->head)->pp) {
#ifdef ADMM_TRACE
        printf(" now done => [%li]->%li !\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL)) ;
        fflush(stdout);
#endif
        inWaitingRebases->tail = inWaitingRebases->tail->tail ;
        free(waitingCell->head) ;
        free(waitingCell) ;
      } else if (ADMM_chunkInfoSolvesWaiting(newChunkInfo, (ADMM_WaitingAddress*)waitingCell->head)) {
#ifdef ADMM_TRACE
        printf(" yes => [%li]->%li !\n", ((ADMM_WaitingAddress*)waitingCell->head)->pp, (((ADMM_WaitingAddress*)waitingCell->head)->pp?*(((ADMM_WaitingAddress*)waitingCell->head)->pp):NULL)) ;
        fflush(stdout);
#endif
        inWaitingRebases->tail = inWaitingRebases->tail->tail ;
        free(waitingCell->head) ;
        free(waitingCell) ;
#ifdef ADMM_COUNTS
        --numberOfWaitings ;
#endif
      } else {
#ifdef ADMM_TRACE
        printf(" no, try again later!\n") ;
        fflush(stdout);
#endif
        inWaitingRebases = inWaitingRebases->tail ;
      }
    }
  }
#ifdef ADMM_TIME_SEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
#ifdef ADMM_TIME
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_unregisterShadowed(void *base, void *baseb, int *nbElements
#ifdef ADMM_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef ADMM_TIME
  startADMMTime = clock() ;
#endif
#ifdef ADMM_TRACE
  printf("ADMM_unregister %li   Shadow: %li \"%s\"\n", base, baseb, label);
  fflush(stdout);
#endif
  ADMM_ChunkInfo* foundChunkInfo = NULL ;
#ifdef ADMM_TIME_SEARCHING
  startSearchTime = clock() ;
#endif
  if (baseb)
    foundChunkInfo = ADMM_searchBase(baseb, BYBASEB) ;
  if (!foundChunkInfo)
    foundChunkInfo = ADMM_searchBase(base, BYBASE) ;
#ifdef ADMM_TIME_SEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  if (foundChunkInfo) {
    *nbElements = foundChunkInfo->nbElements ;
#ifdef ADMM_TIME_SEARCHING
    startSearchTime = clock() ;
#endif
    ADMM_removeChunkInfo(foundChunkInfo, BYBASE) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYOBASE) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYBASEB) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYOBASEB) ;
#ifdef ADMM_TIME_SEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
    ADMM_cleanDeadWaitingsShadowed(base, foundChunkInfo->sizeInBytes, baseb, foundChunkInfo->sizeInBytesb) ;
    free(foundChunkInfo) ;
#ifdef ADMM_COUNTS
    ++numberOfUnregisterings ;
    --numberOfChunks ;
#endif
  }
#ifdef ADMM_TIME
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_unregister(void *base, int *nbElements
#ifdef ADMM_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef ADMM_TIME
  startADMMTime = clock() ;
#endif
#ifdef ADMM_TRACE
  printf("ADMM_unregister %li \"%s\"\n", base, label);
  fflush(stdout);
#endif
#ifdef ADMM_TIME_SEARCHING
  startSearchTime = clock() ;
#endif
  ADMM_ChunkInfo* foundChunkInfo = ADMM_searchBase(base, BYBASE) ;
#ifdef ADMM_TIME_SEARCHING
  endSearchTime = clock() ;
  timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  if (foundChunkInfo) {
    *nbElements = foundChunkInfo->nbElements ;
#ifdef ADMM_TIME_SEARCHING
    startSearchTime = clock() ;
#endif
    ADMM_removeChunkInfo(foundChunkInfo, BYBASE) ;
    ADMM_removeChunkInfo(foundChunkInfo, BYOBASE) ;
#ifdef ADMM_TIME_SEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
    ADMM_cleanDeadWaitingsShadowed(base, foundChunkInfo->sizeInBytes, NULL, 0) ;
    free(foundChunkInfo) ;
#ifdef ADMM_COUNTS
    ++numberOfUnregisterings ;
    --numberOfChunks ;
#endif
  }
#ifdef ADMM_TIME
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

void ADMM_rebaseShadowed(void** pointer, void** pointerb
#ifdef ADMM_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef ADMM_TIME
  startADMMTime = clock() ;
#endif
#ifdef ADMM_TRACE
  printf("   ADMM_rebase [%li]->%li Shadow:[%li]->%li \"%s\" ?", pointer, (pointer?*pointer:NULL), pointerb, (pointerb?*pointerb:NULL), label) ;
  fflush(stdout);
#endif
  ADMM_ChunkInfo* foundChunkInfo = NULL ;
#ifdef ADMM_TIME_SEARCHING
  startSearchTime = clock() ;
#endif
  if (*pointerb)
    foundChunkInfo = ADMM_searchAddress(*pointerb, BYOBASEB) ;
  if (!foundChunkInfo && pointer && *pointer)
    foundChunkInfo = ADMM_searchAddress(*pointer, BYOBASE) ;
#ifdef ADMM_TIME_SEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
  if (foundChunkInfo) {
    if (pointer && *pointer)
      *pointer = foundChunkInfo->bases[BYBASE]+(*pointer-foundChunkInfo->bases[BYOBASE]) ;
    if (*pointerb)
      *pointerb = foundChunkInfo->bases[BYBASEB]+(*pointerb-foundChunkInfo->bases[BYOBASEB]) ;
#ifdef ADMM_TRACE
    printf(" yes => [%li]->%li Shadow:[%li]->%li !\n", pointer, (pointer?*pointer:NULL), pointerb, (pointerb?*pointerb:NULL)) ;
    printf("  from (old:%li=>)%li+%i    Shadow: (old:%li=>)%li+%i\n", foundChunkInfo->bases[BYOBASE], foundChunkInfo->bases[BYBASE], foundChunkInfo->sizeInBytes, foundChunkInfo->bases[BYOBASEB], foundChunkInfo->bases[BYBASEB], foundChunkInfo->sizeInBytesb);
    fflush(stdout);
#endif
  } else if ((pointer && *pointer) || *pointerb) {
    ADMM_addWaitingRebase(pointer, pointerb) ;
#ifdef ADMM_TRACE
    long indexInWait, numberInWait ;
    ADMM_findRanksInWaitings(pointer, pointerb, &indexInWait, &numberInWait) ;
    printf(" no  => add to waiting list [%li/%li]\n",indexInWait,numberInWait) ;
    fflush(stdout);
#endif
  } else {
#ifdef ADMM_TRACE
    printf(".\n") ;
    fflush(stdout);
#endif
  }
#ifdef ADMM_COUNTS
  ++numberOfRebases ;
#endif
#ifdef ADMM_TIME
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}


void ADMM_rebase(void** pointer
#ifdef ADMM_LABELS
, char *label) {
#else
) {char *label = "" ;
#endif
#ifdef ADMM_TIME
  startADMMTime = clock() ;
#endif
#ifdef ADMM_TRACE
  printf("   ADMM_rebase [%li]->%li \"%s\" ?", pointer, (pointer?*pointer:NULL), label) ;
  fflush(stdout);
#endif
  ADMM_ChunkInfo* foundChunkInfo = NULL ;
  if (*pointer) {
#ifdef ADMM_TIME_SEARCHING
    startSearchTime = clock() ;
#endif
    foundChunkInfo = ADMM_searchAddress(*pointer, BYOBASE) ;
#ifdef ADMM_TIME_SEARCHING
    endSearchTime = clock() ;
    timeSpentSearching += ((double)(endSearchTime-startSearchTime))/((double)CLOCKS_PER_SEC) ;
#endif
    if (foundChunkInfo) {
      *pointer = foundChunkInfo->bases[BYBASE]+(*pointer-foundChunkInfo->bases[BYOBASE]) ;
#ifdef ADMM_TRACE
      printf(" yes => [%li]->%li !\n", pointer, (pointer?*pointer:NULL)) ;
      printf("  from (old:%li=>)%li+%i    Shadow: (old:%li=>)%li+%i\n", foundChunkInfo->bases[BYOBASE], foundChunkInfo->bases[BYBASE], foundChunkInfo->sizeInBytes, foundChunkInfo->bases[BYOBASEB], foundChunkInfo->bases[BYBASEB], foundChunkInfo->sizeInBytesb);
      fflush(stdout);
#endif
    } else {
      ADMM_addWaitingRebase(pointer, NULL) ;
#ifdef ADMM_TRACE
      long indexInWait, numberInWait ;
      ADMM_findRanksInWaitings(pointer, NULL, &indexInWait, &numberInWait) ;
      printf(" no  => add to waiting list [%li/%li]\n",indexInWait,numberInWait) ;
      fflush(stdout);
#endif
    }
  } else {
#ifdef ADMM_TRACE
    printf(".\n") ;
    fflush(stdout);
#endif
  }
#ifdef ADMM_COUNTS
  ++numberOfRebases ;
#endif
#ifdef ADMM_TIME
  endADMMTime = clock() ;
  timeSpentInADMM += ((double)(endADMMTime-startADMMTime))/((double)CLOCKS_PER_SEC) ;
#endif
}

#ifdef ADMM_COUNTS
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
  fflush(stdout);
}
#endif

#ifdef ADMM_COUNTS
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
  fflush(stdout);
}
#endif

void ADMM_statistics() {
#ifdef ADMM_TIME
  printf("Time spent in ADMM:%lf\n", timeSpentInADMM) ;
#endif
#ifdef ADMM_TIME_SEARCHING
  printf("Time spent searching:%lf\n", timeSpentSearching) ;
#endif
#ifdef ADMM_COUNTS
  printf("Registerings:%i, Unregisterings:%i, numberOfRebases:%i\n",
         numberOfRegisterings, numberOfUnregisterings, numberOfRebases) ;
  printf("chunks:%i[peak:%i] waiting:%i[peak:%i]\n",
         numberOfChunks, peakNumberOfChunks, numberOfWaitings, peakNumberOfWaitings);
  ADMM_showChunks() ;
  ADMM_showWaitings() ;
#endif
  fflush(stdout);
}
