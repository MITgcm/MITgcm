/*
 * TAPENADE Automatic Differentiation Engine
 * Copyright (C) 1999-2024 Inria
 * See the LICENSE.md file in the project root for more information.
 */

/**** TAPENADE's ADJOINT AD STACK ****/

// TODO:
// A few storage files (tapStackNNNNN) sometimes are not erased in the end


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <stdint.h>
#include <inttypes.h>

#include "adStack.h"
#include "adComplex.h"

// This code limits the number of stack Block's kept in-core to MAX_SPACES.
// When more Block's must be kept, some Block's will be kept as files.
// File operations (storage and retrieval) are done synchronously by default,
// but they can be done asynchronously by a 2nd thread (using pthreads).

// Compile with -D ADSTACK_BLOCK_SIZE=<number> to set the size in bytes of each
// stack "block" (also the size of each file used to offload stack to disk)
// See the default value below.

// Compile with -D ADSTACK_MAX_SPACES=<number> to set the number of blocks
// allowed in main memory before offloading to disk starts.
// See the default value below.

// Compile with -D ADSTACK_PREFETCH to use a 2nd thread for asynchronous file operations.
// In that case, code must be linked with -lpthread

// Compile with -D ADSTACK_PROFILE to enable summary of stack usage
// through adStack_showPeakSize() and adStack_showTotalTraffic()

// Compile with -D ADSTACK_TRACE to see a trace of the "file storage"
// and (optionaly) "prefetching" mechanism

/******* Size and max number of blocks ********/

#ifndef ADSTACK_BLOCK_SIZE
/* The default value of the size of a Block in bytes. This is also the
 * size of each separate storage file, when the stack is offloaded to disk storage. */
// #define ADSTACK_BLOCK_SIZE 17 // A very small ADSTACK_BLOCK_SIZE allows for stronger testing!
// #define ADSTACK_BLOCK_SIZE 1000 // ridiculously small, just for testing.
#define ADSTACK_BLOCK_SIZE 1048576
#endif

#ifndef ADSTACK_MAX_SPACES
/* The default value of the max number of Block's allowed in main memory.
 * If more Block's are needed,the older Block's will be offloaded to disk (and retrieved later). */
#define ADSTACK_MAX_SPACES 8000
#endif

/**************** Data structures and globals ******************/

#ifdef ADSTACK_PREFETCH

#include <pthread.h>

/* Asynchronous lookahead: number of Block's that we may enqueue for
 * anticipated "preStore" or "preRestore" (before or after) current Block.
 * We assume/require that 2*FETCHAHEAD < ADSTACK_MAX_SPACES, to
 * avoid (possible?) conflicts between preStoring and preRestoring. */
#define FETCHAHEAD 10

pthread_t fileStorageThread ;
pthread_mutex_t fileStorageMutex;
pthread_cond_t enqueuedStorageCV, doneStoreOrRestoreCV ;

#endif

#ifdef ADSTACK_TRACE
// When not set to -1, DEBUG mode will show only actions about the space with this "id" field.
static int tracedId = -1 ;
#endif

typedef enum {INUSE, STORED} CONTENTS_STATE ;
typedef enum {NOACTION, STORE, RESTORE} ACTION ;

char* stateNames[] = {"U", "S"};

/** One elementary (large) chunk of memory used by the stack.
 * It is also the atomic chunk of memory that may be stored in each file */
typedef struct {
  unsigned int rank ;         // From 1 up, or 0 for "not used yet"
  CONTENTS_STATE state ;      // One of {INUSE, STORED}
  char contents[ADSTACK_BLOCK_SIZE] ; // The main storage space.
#ifdef ADSTACK_TRACE
  int id ; //Only to have a distinct name (char 'a'+id) for debug!
#endif
} BlockContents ;

/** The stack is a fwd-bwd-chained list of DoubleChainedBlock objects.
 * Each DoubleChainedBlock holds a pointer to the BlockContents that
 * actually holds the memory space used. */
typedef struct _DoubleChainedBlock{
  unsigned int rank ;  // From 1 up
  struct _DoubleChainedBlock *prev ;
  struct _DoubleChainedBlock *next ;
  BlockContents* toContents ;
} DoubleChainedBlock ;

/** Used to build the name of each stack storage file.
 * Used only by 2nd thread */
char tapStackFileName[14] ;

/** Memorizes the file action currently being done (asynchronously).
 * These 3 globals are written only by 2nd thread, only in a locked section,
 * and they may be read by 1st thread, only in a locked section. */
ACTION fileActionBeingDone = NOACTION ;
BlockContents *spaceBeingDone = NULL ;
int rankBeingDone = -1 ;

// We call "current location" the pair of
// (DoubleChainedBlock *) curStack // the current top stack block.
// (int) tappos                    // the offset of the current top in the current top stack block.

/** A block and an integer to keep the current top in the block. When the block
 * is full, a fresh block is added on top of the stack and will receive the
 * new pushes. During popping, when a block becomes empty, the block below
 * becomes current, with the current top at its topmost position.
 * These 3 globals are used only by 1st thread */
static DoubleChainedBlock *curStack = NULL ;
static char* tapblock = NULL ;
static int tappos  = ADSTACK_BLOCK_SIZE ;

/** All the BlockContents allocated. We allow only ADSTACK_MAX_SPACES of them.
 * Used only by 1st thread */
static BlockContents* allBlockContents[ADSTACK_MAX_SPACES] ; //assume they are all initialized to NULL

/** Structure keeping the needed info for one repetition level (used to read from
 * the stack without removing the values read, thus allowing for reading again).
 * As we can have nested repetition levels, these structures can be stacked. */
typedef struct _RepetitionLevel {
  int active ;
  // The top location (block+offset) of the re-readable part:
  DoubleChainedBlock* resumePointBlock ;
  int resumePoint ;
  // The "freePush" location where one can push/pop (if needed) without overwriting the re-readable values:
  DoubleChainedBlock* freePushBlock ;
  int freePush ;
  // The saved location where one must pop back when returning from the "freePush" location:
  DoubleChainedBlock* backPopBlock ;
  int hasBackPop ;
  int backPop ;
  unsigned int storedadbitbuf ;
  int storedadbitibuf ;
  struct _RepetitionLevel *previous ;
} RepetitionLevel ;

/** The current stack of repetition levels. Initially empty
 * Used only by 1st thread */
RepetitionLevel *topRepetitionPoint = NULL ;

/** Pushing single bits is different from the rest: we collect
 * 32 bits in the integer below, before pushing that to the stack.
 * Used only by 1st thread */
static unsigned int adbitbuf = 0 ;
static int adbitibuf = 0 ;

/** Accumulates the number of bytes pushed and popped */
static u_int64_t pushPopTraffic = 0 ;

/** Remembers the maximum number of stack Blocks used */
static u_int64_t maxBlocks = 0 ;

/* All data structures must be threadprivate, so that each OpenMP thread has
   its own stack. If the stack is compiled with OpenMP support and then used
   in a program with no OpenMP parallel regions, the stack will work as
   expected without using any extra resources. */
#pragma omp threadprivate(tappos, tapblock, curStack, adbitbuf, adbitibuf, topRepetitionPoint)

// pre-declare a few prototypes.
void showLocation(DoubleChainedBlock *locBlock, int loc) ;
void dumpContents(BlockContents *space) ;
void dumpStack(DoubleChainedBlock *st) ;
void dumpRepetitionLevels() ;

char* pushBlock() ;
char* popBlock() ;
void removeStorageFile(int blockRank) ;
void storeInFile(BlockContents* blockContents) ;
void restoreFromFile(BlockContents* blockContents) ;

#ifdef ADSTACK_PREFETCH

/**** Queue of requested file store/restore actions (only when asynchronous) ****/

void dumpFileActionQueue() ;
#endif

/** One cell of the queue of requested file store/restore actions */
typedef struct _FileActionCell {
  struct _FileActionCell *next ;
  int restoredRank ;
  ACTION action ; // One of {STORE, RESTORE}
  BlockContents *space ;
} FileActionCell ;

/** Globals that keep the head and tail of the queue of requested file store/restore actions */
FileActionCell *fileActionQueueHead = NULL;
FileActionCell *fileActionQueueTail = NULL;

#ifdef ADSTACK_PREFETCH
/** Used only by 1st thread. Always called inside pthread_mutex_lock(&fileStorageMutex) */
int enqueued(BlockContents *space) {
  int found = 0 ;
  FileActionCell *curCell = fileActionQueueHead ;
  while (!found && curCell) {
    found = (curCell->space==space) ;
    curCell = curCell->next ;
  }
  return found ;
}

/** Used only by 1st thread, always inside pthread_mutex_lock(&fileStorageMutex) */
void addTailFileAction(BlockContents *space, ACTION action, int restoredRank) {
  FileActionCell *newCell = (FileActionCell*)malloc(sizeof(FileActionCell)) ;
  newCell->next = NULL ;
  newCell->action = action ;
  newCell->restoredRank = restoredRank ;
  newCell->space = space ;
  if (fileActionQueueHead==NULL) {
    fileActionQueueHead = newCell ;
    fileActionQueueTail = newCell ;
  } else {
    fileActionQueueTail->next = newCell ;
    fileActionQueueTail = newCell ;
  }
}

/** Used only by 2nd thread, always inside pthread_mutex_lock(&fileStorageMutex) */
BlockContents *popHeadFileAction() {
  BlockContents *result = fileActionQueueHead->space ;
  if (fileActionQueueHead==fileActionQueueTail) {
    free(fileActionQueueHead) ;
    fileActionQueueHead = NULL ;
    fileActionQueueTail = NULL ;
  } else {
    FileActionCell *tofree = fileActionQueueHead ;
    fileActionQueueHead = tofree->next ;
    free(tofree) ;
  }
  return result ;
}

/** Used only by 1st thread. Always called inside pthread_mutex_lock(&fileStorageMutex) */
void dequeueFileAction(FileActionCell *cell) {
  FileActionCell **toheadcell = &(fileActionQueueHead) ;
  FileActionCell *lastCellSeen = NULL ;
  while (*toheadcell) {
    if (*toheadcell==cell) {
      *toheadcell = cell->next ;
      free(cell) ;
      if (!*toheadcell) { // We removed the last in queue
        fileActionQueueTail = lastCellSeen ;
      }
    } else {
      lastCellSeen = *toheadcell ;
      toheadcell = &((*toheadcell)->next) ;
    }
  }
}

/** Used only by 1st thread. Always called inside pthread_mutex_lock(&fileStorageMutex) */
void emptyFileActionQueue() {
  while (fileActionQueueHead) {
    FileActionCell *tofree = fileActionQueueHead ;
#ifdef ADSTACK_TRACE
    printf(" Forget %s (%c: %02i %s)\n",
           (tofree->action==STORE ? "store" : "restore"),
           'a'+tofree->space->id,
           tofree->space->rank,
           stateNames[tofree->space->state]) ;
#endif
    fileActionQueueHead = tofree->next ;
    free(tofree) ;
  }
  fileActionQueueTail = NULL ;
}

/** Used only by 1st thread. Always called inside pthread_mutex_lock(&fileStorageMutex) */
FileActionCell *alreadyEnqueuedRestore(BlockContents *space) {
  FileActionCell *found = NULL ;
  FileActionCell *curCell = fileActionQueueHead ;
  while (!found && curCell) {
    if (curCell->action==RESTORE
        && curCell->space==space) {
      found = curCell ;
    }
    curCell = curCell->next ;
  }
  return found ;
}

/************ Prefetching mechanism (only when asynchronous) *************/

/** True when the last move was a pushBlock (or equivalent).
 * Used only by 1st thread */
static int goingForward = 1 ;

/** Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void enqueueFileAction(BlockContents* space, ACTION action, int restoredRank) {
  struct timespec ts ;
#ifdef ADSTACK_TRACE
  if (tracedId==-1 || tracedId==space->id) {
    clock_gettime(CLOCK_REALTIME, &ts);
    if (action==STORE) {
      printf("t%09i Enqueue store (%c: %02i %s)\n", ts.tv_nsec,
           'a'+space->id,
           space->rank,
           stateNames[space->state]) ;
    } else if (action==RESTORE) {
      printf("t%09i Enqueue restore %02i into (%c: %02i %s)\n", ts.tv_nsec,
           restoredRank,
           'a'+space->id,
           space->rank,
           stateNames[space->state]) ;
    }
  }
#endif
  addTailFileAction(space, action, restoredRank) ;
#ifdef ADSTACK_TRACE
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i Send signal enqueuedStorageCV\n", ts.tv_nsec) ;
#endif
  pthread_cond_signal(&enqueuedStorageCV) ;  
}

/** Register a request for storing the given BlockContents to file.
 * Rationale: IF the queue already contains a store request for this BlockContents,
 *  or if the fileActionBeingDone is precisely that, THEN (we assume that this other
 *  store deals with the same rank as the given storedRank in this BlockContents and)
 *  we don't enqueue this request because it is already requested or being done.
 *  ELSE IF the queue already contains a restore request for this BlockContents
 *  or if the fileActionBeingDone is precisely that, THEN (we assume that this restore
 *  has already been preceded by the appropriate store, and anyway it is too late for
 *  storing, and) we don't enqueue this request because it is obsolete.
 *  ELSE the queue contains nothing about this BlockContents and the fileActionBeingDone,
 *  if any, does not concern this BlockContents, and therefore we enqueue the given request.
 * Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void preStoreBlock(BlockContents *future, int storedRank) {
#ifdef ADSTACK_TRACE
  if (tracedId==-1 || tracedId==future->id) {
    struct timespec ts ;
    clock_gettime(CLOCK_REALTIME, &ts);
    printf("t%09i preStoreBlock (%c: %02i %s)\n", ts.tv_nsec,
           'a'+future->id, future->rank, stateNames[future->state]) ;
    dumpRepetitionLevels() ;
  }
#endif
  // Don't enqueue for store if already stored:
  if (future->state==INUSE) {
    if (spaceBeingDone==future || enqueued(future)) {
      // possibly check that ranks match, but a priori don't enqueue
    } else {
      enqueueFileAction(future, STORE, storedRank) ;
    }
  }
}

/** Register a request for restoring values for the given restoredRank,
 * from file, into the given BlockContents.
 * Rationale: IF the fileActionBeingDone is precisely a restore for this BlockContents,
 *  THEN (either the restored rank is the same and we don't enqueue the request
 *  because it is being done anyway, or it is for another rank and then we must
 *  enqueue the given request),
 *  ELSE IF the queue already contains a restore request for this BlockContents,
 *  THEN we believe this already enqueued restore request is obsolete, and we
 *  just replace in-place its enqueued rank with the given restoredRank,
 *  ELSE IF the queue already contains (just) a store request for this BlockContents,
 *  or if the fileActionBeingDone is precisely that, THEN we enqueue the given request,
 *  ELSE the queue contains nothing about this BlockContents and the fileActionBeingDone,
 *  if any, does not concern this BlockContents, and therefore we must first enqueue
 *  a request to store the present BlockContents (because restoring will overwrite it)
 *  if not already stored yet, and then enqueue the given request.
 * Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void preRestoreBlock(BlockContents *future, int restoredRank) {
#ifdef ADSTACK_TRACE
  if (tracedId==-1 || tracedId==future->id) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    printf("t%09i preRestoreBlock %02i INTO (%c: %02i %s)\n", ts.tv_nsec,
           restoredRank, 'a'+future->id, future->rank, stateNames[future->state]) ;
    dumpRepetitionLevels() ;
  }
#endif
  FileActionCell *enqueuedRestore ;
  if (fileActionBeingDone==RESTORE && spaceBeingDone==future) {
    if (rankBeingDone!=restoredRank) {
      enqueueFileAction(future, RESTORE, restoredRank) ;
    }
  } else if ((enqueuedRestore = alreadyEnqueuedRestore(future)) != NULL) {
    enqueuedRestore->restoredRank = restoredRank ;
    if (future->rank==restoredRank) {
      dequeueFileAction(enqueuedRestore) ;
    }
  } else if ((fileActionBeingDone==STORE && spaceBeingDone==future)
             || enqueued(future)) {
    if (future->rank!=restoredRank) {
      enqueueFileAction(future, RESTORE, restoredRank) ;
    }
  } else {
    if (future->rank!=restoredRank) {
      if (future->state==INUSE) {
        enqueueFileAction(future, STORE, 0) ;
      }
      enqueueFileAction(future, RESTORE, restoredRank) ;
    }
  }
}

/** Enqueue the anticipated action for the current Block and FETCHAHEAD Blocks after it.
 * When not already done, this action is a store of whatever contents may be in the Block,
 * because this method is called when PUSHing, and this contents will be overwritten by the coming PUSHes.
 * Used only by 1st thread, Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void preStore() {
  if (!goingForward) {
    emptyFileActionQueue() ;
    goingForward = 1 ;
  }
  preStoreBlock(curStack->toContents, curStack->rank) ;
  int curIndex = (curStack->rank - 1)%ADSTACK_MAX_SPACES ;
  for (int i=1 ; i<=FETCHAHEAD ; ++i) {
    BlockContents *future = allBlockContents[(curIndex+i<ADSTACK_MAX_SPACES ? curIndex+i : curIndex+i-ADSTACK_MAX_SPACES)] ;
    if (future) {
      preStoreBlock(future, curStack->rank + i) ;
    }
  }
}

/** Special case of preStore(): the current stack top is the freePush location of the current repetition level.
 * This implies that the contents of this top Block already have some stored values, that must be restored.
 * The anticipated action for the Blocks after current is a pre-store, like in the general case.
 * Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void preStoreAtFreePush() {
  if (!goingForward) {
    emptyFileActionQueue() ;
    goingForward = 1 ;
  }
  // curStack just jumped to the freePush block. Therefore
  // we must retrieve its contents to preserve the part of
  // its contents which is *before* the freePush location.
  preRestoreBlock(curStack->toContents, curStack->rank) ;  
  int curIndex = (curStack->rank - 1)%ADSTACK_MAX_SPACES ;
  for (int i=1 ; i<=FETCHAHEAD ; ++i) {
    BlockContents *future = allBlockContents[(curIndex+i<ADSTACK_MAX_SPACES ? curIndex+i : curIndex+i-ADSTACK_MAX_SPACES)] ;
    if (future) {
      preStoreBlock(future, curStack->rank + i) ;
    }
  }
}

/** Enqueue the anticipated action for the current Block and FETCHAHEAD Blocks before it.
 * When not already done, this action is a restore because this method is called when POPping.
 * Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void preRestore() {
  if (goingForward) {
    emptyFileActionQueue() ;
    goingForward = 0 ;
  }
  preRestoreBlock(curStack->toContents, curStack->rank) ;
  int curIndex = (curStack->rank - 1)%ADSTACK_MAX_SPACES ;
  for (int i=0 ; i<=FETCHAHEAD && i<curStack->rank ; ++i) {
    BlockContents *future = allBlockContents[(curIndex-i>=0 ? curIndex-i : curIndex-i+ADSTACK_MAX_SPACES)] ;
    preRestoreBlock(future, curStack->rank - i) ;
  }
}

/** Wait until the contents of current stack top can be overwritten (i.e. forward),
 * i.e. are used but already STORED in a file (which also applies to the special initial case with rank==0)
 * Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void waitForward() {
  BlockContents *space = curStack->toContents ;
  // We must wait as long as this space's state is INUSE, or also if this space
  // is being read/written asynchronously or is enqueued for future read/write:
  struct timespec ts;
  while (spaceBeingDone==space
         || space->state==INUSE
         || enqueued(space)) {
#ifdef ADSTACK_TRACE
      clock_gettime(CLOCK_REALTIME, &ts);
      printf("t%09i Waiting forward for [%02i (%c: %02i %s)] %c%c%c\n", ts.tv_nsec,
             curStack->rank, 'a'+space->id, space->rank, stateNames[space->state],
             (spaceBeingDone==space ? 't' : 'f'),
             (space->state==INUSE ? 't' : 'f'),
             (enqueued(space) ? 't' : 'f')) ;
#endif
      int rt = 0 ;
//    clock_gettime(CLOCK_REALTIME, &ts);
//    ts.tv_sec += 1;
//    rt = pthread_cond_timedwait(&doneStoreOrRestoreCV, &fileStorageMutex, &ts) ;
      pthread_cond_wait(&doneStoreOrRestoreCV, &fileStorageMutex) ;
#ifdef ADSTACK_TRACE
      clock_gettime(CLOCK_REALTIME, &ts);
      printf("t%09i Wait forward %s\n", ts.tv_nsec, (rt ? "timed out" : "woke up")) ;
#endif
  }
#ifdef ADSTACK_TRACE
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i Done waiting forward [%02i (%c: %02i %s)]\n", ts.tv_nsec,
         curStack->rank, 'a'+space->id, space->rank, stateNames[space->state]) ;
#endif
}

/** Wait until the contents of current stack top can be popped (i.e. read backwards).
 * i.e. do contain the data about the current stack top.
 * Used only by 1st thread. Must be called inside pthread_mutex_lock(&fileStorageMutex) */
void waitBackward() {
  BlockContents *space = curStack->toContents ;
  struct timespec ts;
  // We must wait as long as the rank stored in this space is not
  // the one required at this location of the stack, or also if this space
  // is being read/written asynchronously or is enqueued for future read/write:
  while (spaceBeingDone==space
         || space->rank!=curStack->rank
         || enqueued(space)) {
#ifdef ADSTACK_TRACE
      clock_gettime(CLOCK_REALTIME, &ts);
      printf("t%09i Waiting backward for %02i (%c: %02i %s) %c%c%c\n", ts.tv_nsec,
             curStack->rank, 'a'+space->id, space->rank, stateNames[space->state],
             (spaceBeingDone==space ? 't' : 'f'),
             (space->rank!=curStack->rank ? 't' : 'f'),
             (enqueued(space) ? 't' : 'f')) ;
#endif
      int rt = 0 ;
//    struct timespec ts;
//    clock_gettime(CLOCK_REALTIME, &ts);
//    ts.tv_sec += 1;
//    rt = pthread_cond_timedwait(&doneStoreOrRestoreCV, &fileStorageMutex, &ts) ;
      pthread_cond_wait(&doneStoreOrRestoreCV, &fileStorageMutex) ;
#ifdef ADSTACK_TRACE
      clock_gettime(CLOCK_REALTIME, &ts);
      printf("t%09i Wait backward %s for %02i (%c: %02i %s)\n", ts.tv_nsec,
             (rt ? "timed out" : "woke up"),
             curStack->rank, 'a'+space->id, space->rank, stateNames[space->state]) ;
#endif
  }
#ifdef ADSTACK_TRACE
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i Done waiting backward [%02i (%c: %02i %s)]\n", ts.tv_nsec,
         curStack->rank, 'a'+space->id, space->rank, stateNames[space->state]) ;
#endif    
}

#else

/********* Code when no prefetching and no queue or requested file actions *********/

int enqueued(BlockContents *space) {return 0 ;}

void checkForward() {
  // Synchronous mode. Just make sure we can overwrite in this space:
  BlockContents *space = curStack->toContents ;
  if (space->state==INUSE) {
#ifdef ADSTACK_TRACE
      if (tracedId==-1 || tracedId==space->id) {
        printf(" store (%c: %02i %s) into file tapStack%05i\n", 'a'+space->id,
               space->rank, stateNames[space->state], space->rank) ;
      }
#endif
      storeInFile(space) ;
      space->state = STORED ;
  }
}

void checkBackward() {
  // Synchronous mode, Just make sure this space contains the values that we will need:
  BlockContents *space = curStack->toContents ;
  if (space->rank!=curStack->rank) {
      if (space->state==INUSE) {
#ifdef ADSTACK_TRACE
        if (tracedId==-1 || tracedId==space->id) {
          printf(" store (%c: %02i %s) into file tapStack%05i\n", 'a'+space->id,
                 space->rank, stateNames[space->state], space->rank) ;
        }
#endif
        storeInFile(space) ;
      }
      space->rank = curStack->rank ;
#ifdef ADSTACK_TRACE
      if (tracedId==-1 || tracedId==space->id) {
        printf(" restore (%c: %02i %s) from file tapStack%05i\n", 'a'+space->id,
               space->rank, stateNames[space->state], space->rank) ;
      }
#endif
      restoreFromFile(space) ;
      space->state = STORED ;
  }
}

#endif
  
/***************** Repeated access mechanism *************/

/** Used only by 1st thread. Not locked */
void setBackPopToCurrentLocation(RepetitionLevel *repetitionLevel) {
  repetitionLevel->hasBackPop = 1 ;
  repetitionLevel->backPopBlock = curStack ;
  repetitionLevel->backPop = tappos ;
}

/** Used only by 1st thread. Not locked, but contains a locked section. */
void setCurrentLocationToBackPop(RepetitionLevel *repetitionLevel) {
  curStack = repetitionLevel->backPopBlock ;
#ifdef ADSTACK_PREFETCH
  // Start a locked section on 1st thread:
  pthread_mutex_lock(&fileStorageMutex) ;
  emptyFileActionQueue() ;
  goingForward = 0 ;
  preRestore() ;
  waitBackward() ;
  curStack->toContents->rank = curStack->rank ;
  pthread_mutex_unlock(&fileStorageMutex) ;
  // End locked section on 1st thread.
#else
  checkBackward() ;
  curStack->toContents->rank = curStack->rank ;
#endif
#ifdef ADSTACK_TRACE
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i %02i <= BackPop\n", ts.tv_nsec, curStack->rank) ;
#endif
  tapblock = curStack->toContents->contents ;
  tappos = repetitionLevel->backPop ;
}

/** Used only by 1st thread. Not locked */
void setResumePointToCurrentLocation(RepetitionLevel *repetitionLevel) {
  repetitionLevel->resumePointBlock = curStack ;
  repetitionLevel->resumePoint = tappos ;
}

/** Used only by 1st thread. Not locked, but contains a locked section. */
void setCurrentLocationToResumePoint(RepetitionLevel *repetitionLevel) {
  curStack = repetitionLevel->resumePointBlock ;
#ifdef ADSTACK_PREFETCH
  // Start a locked section on 1st thread:
  pthread_mutex_lock(&fileStorageMutex) ;
  emptyFileActionQueue() ;
  goingForward = 0 ;
  preRestore() ;
  waitBackward() ;
  curStack->toContents->rank = curStack->rank ;
  pthread_mutex_unlock(&fileStorageMutex) ;
  // End locked section on 1st thread.
#else
  checkBackward() ;
  curStack->toContents->rank = curStack->rank ;
#endif
#ifdef ADSTACK_TRACE
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i %02i <= Repeat\n", ts.tv_nsec, curStack->rank) ;
#endif
  tapblock = curStack->toContents->contents ;
  tappos = repetitionLevel->resumePoint ;
} 

/** Used only by 1st thread. Not locked */
void setFreePushToCurrentLocation(RepetitionLevel *repetitionLevel) {
  repetitionLevel->freePushBlock = curStack ;
  repetitionLevel->freePush = tappos ;
}

/** Used only by 1st thread. Not locked, but contains a locked section. */
void setCurrentLocationToFreePush(RepetitionLevel *repetitionLevel) {
  curStack = repetitionLevel->freePushBlock ;
#ifdef ADSTACK_PREFETCH
  // Start a locked section on 1st thread:
  pthread_mutex_lock(&fileStorageMutex) ;
  emptyFileActionQueue() ;
  goingForward = 1 ;
  preStoreAtFreePush() ;
  waitBackward() ;
  curStack->toContents->rank = curStack->rank ;
  curStack->toContents->state = INUSE ; // because we are going to push into curStack
  pthread_mutex_unlock(&fileStorageMutex) ;
  // End locked section on 1st thread.
#else
  checkBackward() ;
  curStack->toContents->rank = curStack->rank ;
  curStack->toContents->state = INUSE ; // because we are going to push into curStack
#endif
#ifdef ADSTACK_TRACE
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i FreePush => %02i\n", ts.tv_nsec, curStack->rank) ;
#endif
  tapblock = curStack->toContents->contents ;
  tappos = repetitionLevel->freePush ;
} 

//TODO: try inline this function for efficiency:
/** Used only by 1st thread. Not locked */
int currentLocationStrictBelowFreePush(RepetitionLevel *repetitionLevel) {
  // Not as simple as it could be, because N;ADSTACK_BLOCK_SIZE <=> N+1;0
  //  and both may happen due to initial NULL curStack...
  int curL1 = curStack->rank ;
  int curL2 = tappos ;
  int fpL1 = repetitionLevel->freePushBlock->rank ;
  int fpL2 = repetitionLevel->freePush ;
  if (curL2==ADSTACK_BLOCK_SIZE) {++curL1 ; curL2=0 ;}
  if (fpL2==ADSTACK_BLOCK_SIZE) {++fpL1 ; fpL2=0 ;}
  return (curL1<fpL1 || (curL1==fpL1 && curL2<fpL2)) ;
}

//TODO: try inline this function for efficiency:
/** Used only by 1st thread. Not locked */
int currentLocationEqualsFreePush(RepetitionLevel *repetitionLevel) {
  // Not as simple as it could be, because N;ADSTACK_BLOCK_SIZE <=> N+1;0
  // and both may happen due to initial NULL curStack...
  int curL1 = curStack->rank ;
  int curL2 = tappos ;
  int fpL1 = repetitionLevel->freePushBlock->rank ;
  int fpL2 = repetitionLevel->freePush ;
  if (curL2==ADSTACK_BLOCK_SIZE) {++curL1 ; curL2=0 ;}
  if (fpL2==ADSTACK_BLOCK_SIZE) {++fpL1 ; fpL2=0 ;}
  return (curL1==fpL1 && curL2==fpL2) ;
}

//TODO: try inline this function for efficiency:
/** Used only by 1st thread. Not locked */
int currentLocationEqualsBackPop(RepetitionLevel *repetitionLevel) {
  // Not as simple as it could be, because N;ADSTACK_BLOCK_SIZE <=> N+1;0
  // and both may happen due to initial NULL curStack...
  int curL1 = curStack->rank ;
  int curL2 = tappos ;
  int bpL1 = repetitionLevel->backPopBlock->rank ;
  int bpL2 = repetitionLevel->backPop ;
  if (curL2==ADSTACK_BLOCK_SIZE) {++curL1 ; curL2=0 ;}
  if (bpL2==ADSTACK_BLOCK_SIZE) {++bpL1 ; bpL2=0 ;}
  return (curL1==bpL1 && curL2==bpL2) ;
}

/** If we are in a protected, read-only section,
 * memorize current location as "backPop" and go to the "freePush" location.
 * Used only by 1st thread. Not locked */
void checkPushInReadOnly() {
  RepetitionLevel *topActive = topRepetitionPoint ;
  while (topActive && !topActive->active) {
    topActive = topActive->previous ;
  }
  if (topActive) {
    if(currentLocationStrictBelowFreePush(topActive)) {
      setBackPopToCurrentLocation(topActive) ;
      setCurrentLocationToFreePush(topActive) ;
#ifdef ADSTACK_TRACE
      printf("BEFORE PUSH AT ") ;
      showLocation(topRepetitionPoint->backPopBlock, topRepetitionPoint->backPop) ;
      printf("  WITH REPETITION LEVELS:\n") ;
      dumpRepetitionLevels() ;
      printf("  MOVE TO FREE PUSH LOCATION ") ;
      showLocation(topRepetitionPoint->freePushBlock, topRepetitionPoint->freePush) ;
      printf("\n") ;
#endif
    } else if (currentLocationEqualsFreePush(topActive)) {
      //setBackPopToCurrentLocation(topActive) ; //not sure
      setCurrentLocationToFreePush(topActive) ;
#ifdef ADSTACK_TRACE
      printf("BEFORE PUSH 2 AT ") ;
      showLocation(topRepetitionPoint->backPopBlock, topRepetitionPoint->backPop) ;
      printf("  WITH REPETITION LEVELS:\n") ;
      dumpRepetitionLevels() ;
      printf("  MOVE TO FREE PUSH LOCATION ") ;
      showLocation(topRepetitionPoint->freePushBlock, topRepetitionPoint->freePush) ;
      printf("\n") ;
#endif
    }
  }
}

/** If current location is some "freePush" location,
 * go back to its "backPop" location, which is in a protected, read-only section.
 * Used only by 1st thread. Not locked */
void checkPopToReadOnly() {
  RepetitionLevel *repetitionPoint = topRepetitionPoint ;
#ifdef ADSTACK_TRACE
  RepetitionLevel *activeRepetitionPoint = topRepetitionPoint ;
  while (activeRepetitionPoint && !activeRepetitionPoint->active) {
    activeRepetitionPoint = activeRepetitionPoint->previous ;
  }
  if (activeRepetitionPoint && activeRepetitionPoint->hasBackPop
      && currentLocationEqualsFreePush(activeRepetitionPoint)) {
    printf("AFTER POP, LOCATION WAS ") ;
    showLocation(curStack, tappos) ;
    printf("  WITH REPETITION LEVELS:\n") ;
    dumpRepetitionLevels() ;
  }
#endif
  int canEraseInactive = 1 ;
  int canRemoveBackPop = 1 ;
  do {
    RepetitionLevel *oldCell = repetitionPoint ;
    if (oldCell->hasBackPop && oldCell->active) {
      if (currentLocationEqualsFreePush(oldCell)) {
        setCurrentLocationToBackPop(oldCell) ;
#ifdef ADSTACK_TRACE
        printf("  MOVED TO BACK POP LOCATION:") ;
        showLocation(curStack, tappos) ;
        printf("\n") ;
#endif
        if (canRemoveBackPop) oldCell->hasBackPop = 0 ;
      } else if (currentLocationEqualsBackPop(oldCell)) {
        if (canRemoveBackPop) oldCell->hasBackPop = 0 ;
      }
    }
    repetitionPoint = oldCell->previous ;
    if (!oldCell->active && canEraseInactive) {
      free(oldCell) ;
      topRepetitionPoint = repetitionPoint ;
    } else {
      canEraseInactive = 0 ;
      canRemoveBackPop = 0 ;
    }
  } while (repetitionPoint) ;
}

/** From now on, and until the matching closing adStack_endRepeat(),
 * the current contents of the push-pop stack are preserved:
 * Even if they are popped, any subsequent adStack_resetRepeat()
 * will reset the push-pop stack to its current contents of now.
 * Used only by 1st thread. Not locked */
void adStack_startRepeat() {
#ifdef ADSTACK_TRACE
  printf("BEFORE START REPEAT AT ") ;
  showLocation(curStack, tappos) ;
  printf("\n") ;
  dumpRepetitionLevels() ;
  dumpStack(curStack) ; printf("\n") ;
#endif
  // Create a new repetition level and push it onto topRepetitionPoint
  RepetitionLevel *newRepetitionLevel = (RepetitionLevel *)malloc(sizeof(RepetitionLevel)) ;
  if (!newRepetitionLevel) {
    printf("Out of memory while creating a RepetitionLevel.\n") ;
    exit(0) ;
  }
  newRepetitionLevel->previous = topRepetitionPoint ;
  newRepetitionLevel->hasBackPop = 0 ;
  newRepetitionLevel->active = 1 ;
  newRepetitionLevel->backPopBlock = NULL ;
  newRepetitionLevel->backPop = 0 ;
  newRepetitionLevel->resumePointBlock = NULL ;
  newRepetitionLevel->resumePoint = 0 ;
  newRepetitionLevel->freePushBlock = NULL ;
  newRepetitionLevel->freePush = 0 ;
  // Copy the bits buffer:
  newRepetitionLevel->storedadbitbuf = adbitbuf ;
  newRepetitionLevel->storedadbitibuf = adbitibuf ;
  // In the very weird case where current stack is empty, make it explicit:
  if (curStack==NULL) {
    tapblock = pushBlock() ;
    tappos = 0 ;
  }
  // Store current location as the "resumePoint" location:
  setResumePointToCurrentLocation(newRepetitionLevel) ;
  // Set the "freePush" location to current location OR to
  // the "freePush" of the "enclosing" repetition level,
  // (if there is one and it is higher)
  if (topRepetitionPoint && currentLocationStrictBelowFreePush(topRepetitionPoint)) {
    newRepetitionLevel->freePushBlock = topRepetitionPoint->freePushBlock ;
    newRepetitionLevel->freePush = topRepetitionPoint->freePush ;
  } else {
    setFreePushToCurrentLocation(newRepetitionLevel) ;
  }
  // Make this new repetition level the current repetition level:
  topRepetitionPoint = newRepetitionLevel ;
#ifdef ADSTACK_TRACE
  printf(">AFTER START REPEAT AT:") ;
  showLocation(curStack, tappos) ;
  printf("\n") ;
  dumpRepetitionLevels() ;
  dumpStack(curStack) ; printf("\n") ;
#endif
}

/** Reset the push-pop stack contents to its contents at
 * the time of the latest adStack_startRepeat() that has not been
 * closed by a subsequent adStack_endRepeat().
 * Used only by 1st thread. Not locked */
void adStack_resetRepeat() {
#ifdef ADSTACK_TRACE
  printf("BEFORE RESET REPEAT AT ") ;
  showLocation(curStack, tappos) ;
  printf("\n") ;
  dumpRepetitionLevels() ;
  dumpStack(curStack) ; printf("\n") ;
#endif
  // Remove (pop) all passive deeper repetition levels:
  while (topRepetitionPoint && !topRepetitionPoint->active) {
    RepetitionLevel *oldTop = topRepetitionPoint ;
    topRepetitionPoint = topRepetitionPoint->previous ;
    free(oldTop) ;
  }
  // Reset current location to "resumePoint" location:
  setCurrentLocationToResumePoint(topRepetitionPoint) ;
  // Reset the bits buffer:
  adbitbuf = topRepetitionPoint->storedadbitbuf ;
  adbitibuf = topRepetitionPoint->storedadbitibuf ;
#ifdef ADSTACK_TRACE
  printf(">AFTER RESET REPEAT AT ") ;
  showLocation(curStack, tappos) ;
  printf("\n") ;
  dumpRepetitionLevels() ;
  dumpStack(curStack) ; printf("\n") ;
#endif
}

/** Close (i.e. remove) the repetition level created by the latest adStack_startRepeat().
 * Used only by 1st thread. Not locked, but contains a locked section */
void adStack_endRepeat() {
#ifdef ADSTACK_TRACE
  printf("BEFORE END REPEAT AT ") ;
  showLocation(curStack, tappos) ;
  printf("\n") ;
  dumpRepetitionLevels() ;
  dumpStack(curStack) ; printf("\n") ;
#endif
  // Set to inactive the topmost active repetition level:
  RepetitionLevel *topActive = topRepetitionPoint ;
  while (!topActive->active) {
    topActive = topActive->previous ;
  }
  topActive->active = 0 ;

  if (!(topActive->previous!=NULL &&
        topActive->previous->freePushBlock == topActive->freePushBlock)) {
    // i.e. unless this repeat section is a sub-section of the repeat above,
    // remove storage files of all Blocks from the current location
    // (or at least from the freePushBlock of the enclosing repeat) ...
    DoubleChainedBlock *eraseBlockFrom = curStack ;
    if (topActive->previous!=NULL
        && topActive->previous->freePushBlock->next
        && topActive->previous->freePushBlock->next->rank > curStack->rank) {
      eraseBlockFrom = topActive->previous->freePushBlock->next ;
    }
    // ... till the freePushBlock of the currently ended repeat level.
    DoubleChainedBlock *eraseBlockTo = topActive->freePushBlock ;
#ifdef ADSTACK_TRACE
    printf(" erase all files from %02i up to %02i\n",
           (eraseBlockFrom ? eraseBlockFrom->rank : -1),
           (eraseBlockTo ? eraseBlockTo->rank : -1)) ;
#endif
#ifdef ADSTACK_PREFETCH
    // Start a locked section on 1st thread:
    pthread_mutex_lock(&fileStorageMutex) ;
#endif
    while (eraseBlockFrom) {
      if (eraseBlockFrom->rank!=eraseBlockFrom->toContents->rank) {
        removeStorageFile(eraseBlockFrom->rank) ;
      }
      if (eraseBlockFrom->rank==eraseBlockFrom->toContents->rank) {
        if (eraseBlockFrom->toContents->state==STORED
            || eraseBlockFrom==topActive->freePushBlock) {
          removeStorageFile(eraseBlockFrom->toContents->rank) ;
          eraseBlockFrom->toContents->state = INUSE ;
        }
        if (eraseBlockFrom!=curStack && spaceBeingDone!=eraseBlockFrom->toContents
            && !enqueued(eraseBlockFrom->toContents)) {
          eraseBlockFrom->toContents->rank = 0 ;
          eraseBlockFrom->toContents->state = STORED ;
        }
      }
      if (eraseBlockFrom==eraseBlockTo) {
        eraseBlockFrom = NULL ;
      } else {
        eraseBlockFrom = eraseBlockFrom->next ;
      }
    }
#ifdef ADSTACK_PREFETCH
    pthread_mutex_unlock(&fileStorageMutex) ;
    // End locked section on 1st thread.
#endif
  }
  // current location may have moved back ; check if we must move further back:
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_TRACE
  printf(">AFTER END REPEAT AT ") ;
  showLocation(curStack, tappos) ;
  printf("\n") ;
  dumpRepetitionLevels() ;
  dumpStack(curStack) ; printf("\n") ;
#endif
}

/************** Management of storage in files ***************/

/** Used only by 2nd thread. Not locked */
void storeInFile(BlockContents* blockContents) {
  sprintf(tapStackFileName, "tapStack%05i\0", blockContents->rank) ;
  FILE *tapStackFile = fopen(tapStackFileName, "wb") ;
  fwrite(blockContents->contents, 1, ADSTACK_BLOCK_SIZE, tapStackFile) ;
  fclose(tapStackFile) ;
}

/** Used only by 2nd thread. Not locked */
void restoreFromFile(BlockContents* blockContents /*, RepetitionLevel* repetitionForbidsRemove*/) {
  sprintf(tapStackFileName, "tapStack%05i\0", blockContents->rank) ;
  FILE *tapStackFile = fopen(tapStackFileName, "rb") ;
  fread(blockContents->contents, 1, ADSTACK_BLOCK_SIZE, tapStackFile) ;
  fclose(tapStackFile) ;
}

/** Used only by 1st thread. Not locked */
void removeStorageFile(int blockRank) {
  char tapStackFileNameBis[14] ;
  sprintf(tapStackFileNameBis, "tapStack%05i\0", blockRank) ;
#ifdef ADSTACK_TRACE
  printf("  Remove storage file %s\n", tapStackFileNameBis) ;
#endif
  remove(tapStackFileNameBis) ;
}

/** Used only by 1st thread. Not locked. */
BlockContents* assignSpace(int blockRank) {
  BlockContents* space ;
  int index = (blockRank-1)%ADSTACK_MAX_SPACES ; //index in allBlockContents
  space = allBlockContents[index] ;
  if (!space) {
      space = (BlockContents*)malloc(sizeof(BlockContents)) ;
      if (!space) {
        printf("Out of memory while creating a BlockContents.\n") ;
        exit(0) ;
      }
#ifdef ADSTACK_TRACE
      space->id = index ;
#endif
      // Convention: STORED and rank 0 means not in use and ready to be used
      space->rank = 0 ;
      space->state = STORED ;
      allBlockContents[index] = space ;
  }
  return space ;
}

#ifdef ADSTACK_PREFETCH
/** Used only by 2nd thread. Not locked, but contains mostly locked sections. */
void* manageFileStorage(void *arg) {
  struct timespec ts;
  // Start a locked section on 2nd thread:
  pthread_mutex_lock(&fileStorageMutex) ;
  while (1) {
    while (fileActionQueueHead==NULL) {
#ifdef ADSTACK_TRACE
      clock_gettime(CLOCK_REALTIME, &ts);
      printf("t%09i------------------ Waiting in manage\n", ts.tv_nsec) ;
#endif
      int rt = 0 ;
//    clock_gettime(CLOCK_REALTIME, &ts);
//    ts.tv_sec += 1;
//    rt = pthread_cond_timedwait(&enqueuedStorageCV, &fileStorageMutex, &ts) ;
      pthread_cond_wait(&enqueuedStorageCV, &fileStorageMutex) ;
#ifdef ADSTACK_TRACE
      clock_gettime(CLOCK_REALTIME, &ts);
      printf("t%09i------------------ Wait %s in manage\n", ts.tv_nsec, (rt ? "timed out" : "woke up")) ;
#endif
    }
#ifdef ADSTACK_TRACE
    clock_gettime(CLOCK_REALTIME, &ts);
    printf("t%09i------------------ Manage file action queue:", ts.tv_nsec) ; dumpFileActionQueue() ; printf("\n") ;
#endif
    fileActionBeingDone = fileActionQueueHead->action ;
    if (fileActionBeingDone==STORE) {
      spaceBeingDone = popHeadFileAction() ;
      rankBeingDone = spaceBeingDone->rank ;
    } else {
      rankBeingDone = fileActionQueueHead->restoredRank ;
      spaceBeingDone = popHeadFileAction() ;
      spaceBeingDone->rank = rankBeingDone ;
    }
    pthread_mutex_unlock(&fileStorageMutex) ;
    // End locked section on 2nd thread.
#ifdef ADSTACK_TRACE
    clock_gettime(CLOCK_REALTIME, &ts);
    if (fileActionBeingDone==STORE) {
      printf("t%09i------------------ Going to store %04i from (%c: %02i %s)\n", ts.tv_nsec,
             spaceBeingDone->rank,
             'a'+spaceBeingDone->id,
             spaceBeingDone->rank,
             stateNames[spaceBeingDone->state]) ;
    } else if (fileActionBeingDone==RESTORE) {
      printf("t%09i------------------ Going to restore %04i into (%c: %02i %s)\n", ts.tv_nsec,
             spaceBeingDone->rank, //restoredRank,
             'a'+spaceBeingDone->id,
             spaceBeingDone->rank,
             stateNames[spaceBeingDone->state]) ;
    }
#endif
    if (fileActionBeingDone==STORE) {
      storeInFile(spaceBeingDone) ;
    } else if (fileActionBeingDone==RESTORE) {
      restoreFromFile(spaceBeingDone) ;
    }
    // Start a locked section on 2nd thread:
    pthread_mutex_lock(&fileStorageMutex) ;
    spaceBeingDone->state = STORED ;
#ifdef ADSTACK_TRACE
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    printf("t%09i------------------ %s (%c: %02i %s) done, send signal doneStoreOrRestoreCV\n",
           ts.tv_nsec, (fileActionBeingDone==STORE ? "Store" : "Restore"),
           'a'+spaceBeingDone->id,
           spaceBeingDone->rank, stateNames[spaceBeingDone->state]) ;
#endif
    fileActionBeingDone = NOACTION ;
    spaceBeingDone = NULL ;
    rankBeingDone = -1 ;
    pthread_cond_signal(&doneStoreOrRestoreCV) ;
  }
  pthread_mutex_unlock(&fileStorageMutex) ;
  // End locked section on 2nd thread.
}
#endif

/***************** Standard management of the stack *************/

/** Push a data block onto the stack
 * Used only by 1st thread. Not locked, but contains a locked section. */
char* pushBlock() {
  struct timespec ts;
  if (curStack && curStack->next) {
    curStack = curStack->next ;
  } else {
    DoubleChainedBlock *newStack = (DoubleChainedBlock*)malloc(sizeof(DoubleChainedBlock)) ;
    if (!newStack) {
      printf("Out of memory while creating a DoubleChainedBlock.\n") ;
      exit(0) ;
    }
    newStack->toContents = NULL ;
    if(curStack != NULL) {
      curStack->next = newStack ;
      newStack->rank = curStack->rank + 1 ;
    } else {
      newStack->rank = 1 ;
#ifdef ADSTACK_PREFETCH
      // INITIALIZE THINGS AND LAUNCH THE 2nd THREAD:
      int errCode = 0 ;
      if ((errCode = pthread_mutex_init(&fileStorageMutex, NULL)) != 0) {
          printf("Error creating mutex. Error code:%i\n", errCode) ;
      }
      if ((errCode = pthread_cond_init(&enqueuedStorageCV, NULL)) != 0) {
          printf("Error creating condition variable enqueuedStorageCV. Error code:%i\n", errCode) ;
      }
      if ((errCode = pthread_cond_init(&doneStoreOrRestoreCV, NULL)) != 0) {
          printf("Error creating condition variable doneStoreOrRestoreCV. Error code:%i\n", errCode) ;
      }
      if ((errCode = pthread_create(&fileStorageThread, NULL, manageFileStorage, NULL)) != 0) {
          printf("Error creating thread. Error code:%i\n", errCode) ;
      }
#endif
    }
    newStack->toContents = NULL ;
    newStack->prev = curStack ;
    newStack->next = NULL ;
    curStack = newStack ;
  }
  BlockContents *space = curStack->toContents ;
  if (!space) {
    space = assignSpace(curStack->rank);
    curStack->toContents = space ;
  }
#ifdef ADSTACK_PREFETCH
  // Start a locked section on 1st thread:
  pthread_mutex_lock(&fileStorageMutex) ;
  preStore() ;
  waitForward() ;
  space->state = INUSE ;
  space->rank = curStack->rank ;
  pthread_mutex_unlock(&fileStorageMutex) ;
  // End locked section on 1st thread.
#else
  checkForward() ;
  space->state = INUSE ;
  space->rank = curStack->rank ;
#endif
#ifdef ADSTACK_TRACE
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i %02i => %02i\n", ts.tv_nsec, curStack->rank - 1,
         curStack->rank) ;
  dumpStack(curStack) ; printf("\n") ;
#endif
#ifdef ADSTACK_PROFILE
  if (curStack->rank > maxBlocks) maxBlocks = curStack->rank ;
#endif
  return space->contents ;
}

/** Pop the data block at top
 * Used only by 1st thread. Not locked, but contains a locked section. */
char* popBlock() {
  DoubleChainedBlock *oldTopStack = curStack ;
  struct timespec ts;
  curStack = curStack->prev ;
#ifdef ADSTACK_PREFETCH
  // Start a locked section on 1st thread:
  pthread_mutex_lock(&fileStorageMutex) ;
  int oldInRepetition =
    (topRepetitionPoint && oldTopStack->rank <= topRepetitionPoint->freePushBlock->rank) ;
  if (!oldInRepetition && spaceBeingDone!=oldTopStack->toContents
      && !enqueued(oldTopStack->toContents)) {
      oldTopStack->toContents->rank = 0 ;
      oldTopStack->toContents->state = STORED ;
    }
  preRestore() ;
  waitBackward() ;
  if (!topRepetitionPoint || curStack->rank >= topRepetitionPoint->freePushBlock->rank) {
    if ((!topRepetitionPoint || curStack != topRepetitionPoint->freePushBlock)
        && curStack->toContents->state==STORED) {
      removeStorageFile(curStack->toContents->rank) ;
    }
    // because we are going to pop from curStack then possibly push again:
    curStack->toContents->state = INUSE ;
  }
  curStack->toContents->rank = curStack->rank ;
  pthread_mutex_unlock(&fileStorageMutex) ;
  // End locked section on 1st thread.
#else
  int oldInRepetition =
    (topRepetitionPoint && oldTopStack->rank <= topRepetitionPoint->freePushBlock->rank) ;
  if (!oldInRepetition && spaceBeingDone!=oldTopStack->toContents
      && !enqueued(oldTopStack->toContents)) {
      oldTopStack->toContents->rank = 0 ;
      oldTopStack->toContents->state = STORED ;
    }
  checkBackward() ;
  if (!topRepetitionPoint || curStack->rank >= topRepetitionPoint->freePushBlock->rank) {
    if ((!topRepetitionPoint || curStack != topRepetitionPoint->freePushBlock)
        && curStack->toContents->state==STORED) {
      removeStorageFile(curStack->toContents->rank) ;
    }
    // because we are going to pop from curStack then possibly push again:
    curStack->toContents->state = INUSE ;
  }
  curStack->toContents->rank = curStack->rank ;
#endif
#ifdef ADSTACK_TRACE
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i %02i <= %02i\n", ts.tv_nsec, curStack->rank, curStack->rank + 1) ;
  dumpStack(curStack) ; printf("\n") ;
#endif
  return (curStack ? curStack->toContents->contents : NULL) ;
}

/********************* Array push/pop functions ***********************/

// All routines below are used only by 1st thread. Not locked.

/* pushNArray/popNArray are used not only to store arrays of various data
   types. These functions are also the only ones that interact with the dynamic
   memory management, e.g. requesting new blocks. If one of the scalar push/pop
   functions (e.g. pushReal4) encounters the end of a block, it will ask
   pushNArray to do all the work, i.e. start a new block and push the real4
   value to it. */
void pushNArray(char *x, int nbChars) {
  do {
    int wsize = tappos+nbChars<ADSTACK_BLOCK_SIZE?nbChars:ADSTACK_BLOCK_SIZE-tappos ;
    if(wsize > 0) {
      memcpy(tapblock+tappos,x,wsize) ;
      nbChars -= wsize ;
      x += wsize ;
      tappos += wsize ;
    }
    else if (nbChars > 0) {
      tapblock = pushBlock() ;
      tappos = 0 ;
    }
  } while(nbChars > 0) ; //=> lazy push: if finishes at the top of block contents, does not push a new block.
}

void popNArray(char *x, int nbChars) {
  x += nbChars ;
  do {
    int wsize = (nbChars<tappos)?nbChars:tappos ;
    if(wsize > 0) {
      memcpy(x-wsize,tapblock+tappos-wsize,wsize) ;
      nbChars -= wsize ;
      x -= wsize ;
      tappos -= wsize ;
    }
    else if (nbChars > 0) {
      tapblock = popBlock() ;
      tappos = ADSTACK_BLOCK_SIZE ;
    }
  } while(nbChars > 0) ; //=> lazy pop: if finishes at the bottom of block contents, does not pop block.
}

void pushInteger4Array(int *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*4)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void popInteger4Array(int *x, int n) {
  popNArray((char *)x,(int)(n*4)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void pushInteger8Array(long *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*8)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void popInteger8Array(long *x, int n) {
  popNArray((char *)x,(int)(n*8)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void pushReal4Array(float *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*4)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void popReal4Array(float *x, int n) {
  popNArray((char *)x,(int)(n*4)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void pushReal8Array(double *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*8)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void popReal8Array(double *x, int n) {
  popNArray((char *)x,(int)(n*8)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void pushReal16Array(long double *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*16)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*16) ;
#endif
}

void popReal16Array(long double *x, int n) {
  popNArray((char *)x,(int)(n*16)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*16) ;
#endif
}

void pushComplex8Array(ccmplx *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*8)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void popComplex8Array(ccmplx *x, int n) {
  popNArray((char *)x,(int)(n*8)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void pushComplex16Array(double complex *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*16)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*16) ;
#endif
}

void popComplex16Array(double complex *x, int n) {
  popNArray((char *)x,(int)(n*16)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)(n*16) ;
#endif
}

void pushCharacterArray(char *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray(x,(int)n) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)n ;
#endif
}

void popCharacterArray(char *x, int n) {
  popNArray(x,(int)n) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += (int)n ;
#endif
}

/***************** Scalar push/pop functions *****************/

void pushCharacter(char val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 1 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 1) ;
  }
  else {
    *(char*)(tapblock+tappos) = val;
    tappos = tappos + 1 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 1 ;
#endif
}

void popCharacter(char * val) {
  if(tappos - 1 < 0) {
    popNArray((char*)val, 1) ;
  }
  else {
    tappos = tappos - 1 ;
    *val = *(char*)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 1 ;
#endif
}

void pushReal4(float val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 4 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 4) ;
  }
  else {
    *(float*)(tapblock+tappos) = val;
    tappos = tappos + 4 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 4 ;
#endif
}

void popReal4(float * val) {
  if(tappos - 4 < 0) {
    popNArray((char*)val, 4) ;
  }
  else {
    tappos = tappos - 4 ;
    *val = *(float*)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 4 ;
#endif
}

void pushReal8(double val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(double*)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void popReal8(double * val) {
  if(tappos - 8 < 0) {
    popNArray((char*)val, 8) ;
  }
  else {
    tappos = tappos - 8 ;
    *val = *(double*)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void pushReal16(long double *val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 16 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)val, 16) ;
  }
  else {
    memcpy(tapblock+tappos, (void *)val, 16);
    tappos = tappos + 16 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 16 ;
#endif
}

void popReal16(long double *val) {
  if(tappos - 16 < 0) {
    popNArray((char*)val, 16) ;
  }
  else {
    tappos = tappos - 16 ;
    memcpy((void *)val, tapblock+tappos, 16) ;
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 16 ;
#endif
}

void pushInteger4(int val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 4 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 4) ;
  }
  else {
    *(int*)(tapblock+tappos) = val;
    tappos = tappos + 4 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 4 ;
#endif
}

void popInteger4(int * val) {
  if(tappos - 4 < 0) {
    popNArray((char*)val, 4) ;
  }
  else {
    tappos = tappos - 4 ;
    *val = *(int*)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 4 ;
#endif
}

void pushInteger8(long val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(long*)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void popInteger8(long * val) {
  if(tappos - 8 < 0) {
    popNArray((char*)val, 8) ;
  }
  else {
    tappos = tappos - 8 ;
    *val = *(long*)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void pushComplex8(ccmplx val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(ccmplx*)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void popComplex8(ccmplx * val) {
  if(tappos - 8 < 0) {
    popNArray((char*)val, 8) ;
  }
  else {
    tappos = tappos - 8 ;
    *val = *(ccmplx*)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void pushComplex16(double complex val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 16 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 16) ;
  }
  else {
    *(double complex *)(tapblock+tappos) = val;
    tappos = tappos + 16 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 16 ;
#endif
}

void popComplex16(double complex *val) {
  if(tappos - 16 < 0) {
    popNArray((char*)val, 16) ;
  }
  else {
    tappos = tappos - 16 ;
    *val = *(double complex *)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 16 ;
#endif
}

void pushPointer4(void * val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 4 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 4) ;
  }
  else {
    *(void**)(tapblock+tappos) = val;
    tappos = tappos + 4 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 4 ;
#endif
}

void popPointer4(void ** val) {
  if(tappos - 4 < 0) {
    popNArray((char*)val, 4) ;
  }
  else {
    tappos = tappos - 4 ;
    *val = *(void**)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 4 ;
#endif
}

void pushPointer8(void * val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > ADSTACK_BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(void**)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

void popPointer8(void ** val) {
  if(tappos - 8 < 0) {
    popNArray((char*)val, 8) ;
  }
  else {
    tappos = tappos - 8 ;
    *val = *(void**)(tapblock+tappos);
  }
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += 8 ;
#endif
}

/******************* Bit (hidden primitives) ***************/

void pushBit(int x) {
  adbitbuf<<=1 ;
  if (x) ++adbitbuf ;
  if (adbitibuf>=31) {
    pushNArray((char *)&adbitbuf, 4) ;
    adbitbuf = 0 ;
    adbitibuf = 0 ;
#ifdef ADSTACK_PROFILE
    pushPopTraffic += 4 ;
#endif
  } else
    ++adbitibuf ;
}

int popBit() {
  if (adbitibuf<=0) {
    popNArray((char *)&adbitbuf, 4) ;
    adbitibuf = 31 ;
#ifdef ADSTACK_PROFILE
    pushPopTraffic += 4 ;
#endif
  } else
    --adbitibuf ;
  int result = adbitbuf%2 ;
  adbitbuf>>=1 ;
  return result ;
}

/*************************** Boolean *************************/

void pushBoolean(int x) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(x) ;
}

//[llh] I have a bug here: the boolean returned to Fortran is bizarre!
void popBoolean(int *x) {
  *x = popBit() ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

/************************* Control ***********************/

void pushControl1b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc) ;
}

void popControl1b(int *cc) {
  *cc = popBit() ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl2b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl2b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl3b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl3b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl4b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl4b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl5b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl5b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl6b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl6b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl7b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl7b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

void pushControl8b(int cc) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc%2) ;
  cc>>=1 ;
  pushBit(cc) ;
}

void popControl8b(int *cc) {
  *cc = (popBit()?2:0) ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  (*cc) <<= 1 ;
  if (popBit()) (*cc)++ ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

uint64_t adStack_getCurrentStackSize() {
 return curStack ? (curStack->rank-1)*ADSTACK_BLOCK_SIZE + tappos : 0;
}

/******** Various dump routines for profiling and debugging *********/

void showLocation(DoubleChainedBlock *locBlock, int loc) {
  printf("%1i.%05i", (locBlock ? locBlock->rank-1 : 0), loc) ;
}

#ifdef ADSTACK_TRACE
void dumpStackPrev(DoubleChainedBlock *st) {
  if (st) {
    if (st->rank==st->toContents->rank) { // Don't dump too far back...
      dumpStackPrev(st->prev) ;
    }
    printf("-->[%02i ", st->rank) ;
    if (st->toContents) {
      printf("(%c: %02i %s)",
             'a'+st->toContents->id,
             st->toContents->rank,
             stateNames[st->toContents->state]) ;
    } else {
      printf("null") ;
    }
    printf("]") ;
  }
}

void dumpStackNext(DoubleChainedBlock *st) {
  if (st) {
    printf("..>[%02i ", st->rank) ;
    if (st->toContents) {
      printf("(%c: %02i %s)",
             'a'+st->toContents->id,
             st->toContents->rank,
             stateNames[st->toContents->state]) ;
    } else {
      printf("null") ;
    }
    printf("]") ;
    if (st->rank==st->toContents->rank) { // Don't dump too far...
      dumpStackNext(st->next) ;
    }
  }
}

void dumpStack(DoubleChainedBlock *st) {
  if (st) {
    printf("           ") ;
    dumpStackPrev(st->prev) ;
    printf(">>>[%02i ", st->rank) ;
    if (st->toContents) {
      printf("(%c: %02i %s)",
             'a'+st->toContents->id,
             st->toContents->rank,
             stateNames[st->toContents->state]) ;
    } else {
      printf("null") ;
    }
    printf("]") ;
    dumpStackNext(st->next) ;
  }
}

void dumpFileActionQueue() {
  FileActionCell *inQueue = fileActionQueueHead ;
  if (!inQueue) {
    printf(" empty") ;
  } else {
    while (inQueue) {
      if (inQueue->action==STORE) {
        printf(" store (%c: %02i %s)",
               'a'+inQueue->space->id,
               inQueue->space->rank,
               stateNames[inQueue->space->state]) ;
      } else if (inQueue->action==RESTORE) {
        printf(" restore %02i into (%c: %02i %s)",
               inQueue->restoredRank,
               'a'+inQueue->space->id,
               inQueue->space->rank,
               stateNames[inQueue->space->state]) ;
      }
      inQueue = inQueue->next ;
    }
  }
}
#endif

void dumpContents(BlockContents* space) {
  for (int i=0 ; i<ADSTACK_BLOCK_SIZE ; ++i) {
    printf("%02x", (unsigned char)space->contents[i]) ;
    if (i%10==9) printf(" ") ;
    if (i%50==49) printf("\n") ;
  }
}

void dumpRepetitionLevels() {
  RepetitionLevel *repetitionPoint = topRepetitionPoint ;
  while (repetitionPoint) {
    printf("  REPETITION LEVEL ACTIVE:%s BP:%s",
           (repetitionPoint->active?"yes":"no"),
           (repetitionPoint->hasBackPop?"yes":"no")) ;
    if (repetitionPoint->hasBackPop)
      {printf(" BP:") ; showLocation(repetitionPoint->backPopBlock, repetitionPoint->backPop) ;}
    if (repetitionPoint->resumePointBlock)
      {printf(" RP:") ; showLocation(repetitionPoint->resumePointBlock, repetitionPoint->resumePoint) ;}
    if (repetitionPoint->freePushBlock)
      {printf(" FP:") ; showLocation(repetitionPoint->freePushBlock, repetitionPoint->freePush) ;}
    printf("\n") ;
    repetitionPoint = repetitionPoint->previous ;
    if (repetitionPoint) printf("  ...in") ;
  }
}

void showErrorContext() {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  printf("t%09i ERROR AT:", ts.tv_nsec) ; showLocation(curStack, tappos) ;
#ifdef ADSTACK_TRACE
  printf(" STACK:") ; dumpStack(curStack) ; printf("\n") ;
  printf("FILE ACTION QUEUE:\n") ;
  dumpFileActionQueue() ; printf("\n") ;
#endif
  printf("REPETITION LEVELS:\n") ;
  dumpRepetitionLevels() ;
}

void adStack_showPeakSize() {
  printf("Peak stack size (%1li blocks): %1llu bytes\n",
         maxBlocks, maxBlocks*((long int)ADSTACK_BLOCK_SIZE)) ;
}

void adStack_showTotalTraffic() {
  printf("Total push/pop traffic %1lu bytes\n", pushPopTraffic) ;
}

void adStack_showStackSize(int label) {
  printf(" %i--> <",label) ;
  printf("%"PRId64"", adStack_getCurrentStackSize());
/*   showLocation(curStack, tappos) ; */
  printf(">") ;
}

void adStack_showStack(char *locationName) {
  if (!curStack || (tappos==0 && !curStack->prev)) {
    printf ("Stack at %s is empty\n", locationName) ;
  } else {
    printf ("Stack top at %s is %1i.%05i :\n", locationName, curStack->rank, tappos) ;
    int bytesToShow = 20 ;
    int blocksToShow = 3 ;
    DoubleChainedBlock *inStack = curStack ;
    int inPos = tappos ;
    while (blocksToShow>0 && inStack) {
      printf("  Block %d:", inStack->rank) ;
      while (bytesToShow>0 && inPos>0) {
        printf(" %02x", (unsigned char)inStack->toContents->contents[--inPos]) ;
        --bytesToShow ;
      }
      if (inPos>0)
        printf(" ...<%d more bytes>...", inPos) ;
      printf(" |\n") ;
      --blocksToShow ;
      inStack = inStack->prev ;
      inPos = ADSTACK_BLOCK_SIZE ;
    }
    if (inStack)
      printf("  %d more blocks below\n", inStack->rank) ;
  }
  if (adbitibuf==0) {
    printf("Bit buffer is empty\n") ;
  } else {
    printf("Bit buffer:%1i in %08x\n", adbitibuf, adbitbuf) ;
  }
  if (topRepetitionPoint) {
    printf("Repetition levels:\n  ") ;
    dumpRepetitionLevels() ;
  }
  printf("----------------\n") ;
}

/******* Query if this stack was compiled with OpenMP ******/

int stackIsThreadSafe() {
  #ifdef _OPENMP
    return 1 ;
  #else
    return 0 ;
  #endif
}

/****************** INTERFACE CALLED FROM FORTRAN *******************/

void adstack_startrepeat_() {
  adStack_startRepeat() ;
}

void adstack_resetrepeat_() {
  adStack_resetRepeat() ;
}

void adstack_endrepeat_() {
  adStack_endRepeat() ;
}

void pushinteger4array_(int *ii, int *ll) {
  pushInteger4Array(ii, *ll) ;
}

void popinteger4array_(int *ii, int *ll) {
  popInteger4Array(ii, *ll) ;
}

void pushinteger8array_(long *ii, int *ll) {
  pushInteger8Array(ii, *ll) ;
}

void popinteger8array_(long *ii, int *ll) {
  popInteger8Array(ii, *ll) ;
}

void pushreal4array_(float *ii, int *ll) {
  pushReal4Array(ii, *ll) ;
}

void popreal4array_(float *ii, int *ll) {
  popReal4Array(ii, *ll) ;
}

void pushreal8array_(double *ii, int *ll) {
  pushReal8Array(ii, *ll) ;
}

void popreal8array_(double *ii, int *ll) {
  popReal8Array(ii, *ll) ;
}

void pushreal16array_(long double *ii, int *ll) {
  pushReal16Array(ii, *ll) ;
}

void popreal16array_(long double *ii, int *ll) {
  popReal16Array(ii, *ll) ;
}

void pushcomplex8array_(ccmplx *ii, int *ll) {
  pushComplex8Array(ii, *ll) ;
}

void popcomplex8array_(ccmplx *ii, int *ll) {
  popComplex8Array(ii, *ll) ;
}

void pushcomplex16array_(cdcmplx *ii, int *ll) {
  pushComplex16Array((double complex *)ii, *ll) ;
}

void popcomplex16array_(cdcmplx *ii, int *ll) {
  popComplex16Array((double complex *)ii, *ll) ;
}

void pushcharacterarray_(char *ii, int *ll) {
  pushCharacterArray(ii, *ll) ;
}

void popcharacterarray_(char *ii, int *ll) {
  popCharacterArray(ii, *ll) ;
}

void pushbooleanarray_(char *x, int *n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray(x,(*n*4)) ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += *n*4 ;
#endif
}

void popbooleanarray_(char *x, int *n) {
  popNArray(x,(*n*4)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef ADSTACK_PROFILE
  pushPopTraffic += *n*4 ;
#endif
}

void pushcharacter_(char* val) {
  pushCharacter(*val) ;
}

void popcharacter_(char* val) {
  popCharacter(val) ;
}

void pushreal4_(float* val) {
  pushReal4(*val) ;
}

void popreal4_(float* val) {
  popReal4(val) ;
}

void pushreal8_(double* val) {
  pushReal8(*val) ;
}

void popreal8_(double* val) {
  popReal8(val) ;
}

void pushreal16_(long double *val) {
  pushReal16(val) ;
}

void popreal16_(long double *val) {
  popReal16(val) ;
}

void pushinteger4_(int* val) {
  pushInteger4(*val) ;
}

void popinteger4_(int* val) {
  popInteger4(val) ;
}

void pushinteger8_(long* val) {
  pushInteger8(*val) ;
}

void popinteger8_(long* val) {
  popInteger8(val) ;
}

void pushcomplex8_(ccmplx* val) {
  pushComplex8(*val) ;
}

void popcomplex8_(ccmplx* val) {
  popComplex8(val) ;
}

void pushcomplex16_(cdcmplx *val) {
  pushComplex16(*((double complex *)val)) ;
}

void popcomplex16_(cdcmplx* val) {
  popComplex16((double complex *)val) ;
}

void pushpointer4_(void** val) {
  pushPointer4(*val) ;
}

void poppointer4_(void** val) {
  popPointer4(val) ;
}

void pushpointer8_(void** val) {
  pushPointer8(*val) ;
}

void poppointer8_(void** val) {
  popPointer8(val) ;
}

void pushcontrol1b_(int* cc) {
  pushControl1b(*cc) ;
}

void popcontrol1b_(int *cc) {
  popControl1b(cc) ;
}

void pushcontrol2b_(int *cc) {
  pushControl2b(*cc) ;
}

void popcontrol2b_(int *cc) {
  popControl2b(cc) ;
}

void pushcontrol3b_(int *cc) {
  pushControl3b(*cc) ;
}

void popcontrol3b_(int *cc) {
  popControl3b(cc) ;
}

void pushcontrol4b_(int *cc) {
  pushControl4b(*cc) ;
}

void popcontrol4b_(int *cc) {
  popControl4b(cc) ;
}

void pushcontrol5b_(int *cc) {
  pushControl5b(*cc) ;
}

void popcontrol5b_(int *cc) {
  popControl5b(cc) ;
}

void pushcontrol6b_(int *cc) {
  pushControl6b(*cc) ;
}

void popcontrol6b_(int *cc) {
  popControl6b(cc) ;
}

void pushcontrol7b_(int *cc) {
  pushControl7b(*cc) ;
}

void popcontrol7b_(int *cc) {
  popControl7b(cc) ;
}

void pushcontrol8b_(int *cc) {
  pushControl8b(*cc) ;
}

void popcontrol8b_(int *cc) {
  popControl8b(cc) ;
}

void pushboolean_(int *x) {
  pushBoolean(*x) ;
}

void popboolean_(int *x) {
  popBoolean(x) ;
}

void adstack_showpeaksize_() {
  adStack_showPeakSize() ;
}

void adstack_showtotaltraffic_() {
  adStack_showTotalTraffic() ;
}

void adstack_showstacksize_(int *label) {
  adStack_showStackSize(*label) ;
}

void adstack_showstack_(char *locationName) {
  adStack_showStack(locationName) ;
}

int stackisthreadsafe_() {
  return stackIsThreadSafe() ;
}

void showerrorcontext_() {
  showErrorContext() ;
  fflush(stdout) ;
}

int locstrb_() {return (curStack ? curStack->rank : 0) ;}
int locstro_() {return tappos;}
