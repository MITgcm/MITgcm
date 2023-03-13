#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "adStack.h"

#include "adComplex.h"

// Set to 0 to hide trace.
static int traceOn = 0 ;
static int freeemptyblocks = 1 ;

char* pushBlock() ;
char* popBlock() ;

/**************** block sizes for all data types *************/

/* The size of a BLOCK in bytes. */
#define BLOCK_SIZE 65536
// #define BLOCK_SIZE 17 // A very small BLOCK_SIZE allows for stronger testing!

/**************** data structures for stack ******************/

/* The main stack is a double-chain of DoubleChainedBlock objects.
 * Each DoubleChainedBlock holds an array[BLOCK_SIZE] of char. */
typedef struct _DoubleChainedBlock{
  unsigned int rank ;
  struct _DoubleChainedBlock *prev ;
  struct _DoubleChainedBlock *next ;
  char contents[BLOCK_SIZE] ;
} DoubleChainedBlock ;

/** Structure keeping the needed info for ONE repetition level.
 * As we can have "nested" repetition levels, these structures can be stacked. */
typedef struct _RepetitionLevel {
  int hasBackPop ;
  int active ;
  DoubleChainedBlock* backPopBlock ;
  int backPop ;
  DoubleChainedBlock* resumePointBlock ;
  int resumePoint ;
  DoubleChainedBlock* freePushBlock ;
  int freePush ;
  unsigned int storedadbitbuf ;
  int storedadbitibuf ;
  struct _RepetitionLevel *previous ;
} RepetitionLevel ;

/* A block and an integer to keep the current top in the block. When the block
   is full, it is added to the double-chain list and a fresh block is used for
   pushing. During popping, empty blocks are replenished with data from the
   double-chain list. */
static int tappos  = BLOCK_SIZE ;
static char* tapblock = NULL ;
static DoubleChainedBlock *curStack = NULL ;

/** The current stack of repetition levels. Initially empty */
RepetitionLevel *topRepetitionPoint = NULL ;

/* Pushing single bits is different from the rest: we collect
   32 bits in the integer below, before pushing that to the stack. */
static unsigned int adbitbuf = 0 ;
static int adbitibuf = 0 ;

/** Accumulates the number of bytes pushed and popped */
static unsigned long long int pushPopTraffic = 0 ;

/** Remembers the maximum number of stack Blocks used */
static long int maxBlocks = 0 ;

//[llh] Don't know how to manage pushPopTraffic and maxBlocks in the OpenMP case ?

/* All data structures must be threadprivate, so that each OpenMP thread has
   its own stack. If the stack is compiled with OpenMP support and then used
   in a program with no OpenMP parallel regions, the stack will work as
   expected without using any extra resources. */
#pragma omp threadprivate(tappos, tapblock, curStack, adbitbuf, adbitibuf, topRepetitionPoint)

/***************** repeated access mechanism *************/

// Possible improvements:
//  - replace tappos with tapblock+tappos, saving a few "+" ?
//  - find a faster currentLocationStrictBelowFreePush and currentLocationEqualsFreePush

// Notice: Algorithm for "nested" repetition level seems wrong (should be corrected)
//  when the deeper repetition level is started after new pushes.

// We call "current location" the pair of
// (DoubleChainedBlock *) curStack // the current top stack block.
// (int) tappos                    // the offset of the current top in the current top stack block.

void setBackPopToCurrentLocation(RepetitionLevel *repetitionLevel) {
  repetitionLevel->hasBackPop = 1 ;
  repetitionLevel->backPopBlock = curStack ;
  repetitionLevel->backPop = tappos ;
}

void setCurrentLocationToBackPop(RepetitionLevel *repetitionLevel) {
  curStack = repetitionLevel->backPopBlock ;
  tapblock = curStack->contents ;
  tappos = repetitionLevel->backPop ;
}

void setResumePointToCurrentLocation(RepetitionLevel *repetitionLevel) {
  repetitionLevel->resumePointBlock = curStack ;
  repetitionLevel->resumePoint = tappos ;
}

void setCurrentLocationToResumePoint(RepetitionLevel *repetitionLevel) {
  curStack = repetitionLevel->resumePointBlock ;
  tapblock = curStack->contents ;
  tappos = repetitionLevel->resumePoint ;
} 

void setFreePushToCurrentLocation(RepetitionLevel *repetitionLevel) {
  repetitionLevel->freePushBlock = curStack ;
  repetitionLevel->freePush = tappos ;
}

void setCurrentLocationToFreePush(RepetitionLevel *repetitionLevel) {
  curStack = repetitionLevel->freePushBlock ;
  tapblock = curStack->contents ;
  tappos = repetitionLevel->freePush ;
} 

//TODO: try inline this function for efficiency:
int currentLocationStrictBelowFreePush(RepetitionLevel *repetitionLevel) {
  //[llh] I'd prefer to test directly on curStack->rank and tappos directly,
  // but it's hard because N;BLOCK_SIZE <=> N+1;0 and both happen due to initial NULL curStack...
  long int curL = (curStack->rank -1)*BLOCK_SIZE + tappos ;
  long int fpL = (repetitionLevel->freePushBlock->rank -1)*BLOCK_SIZE + repetitionLevel->freePush ;
  return (curL<fpL) ;
}

//TODO: try inline this function for efficiency:
int currentLocationEqualsFreePush(RepetitionLevel *repetitionLevel) {
  //[llh] I'd prefer to test directly on curStack->rank and tappos directly,
  // but it's hard because N;BLOCK_SIZE <=> N+1;0 and both happen due to initial NULL curStack...
  long int curL = (curStack->rank -1)*BLOCK_SIZE + tappos ;
  long int fpL = (repetitionLevel->freePushBlock->rank -1)*BLOCK_SIZE + repetitionLevel->freePush ;
  return (curL==fpL) ;
}

void showLocation(DoubleChainedBlock *locBlock, int loc) {
  printf("%1i.%05i", (locBlock ? locBlock->rank : 0), loc) ;
}

void showRepetitionLevels() {
  RepetitionLevel *repetitionPoint = topRepetitionPoint ;
  while (repetitionPoint) {
    printf("  REPETITION LEVEL ACTIVE:%i BP?%i", repetitionPoint->active, repetitionPoint->hasBackPop) ;
    printf(" BP:") ; showLocation(repetitionPoint->backPopBlock, repetitionPoint->backPop) ;
    printf(" RP:") ; showLocation(repetitionPoint->resumePointBlock, repetitionPoint->resumePoint) ;
    printf(" FP:") ; showLocation(repetitionPoint->freePushBlock, repetitionPoint->freePush) ;
    printf("\n") ;
    repetitionPoint = repetitionPoint->previous ;
    if (repetitionPoint) printf("  ...in") ;
  }
}

int locstrb_() {return (curStack ? curStack->rank : 0) ;}
int locstro_() {return tappos;}

/** If we are in a protected, read-only section,
 * memorize current location as "backPop" and go to the "freePush" location */
void checkPushInReadOnly() {
  RepetitionLevel *topActive = topRepetitionPoint ;
  while (topActive && !topActive->active) {
    topActive = topActive->previous ;
  }
  if (topActive && currentLocationStrictBelowFreePush(topActive)) {
    setBackPopToCurrentLocation(topActive) ;
    setCurrentLocationToFreePush(topActive) ;
    if (traceOn) {
      printf("BEFORE PUSH AT ") ;
      showLocation(topRepetitionPoint->backPopBlock, topRepetitionPoint->backPop) ;
      printf("  WITH REPETITION LEVELS:\n") ;
      showRepetitionLevels() ;
      printf("  MOVE TO FREE PUSH LOCATION ") ;
      showLocation(topRepetitionPoint->freePushBlock, topRepetitionPoint->freePush) ;
      printf("\n") ;
    }
  }
}

/** If current location is some "freePush" location,
 * go back to its "backPop" location, which is in a protected, read-only section */
void checkPopToReadOnly() {
  RepetitionLevel *repetitionPoint = topRepetitionPoint ;
  int moves = (repetitionPoint->hasBackPop && currentLocationEqualsFreePush(repetitionPoint)) ;
  if (traceOn && moves) {
    printf("AFTER POP, LOCATION WAS ") ;
    showLocation(curStack, tappos) ;
    printf("  WITH REPETITION LEVELS:\n") ;
    showRepetitionLevels() ;
  }
  int canEraseInactive = 1 ;
  int canRemoveBackPop = 1 ;
  do {
    RepetitionLevel *oldCell = repetitionPoint ;
    if (oldCell->hasBackPop && oldCell->active && currentLocationEqualsFreePush(oldCell)) {
      setCurrentLocationToBackPop(oldCell) ;
      if (canRemoveBackPop) oldCell->hasBackPop = 0 ;
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
  if (traceOn && moves) {
    printf("  MOVED TO BACK POP LOCATION:") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
  }
}

/** From now on, and until the matching closing adStack_endRepeat(),
 * the current contents of the push-pop stack are preserved:
 * Even if they are popped, any subsequent adStack_resetRepeat()
 * will reset the push-pop stack to its current contents of now. */
void adStack_startRepeat() {
  if (traceOn) {
    printf("BEFORE START REPEAT AT ") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
    showRepetitionLevels() ;
  }
  // Create a new repetition level and push it onto topRepetitionPoint
  RepetitionLevel *newRepetitionLevel = (RepetitionLevel *)malloc(sizeof(RepetitionLevel)) ;
  newRepetitionLevel->previous = topRepetitionPoint ;
  newRepetitionLevel->hasBackPop = 0 ;
  newRepetitionLevel->active = 1 ;
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
  if (traceOn) {
    printf(">AFTER START REPEAT AT:") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
    showRepetitionLevels() ;
  }
}

/** Reset the push-pop stack contents to its contents at
 * the time of the latest adStack_startRepeat() that has not been
 * closed by a subsequent adStack_endRepeat() */
void adStack_resetRepeat() {
  if (traceOn) {
    printf("BEFORE RESET REPEAT AT ") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
    showRepetitionLevels() ;
  }
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
  if (traceOn) {
    printf(">AFTER RESET REPEAT AT ") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
    showRepetitionLevels() ;
  }
}

/** Close (i.e. remove) the repetition level created by the latest adStack_startRepeat(). */
void adStack_endRepeat() {
  if (traceOn) {
    printf("BEFORE END REPEAT AT ") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
    showRepetitionLevels() ;
  }
  // Set to inactive the topmost active repetition level:
  RepetitionLevel *topActive = topRepetitionPoint ;
  while (!topActive->active) {
    topActive = topActive->previous ;
  }
  topActive->active = 0 ;
  // current location may have moved back ; check if we must move further back:
  if (topRepetitionPoint) checkPopToReadOnly() ;
  if (traceOn) {
    printf(">AFTER END REPEAT AT ") ;
    showLocation(curStack, tappos) ;
    printf("\n") ;
    showRepetitionLevels() ;
  }
}

/***************** double-linked list management *************/

/** add a data block to the double-linked list */
char* pushBlock() {
  if (curStack && curStack->next) {
    curStack = curStack->next ;
  } else {
    DoubleChainedBlock *newStack = (DoubleChainedBlock*)malloc(sizeof(DoubleChainedBlock)) ;
    if (newStack == NULL) {
      /* We ran out of memory, print an error message and give up. */
      printf("Out of memory in AD Stack.\n") ;
      exit(0) ;
    }
    if(curStack != NULL) {
      curStack->next = newStack ;
      newStack->rank = curStack->rank + 1 ;
    } else {
      newStack->rank = 1 ;
    }

    newStack->prev = curStack ;
    newStack->next = NULL ;
    curStack = newStack ;
  }
#ifdef _ADSTACKPROFILE
  if (curStack->rank > maxBlocks) maxBlocks = curStack->rank ;
#endif
  return curStack->contents ;
}

/** retrieve a data block from the double-linked list */
char* popBlock() {
  DoubleChainedBlock *oldTopStack = curStack ;
  curStack = curStack->prev ;
  if (freeemptyblocks) {
    // Not necessary. Only needed if we want to free when the stack goes down:
    // We must not free if we are in a repetition level and below its freePush point.
    if (!(topRepetitionPoint && oldTopStack->rank <= topRepetitionPoint->freePushBlock->rank)) {
      free(oldTopStack) ;
      if (curStack) curStack->next = NULL ;
    }
    // end "Not necessary"
  }
  return (curStack ? curStack->contents : NULL) ;
}

/********************* push/pop arrays ***********************/

/* pushNArray/popNArray are used not only to store arrays of various data
   types. These functions are also the only ones that interact with the dynamic
   memory management, e.g. requesting new blocks. If one of the scalar push/pop
   functions (e.g. pushReal4) encounters the end of a block, it will ask
   pushNArray to do all the work, i.e. start a new block and push the real4
   value to it. */
void pushNArray(char *x, int nbChars) {
  do {
    int wsize = tappos+nbChars<BLOCK_SIZE?nbChars:BLOCK_SIZE-tappos ;
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
      tappos = BLOCK_SIZE ;
    }
  } while(nbChars > 0) ; //=> lazy pop: if finishes at the bottom of block contents, does not pop block.
}

void pushInteger4Array(int *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*4)) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void popInteger4Array(int *x, int n) {
  popNArray((char *)x,(int)(n*4)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void pushInteger8Array(long *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*8)) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void popInteger8Array(long *x, int n) {
  popNArray((char *)x,(int)(n*8)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void pushReal4Array(float *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*4)) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void popReal4Array(float *x, int n) {
  popNArray((char *)x,(int)(n*4)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*4) ;
#endif
}

void pushReal8Array(double *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*8)) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void popReal8Array(double *x, int n) {
  popNArray((char *)x,(int)(n*8)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void pushComplex8Array(ccmplx *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*8)) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void popComplex8Array(ccmplx *x, int n) {
  popNArray((char *)x,(int)(n*8)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*8) ;
#endif
}

void pushComplex16Array(double complex *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray((char *)x,(int)(n*16)) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*16) ;
#endif
}

void popComplex16Array(double complex *x, int n) {
  popNArray((char *)x,(int)(n*16)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)(n*16) ;
#endif
}

void pushCharacterArray(char *x, int n) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushNArray(x,(int)n) ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)n ;
#endif
}

void popCharacterArray(char *x, int n) {
  popNArray(x,(int)n) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
  pushPopTraffic += (int)n ;
#endif
}

/***************** scalar push/pop functions *****************/

void pushCharacter(char val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 1 > BLOCK_SIZE) {
    pushNArray((char*)&val, 1) ;
  }
  else {
    *(char*)(tapblock+tappos) = val;
    tappos = tappos + 1 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 1 ;
#endif
}

void pushReal4(float val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 4 > BLOCK_SIZE) {
    pushNArray((char*)&val, 4) ;
  }
  else {
    *(float*)(tapblock+tappos) = val;
    tappos = tappos + 4 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 4 ;
#endif
}

void pushReal8(double val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(double*)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 8 ;
#endif
}

void pushInteger4(int val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 4 > BLOCK_SIZE) {
    pushNArray((char*)&val, 4) ;
  }
  else {
    *(int*)(tapblock+tappos) = val;
    tappos = tappos + 4 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 4 ;
#endif
}

void pushInteger8(long val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(long*)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 8 ;
#endif
}

void pushComplex8(ccmplx val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(ccmplx*)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 8 ;
#endif
}

void pushComplex16(double complex val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 16 > BLOCK_SIZE) {
    pushNArray((char*)&val, 16) ;
  }
  else {
    *(double complex *)(tapblock+tappos) = val;
    tappos = tappos + 16 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 16 ;
#endif
}

void pushPointer4(void * val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 4 > BLOCK_SIZE) {
    pushNArray((char*)&val, 4) ;
  }
  else {
    *(void**)(tapblock+tappos) = val;
    tappos = tappos + 4 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 4 ;
#endif
}

void pushPointer8(void * val) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  if(tappos + 8 > BLOCK_SIZE) {
    pushNArray((char*)&val, 8) ;
  }
  else {
    *(void**)(tapblock+tappos) = val;
    tappos = tappos + 8 ;
  }
#ifdef _ADSTACKPROFILE
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += 8 ;
#endif
}

/******************* bit (hidden primitives) ***************/

void pushBit(int x) {
  adbitbuf<<=1 ;
  if (x) ++adbitbuf ;
  if (adbitibuf>=31) {
    pushNArray((char *)&adbitbuf, 4) ;
    adbitbuf = 0 ;
    adbitibuf = 0 ;
#ifdef _ADSTACKPROFILE
    pushPopTraffic += 4 ;
#endif
  } else
    ++adbitibuf ;
}

int popBit() {
  if (adbitibuf<=0) {
    popNArray((char *)&adbitbuf, 4) ;
    adbitibuf = 31 ;
#ifdef _ADSTACKPROFILE
    pushPopTraffic += 4 ;
#endif
  } else
    --adbitibuf ;
  int result = adbitbuf%2 ;
  adbitbuf>>=1 ;
  return result ;
}

/*************************** boolean *************************/

void pushBoolean(int x) {
  if (topRepetitionPoint) checkPushInReadOnly() ;
  pushBit(x) ;
}

//[llh] I have a bug here: the boolean returned to Fortran is bizarre!
void popBoolean(int *x) {
  *x = popBit() ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
}

/************************* control ***********************/

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

/****************** Profiling and debugging *******************/

void adStack_showPeakSize() {
  printf("Peak stack size (%1li blocks): %1llu bytes\n",
         maxBlocks, maxBlocks*((long long int)BLOCK_SIZE)) ;
}

void adStack_showTotalTraffic() {
  printf("Total push/pop traffic %1llu bytes\n", pushPopTraffic) ;
}

void adStack_showStackSize(int label) {
  printf(" %i--> <",label) ;
  showLocation(curStack, tappos) ;
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
        printf(" %02x", (unsigned char)inStack->contents[--inPos]) ;
        --bytesToShow ;
      }
      if (inPos>0)
        printf(" ...<%d more bytes>...", inPos) ;
      printf(" |\n") ;
      --blocksToShow ;
      inStack = inStack->prev ;
      inPos = BLOCK_SIZE ;
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
    showRepetitionLevels() ;
  }
  printf("----------------\n") ;
}

/******* query if this stack was compiled with OpenMP ******/
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
#ifdef _ADSTACKPROFILE
  pushPopTraffic += *n*4 ;
#endif
}

void popbooleanarray_(char *x, int *n) {
  popNArray(x,(*n*4)) ;
  if (topRepetitionPoint) checkPopToReadOnly() ;
#ifdef _ADSTACKPROFILE
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

void pushboolean_(int *x) {
  pushBoolean(*x) ;
}

void popboolean_(int *x) {
  popBoolean(x) ;
}

int stackisthreadsafe_() {
  return stackIsThreadSafe() ;
}
