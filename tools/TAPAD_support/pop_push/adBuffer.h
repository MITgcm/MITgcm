#ifndef ADBUFFER_LOADED
#define ADBUFFER_LOADED 1

#include "adStack.h"

typedef struct {float r,i;} ccmplx ;
typedef struct {double dr, di;} cdcmplx ;

/** Push an int (4 bytes int) */
extern void pushInteger4(int x) ;

/** Pop an int (4 bytes int) */
extern void popInteger4(int *x) ;

/** Push a long (8 bytes int) */
extern void pushInteger8(long x) ;

/** Pop a long (8 bytes int) */
extern void popInteger8(long *x) ;

/** Push a float (4 bytes real) */
extern void pushReal4(float x) ;

/** Pop a float (4 bytes real) */
extern void popReal4(float *x) ;

/** Push a double (8 bytes real) */
extern void pushReal8(double x) ;

/** Pop a double (8 bytes real) */
extern void popReal8(double *x) ;

/** Push a complex (4 bytes per component) */
extern void pushComplex8(ccmplx x) ;

/** Pop a complex (4 bytes per component) */
extern void popComplex8(ccmplx *x) ;

/** Push a double complex (8 bytes per component) */
extern void pushComplex16(cdcmplx x) ;

/** Pop a double complex (8 bytes per component) */
extern void popComplex16(cdcmplx *x) ;

/** Push a char (byte) */
extern void pushCharacter(char x) ;

/** Pop a char (byte) */
extern void popCharacter(char *x) ;

/** Push a boolean (one bit) */
extern void pushBoolean(int x) ;

/** Pop a boolean (one bit) */
extern void popBoolean(int *x) ;

/** Push a value ranging in [0,1] */
extern void pushControl1b(int cc) ;

/** Pop a value ranging in [0,1] */
extern void popControl1b(int *cc) ;

/** Push a value ranging in [0,3] */
extern void pushControl2b(int cc) ;

/** Pop a value ranging in [0,3] */
extern void popControl2b(int *cc) ;

/** Push a value ranging in [0,7] */
extern void pushControl3b(int cc) ;

/** Pop a value ranging in [0,7] */
extern void popControl3b(int *cc) ;

/** Push a value ranging in [0,15] */
extern void pushControl4b(int cc) ;

/** Pop a value ranging in [0,15] */
extern void popControl4b(int *cc) ;

/** Push a value ranging in [0,31] */
extern void pushControl5b(int cc) ;

/** Pop a value ranging in [0,31] */
extern void popControl5b(int *cc) ;

/** Push a value ranging in [0,63] */
extern void pushControl6b(int cc) ;

/** Pop a value ranging in [0,63] */
extern void popControl6b(int *cc) ;

/** Push a value ranging in [0,127] */
extern void pushControl7b(int cc) ;

/** Pop a value ranging in [0,127] */
extern void popControl7b(int *cc) ;

/** Push a value ranging in [0,255] */
extern void pushControl8b(int cc) ;

/** Pop a value ranging in [0,255] */
extern void popControl8b(int *cc) ;

/** Push a 32 bits pointer */
extern void pushPointer4(void *x) ;

/** Pop a 32 bits pointer */
extern void popPointer4(void **x) ;

/** Push a 64 bits pointer */
extern void pushPointer8(void *x) ;

/** Pop a 64 bits pointer */
extern void popPointer8(void **x) ;

/** From now on, everything below current stack top may be read repeated times.
 * This opens a so-called new repeated-access level */
extern void adStack_startRepeat() ;

/** Reset the stack top to the deepest enclosing repeated-access level */
extern void adStack_resetRepeat() ;

/** Return the deepest enclosing repeated-access level to normal stack behavior */
extern void adStack_endRepeat() ;

/** Display the total amount of memory pushed */
extern void adStack_showTraffic() ;

/** Display in detail the contents of the AD stack,
 * followed by the detailed contents of each type buffer.
 * Also show the stack of nested repeated-access levels */
extern void showStackAndBuffers(char *locationName) ;

/** Display the current size of the AD stack as
 * <#blocks.#bytesInTopBlock> then size of each type buffers */
extern void showStackAndBuffersSize() ;

#endif
