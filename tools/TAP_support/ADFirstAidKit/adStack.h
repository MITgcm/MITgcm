#ifndef ADSTACK_INCLUDED
#define ADSTACK_INCLUDED

#include "complex.h"
#include <stdint.h>

void adStack_startRepeat() ;
void adStack_resetRepeat() ;
void adStack_endRepeat() ;

/* char* pushBlock() ; */
/* char* popBlock() ; */
/* void pushNArray(char *x, int nbChars) ; */
/* void popNArray(char *x, int nbChars) ; */

void pushInteger4Array(int *x, int n) ;
void popInteger4Array(int *x, int n) ;
void pushInteger8Array(long *x, int n) ;
void popInteger8Array(long *x, int n) ;
void pushReal4Array(float *x, int n) ;
void popReal4Array(float *x, int n) ;
void pushReal8Array(double *x, int n) ;
void popReal8Array(double *x, int n) ;
void pushReal16Array(long double *x, int n) ;
void popReal16Array(long double *x, int n) ;
/* Commented out because sizeof(complex) == sizeof(double complex) == 16 */
/* void pushComplex8Array(complex *x, int n) ; */
/* void popComplex8Array(complex *x, int n) ; */
void pushComplex16Array(double complex *x, int n) ;
void popComplex16Array(double complex *x, int n) ;
void pushCharacterArray(char *x, int n) ;
void popCharacterArray(char *x, int n) ;

void pushCharacter(char val) ;
void popCharacter(char* val) ;
void pushReal4(float val) ;
void popReal4(float* val) ;
void pushReal8(double val) ;
void popReal8(double* val) ;
void pushReal16(long double *val) ;
void popReal16(long double *val) ;
void pushInteger4(int val) ;
void popInteger4(int* val) ;
void pushInteger8(long val) ;
void popInteger8(long* val) ;
/* Commented out because sizeof(complex) == sizeof(double complex) == 16 */
/* void pushComplex8(complex val) ; */
/* void popComplex8(complex *val) ; */
void pushComplex16(double complex val) ;
void popComplex16(double complex *val) ;
void pushPointer4(void* val) ;
void popPointer4(void** val) ;
void pushPointer8(void* val) ;
void popPointer8(void** val) ;
void pushBoolean(int x) ;
void popBoolean(int *x) ;

void pushBit(int x) ;
int popBit() ;

void pushControl1b(int cc) ;
void popControl1b(int *cc) ;
void pushControl2b(int cc) ;
void popControl2b(int *cc) ;
void pushControl3b(int cc) ;
void popControl3b(int *cc) ;
void pushControl4b(int cc) ;
void popControl4b(int *cc) ;
void pushControl5b(int cc) ;
void popControl5b(int *cc) ;
void pushControl6b(int cc) ;
void popControl6b(int *cc) ;
void pushControl7b(int cc) ;
void popControl7b(int *cc) ;
void pushControl8b(int cc) ;
void popControl8b(int *cc) ;

void adStack_showPeakSize() ;
void adStack_showTotalTraffic() ;
void adStack_showStackSize(int label) ;
void adStack_showStack(char *locationName) ;

uint64_t adStack_getCurrentStackSize();

int stackIsThreadSafe() ;
#endif
