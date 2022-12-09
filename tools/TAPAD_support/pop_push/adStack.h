#ifndef ADSTACK_H
#define ADSTACK_H
typedef struct {float r,i;} ccmplx ;
typedef struct {double dr, di;} cdcmplx ;

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
void pushComplex8Array(ccmplx *x, int n) ;
void popComplex8Array(ccmplx *x, int n) ;
void pushComplex16Array(cdcmplx *x, int n) ;
void popComplex16Array(cdcmplx *x, int n) ;
void pushCharacterArray(char *x, int n) ;
void popCharacterArray(char *x, int n) ;

void pushCharacter(char val) ;
void popCharacter(char* val) ;
void pushReal4(float val) ;
void popReal4(float* val) ;
void pushReal8(double val) ;
void popReal8(double* val) ;
void pushInteger4(int val) ;
void popInteger4(int* val) ;
void pushInteger8(long val) ;
void popInteger8(long* val) ;
void pushComplex8(ccmplx val) ;
void popComplex8(ccmplx* val) ;
void pushComplex16(cdcmplx val) ;
void popComplex16(cdcmplx* val) ;
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
void adStack_showStackSize() ;
void adStack_showStack(char *locationName) ;

int stackIsThreadSafe() ;
#endif
