#ifndef ADCONTEXT_INCLUDED
#define ADCONTEXT_INCLUDED

#include "complex.h"

void adContextTgt_init(double epsilon, double seed) ;
void adContextTgt_initReal8(char* varname, double *indep, double *indepd) ;
void adContextTgt_initReal8Array(char* varname, double *indep, double *indepd, int length) ;
void adContextTgt_initReal4(char* varname, float *indep, float *indepd) ;
void adContextTgt_initReal4Array(char* varname, float *indep, float *indepd, int length) ;
void adContextTgt_initComplex16(char* varname, double complex *indep, double complex *indepd) ;
void adContextTgt_initComplex16Array(char* varname, double complex *indep, double complex *indepd, int length) ;
/* Commented out because sizeof(complex) == sizeof(double complex) == 16 */
/* void adContextTgt_initComplex8(char* varname, complex *indep, complex *indepd) ; */
/* void adContextTgt_initComplex8Array(char* varname, complex *indep, complex *indepd, int length) ; */
void adContextTgt_startConclude() ;
void adContextTgt_concludeReal8(char* varname, double dep, double depd) ;
void adContextTgt_concludeReal8Array(char* varname, double *dep, double *depd, int length) ;
void adContextTgt_concludeReal4(char* varname, float dep, float depd) ;
void adContextTgt_concludeReal4Array(char* varname, float *dep, float *depd, int length) ;
void adContextTgt_concludeComplex16(char* varname, double complex dep, double complex depd) ;
void adContextTgt_concludeComplex16Array(char* varname, double complex *dep, double complex *depd, int length) ;
/* Commented out because sizeof(complex) == sizeof(double complex) == 16 */
/* void adContextTgt_concludeComplex8(char* varname, complex *dep, complex *depd) ; */
/* void adContextTgt_concludeComplex8Array(char* varname, complex *dep, complex *depd, int length) ; */
void adContextTgt_conclude() ;
void adContextAdj_init(double seed) ;
void adContextAdj_initReal8(char* varname, double *dep, double *depb) ;
void adContextAdj_initReal8Array(char* varname, double *dep, double *depb, int length) ;
void adContextAdj_initReal4(char* varname, float *dep, float *depb) ;
void adContextAdj_initReal4Array(char* varname, float *dep, float *depb, int length) ;
void adContextAdj_initComplex16(char* varname, double complex *dep, double complex *depb) ;
void adContextAdj_initComplex16Array(char* varname, double complex *dep, double complex *depb, int length) ;
/* Commented out because sizeof(complex) == sizeof(double complex) == 16 */
/* void adContextAdj_initComplex8(char* varname, complex *dep, complex *depb) ; */
/* void adContextAdj_initComplex8Array(char* varname, complex *dep, complex *depb, int length) ; */
void adContextAdj_startConclude() ;
void adContextAdj_concludeReal8(char* varname, double dep, double depb) ;
void adContextAdj_concludeReal8Array(char* varname, double *dep, double *depb, int length) ;
void adContextAdj_concludeReal4(char* varname, float dep, float depb) ;
void adContextAdj_concludeReal4Array(char* varname, float *dep, float *depb, int length) ;
void adContextAdj_concludeComplex16(char* varname, double complex dep, double complex depb) ;
void adContextAdj_concludeComplex16Array(char* varname, double complex *dep, double complex *depb, int length) ;
/* Commented out because sizeof(complex) == sizeof(double complex) == 16 */
/* void adContextAdj_concludeComplex8(char* varname, complex *dep, complex *depb) ; */
/* void adContextAdj_concludeComplex8Array(char* varname, complex *dep, complex *depb, int length) ; */
void adContextAdj_conclude() ;

#endif
