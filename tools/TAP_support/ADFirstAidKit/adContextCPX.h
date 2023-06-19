#ifndef ADCONTEXTCPX_INCLUDED
#define ADCONTEXTCPX_INCLUDED

#include "adComplex.h"

void adContextCpx_init(double epsilon, double seed) ;
void adContextCpx_initReal8(char* varname, cdcmplx *indep) ;
void adContextCpx_initReal8Array(char* varname, cdcmplx *indep, int length) ;
void adContextCpx_initReal4(char* varname, ccmplx *indep) ;
void adContextCpx_initReal4Array(char* varname, ccmplx *indep, int length) ;
void adContextCpx_startConclude() ;
void adContextCpx_concludeReal8(char* varname, cdcmplx *dep) ;
void adContextCpx_concludeReal8Array(char* varname, cdcmplx *dep, int length) ;
void adContextCpx_concludeReal4(char* varname, ccmplx *dep) ;
void adContextCpx_concludeReal4Array(char* varname, ccmplx *dep, int length) ;
void adContextCpx_conclude() ;

#endif
