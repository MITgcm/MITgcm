#ifndef ADCONTEXT_INCLUDED
#define ADCONTEXT_INCLUDED

void adContextTgt_init(double epsilon, double seed) ;
void adContextTgt_initReal8(char* varname, double *indep, double *indepd) ;
void adContextTgt_initReal8Array(char* varname, double *indep, double *indepd, int length) ;
void adContextTgt_initReal4(char* varname, float *indep, float *indepd) ;
void adContextTgt_initReal4Array(char* varname, float *indep, float *indepd, int length) ;
void adContextTgt_startConclude() ;
void adContextTgt_concludeReal8(char* varname, double dep, double depd) ;
void adContextTgt_concludeReal8Array(char* varname, double *dep, double *depd, int length) ;
void adContextTgt_concludeReal4(char* varname, float dep, float depd) ;
void adContextTgt_concludeReal4Array(char* varname, float *dep, float *depd, int length) ;
void adContextTgt_conclude() ;
void adContextAdj_init(double seed) ;
void adContextAdj_initReal8(char* varname, double *dep, double *depb) ;
void adContextAdj_initReal8Array(char* varname, double *dep, double *depb, int length) ;
void adContextAdj_initReal4(char* varname, float *dep, float *depb) ;
void adContextAdj_initReal4Array(char* varname, float *dep, float *depb, int length) ;
void adContextAdj_startConclude() ;
void adContextAdj_concludeReal8(char* varname, double dep, double depb) ;
void adContextAdj_concludeReal8Array(char* varname, double *dep, double *depb, int length) ;
void adContextAdj_concludeReal4(char* varname, float dep, float depb) ;
void adContextAdj_concludeReal4Array(char* varname, float *dep, float *depb, int length) ;
void adContextAdj_conclude() ;

#endif
