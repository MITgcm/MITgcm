#ifndef ADOMP_H
#define ADOMP_H
void assertSafeStack() ;

void getStaticSchedule(int lo, int hi, int stride, int* threadlo, int* threadhi) ;

void initDynamicSchedule() ;
void recordDynamicSchedule(int counter, int stride) ;
void finalizeDynamicSchedule() ;
#endif
