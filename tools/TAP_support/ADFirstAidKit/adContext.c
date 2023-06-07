/*
 * TAPENADE Automatic Differentiation Engine
 * Copyright (C) 1999-2021 Inria
 * See the LICENSE.md file in the project root for more information.
 *
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "adContext.h"

#include "adComplex.h"

static int dbad_mode, dbad_phase ;
static double dbad_ddeps = 1.e-6 ;
static double dbad_seed = 0.137 ;
static double dbad_currentSeed = 0.0 ;
static double dbad_condensed_val, dbad_condensed_tgt, dbad_condensed_adj ;

double dbad_nextRandom() {
  dbad_currentSeed += dbad_seed ;
  if (dbad_currentSeed>=1.0) dbad_currentSeed-=1.0 ;
  /* Return a value in range [1.0 2.0[ */
  return dbad_currentSeed+1.0 ;
}

void adContextTgt_init(double epsilon, double seed) {
  dbad_mode = 1 ;
  dbad_ddeps = epsilon ;
  dbad_seed = seed ;
  char* phase = getenv("DBAD_PHASE") ;
  if (phase==NULL) {
    printf("Please set DBAD_PHASE environment variable to 1 (perturbed) or 2 (tangent)\n") ;
    exit(0) ;
  } else if (strcmp(phase,"2")==0) {
    printf("Tangent code,  seed=%7.1e\n", seed) ;
    printf("=============================================\n") ;
    dbad_phase = 2 ;
    dbad_currentSeed = 0.0 ;
  } else if (strcmp(phase,"1")==0) {
    printf("Perturbed run, seed=%7.1e, epsilon=%7.1e\n", seed, epsilon) ;
    printf("=============================================\n") ;
    dbad_phase = 1 ;
    dbad_currentSeed = 0.0 ;
  } else if (strcmp(phase,"99")==0) {
    printf("INTERNAL INTERFACE TESTS, seed=%7.1e, epsilon=%7.1e\n", seed, epsilon) ;
    printf("=============================================\n") ;
    dbad_phase = 99 ;
  } else {
    printf("DBAD_PHASE environment variable must be set to 1 or 2\n") ;
    exit(0) ;
  }
}

void adContextTgt_initReal8(char* varname, double *indep, double *indepd) {
  *indepd = dbad_nextRandom() ;
  if (dbad_phase==1)
    *indep = (*indep)+dbad_ddeps*(*indepd) ;
  else if (dbad_phase==99)
    printf("initReal8 of %s: %24.16e //%24.16e\n", varname, *indep, *indepd) ;
}

void adContextTgt_initReal8Array(char* varname, double *indep, double *indepd, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    indepd[i] = dbad_nextRandom() ;
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i] = indep[i]+dbad_ddeps*indepd[i] ;
    }
  } else if (dbad_phase==99) {
    printf("initReal8Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e //%24.16e",i,indep[i],indepd[i]) ;
    printf("\n") ;
  }
}

void adContextTgt_initReal4(char* varname, float *indep, float *indepd) {
  *indepd = (float)dbad_nextRandom() ;
  if (dbad_phase==1)
    *indep = (*indep)+dbad_ddeps*(*indepd) ;
  else if (dbad_phase==99)
    printf("initReal4 of %s: %24.16e //%24.16e\n", varname, *indep, *indepd) ;
}

void adContextTgt_initReal4Array(char* varname, float *indep, float *indepd, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    indepd[i] = (float)dbad_nextRandom() ;
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i] = indep[i]+dbad_ddeps*indepd[i] ;
    }
  } else if (dbad_phase==99) {
    printf("initReal4Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e //%24.16e",i,indep[i],indepd[i]) ;
    printf("\n") ;
  }
}

void adContextTgt_initComplex16(char* varname, double complex *indep, double complex *indepd) {
  double rdot =  dbad_nextRandom() ;
  double idot =  dbad_nextRandom() ;
  *indepd = rdot + I*idot ;
  if (dbad_phase==1) {
    *indep = *indep + dbad_ddeps*(*indepd) ;
  } else if (dbad_phase==99)
    printf("initComplex16 of %s: %24.16e+i%24.16e //%24.16e+i%24.16e\n",
           varname, creal(*indep), cimag(*indep), creal(*indepd), cimag(*indepd)) ;
}

void adContextTgt_initComplex16Array(char* varname, double complex *indep, double complex *indepd, int length) {
  double rdot, idot ;
  int i ;
  for (i=0 ; i<length ; ++i) {
    rdot =  dbad_nextRandom() ;
    idot =  dbad_nextRandom() ;
    indepd[i] = rdot + I*idot ;
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i] = indep[i] + dbad_ddeps*indepd[i] ;
    }
  } else if (dbad_phase==99) {
    printf("initComplex16Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e+i%24.16e //%24.16e+i%24.16e",
             i,creal(indep[i]),cimag(indep[i]),creal(indepd[i]),cimag(indepd[i])) ;
    printf("\n") ;
  }
}

void adContextTgt_initComplex8(char* varname, ccmplx *indep, ccmplx *indepd) {
  indepd->r = (float)dbad_nextRandom() ;
  indepd->i = (float)dbad_nextRandom() ;
  if (dbad_phase==1) {
    indep->r = indep->r + dbad_ddeps*indepd->r ;
    indep->i = indep->i + dbad_ddeps*indepd->i ;
  } else if (dbad_phase==99)
    printf("initComplex8 of %s: %24.16e+i%24.16e //%24.16e+i%24.16e\n",
           varname, indep->r, indep->i, indepd->r, indepd->i) ;
}

void adContextTgt_initComplex8Array(char* varname, ccmplx *indep, ccmplx *indepd, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    indepd[i].r = (float)dbad_nextRandom() ;
    indepd[i].i = (float)dbad_nextRandom() ;
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i].r = indep[i].r+dbad_ddeps*indepd[i].r ;
      indep[i].i = indep[i].i+dbad_ddeps*indepd[i].i ;
    }
  } else if (dbad_phase==99) {
    printf("initComplex8Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e+i%24.16e //%24.16e+i%24.16e",
             i,indep[i].r,indep[i].i,indepd[i].r,indepd[i].i) ;
    printf("\n") ;
  }
}

void adContextTgt_startConclude() {
  dbad_currentSeed= 0.0 ;
  dbad_condensed_val = 0.0 ;
  dbad_condensed_tgt = 0.0 ;
}

void adContextTgt_concludeReal8(char* varname, double dep, double depd) {
  double depb = dbad_nextRandom() ;
  dbad_condensed_val += depb*(dep) ;
  if (dbad_phase==2 || dbad_phase==1)
    dbad_condensed_tgt += depb*(depd) ;
  else if (dbad_phase==99)
    printf("concludeReal8 of %s [%24.16e *] %24.16e //%24.16e\n", varname, depb, dep, depd) ;
}

void adContextTgt_concludeReal8Array(char* varname, double *dep, double *depd, int length) {
  int i ;
  double depb ;
  if (dbad_phase==99) printf("concludeReal8Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depb = dbad_nextRandom() ;
    dbad_condensed_val += depb*dep[i] ;
    if (dbad_phase==2 || dbad_phase==1) {
       dbad_condensed_tgt += depb*depd[i] ;
    } else if (dbad_phase==99) {
      printf("    %i:[%24.16e *] %24.16e //%24.16e",i,depb,dep[i],depd[i]) ;
    }
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextTgt_concludeReal4(char* varname, float dep, float depd) {
  float depb = (float)dbad_nextRandom() ;
  dbad_condensed_val += depb*(dep) ;
  if (dbad_phase==2 || dbad_phase==1)
    dbad_condensed_tgt += depb*(depd) ;
  else if (dbad_phase==99)
    printf("concludeReal4 of %s [%24.16e *] %24.16e //%24.16e\n", varname, depb, dep, depd) ;
}

void adContextTgt_concludeReal4Array(char* varname, float *dep, float *depd, int length) {
  int i ;
  float depb ;
  if (dbad_phase==99) printf("concludeReal4Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depb = (float)dbad_nextRandom() ;
    dbad_condensed_val += depb*dep[i] ;
    if (dbad_phase==2 || dbad_phase==1) {
       dbad_condensed_tgt += depb*depd[i] ;
    } else if (dbad_phase==99) {
      printf("    %i:[%24.16e *] %24.16e //%24.16e",i,depb,dep[i],depd[i]) ;
    }
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextTgt_concludeComplex16(char* varname, double complex dep, double complex depd) {
  double depbr = dbad_nextRandom() ;
  double depbi = dbad_nextRandom() ;
  dbad_condensed_val += depbr*creal(dep) + depbi*cimag(dep);
  if (dbad_phase==2 || dbad_phase==1)
    dbad_condensed_tgt += depbr*creal(depd) + depbi*cimag(depd) ;
  else if (dbad_phase==99)
    printf("concludeComplex16 of %s [%24.16e;%24.16e *] %24.16e+i%24.16e //%24.16e+i%24.16e\n",
           varname, depbr, depbi, creal(dep), cimag(dep), creal(depd), cimag(depd)) ;
}

void adContextTgt_concludeComplex16Array(char* varname, double complex *dep, double complex *depd, int length) {
  int i ;
  double depbr, depbi ;
  if (dbad_phase==99) printf("concludeComplex16Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depbr = dbad_nextRandom() ;
    depbi = dbad_nextRandom() ;
    dbad_condensed_val += depbr*creal(dep[i]) + depbi*cimag(dep[i]);
    if (dbad_phase==2 || dbad_phase==1) {
      dbad_condensed_tgt += depbr*creal(depd[i]) + depbi*cimag(depd[i]) ;
    } else if (dbad_phase==99) {
      printf("    %i:[%24.16e;%24.16e *] %24.16e //%24.16e",
             i, depbr, depbi, creal(dep[i]), cimag(dep[i]), creal(depd[i]), cimag(depd[i])) ;
    }
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextTgt_concludeComplex8(char* varname, ccmplx *dep, ccmplx *depd) {
  float depbr = (float)dbad_nextRandom() ;
  float depbi = (float)dbad_nextRandom() ;
  dbad_condensed_val += depbr*(dep->r) + depbi*(dep->i) ;
  if (dbad_phase==2 || dbad_phase==1)
    dbad_condensed_tgt += depbr*(depd->r) + depbi*(depd->i) ;
  else if (dbad_phase==99)
    printf("concludeComplex8 of %s [%24.16e;%24.16e *] %24.16e+i%24.16e //%24.16e+i%24.16e\n",
           varname, depbr, depbi, dep->r, dep->i, depd->r, depd->i) ;
}

void adContextTgt_concludeComplex8Array(char* varname, ccmplx *dep, ccmplx *depd, int length) {
  int i ;
  float depbr, depbi ;
  if (dbad_phase==99) printf("concludeComplex8Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depbr = (float)dbad_nextRandom() ;
    depbi = (float)dbad_nextRandom() ;
    dbad_condensed_val += depbr*(dep[i].r) + depbi*(dep[i].i) ;
    if (dbad_phase==2 || dbad_phase==1) {
      dbad_condensed_tgt += depbr*(depd[i].r) + depbi*(depd[i].i) ;
    } else if (dbad_phase==99) {
      printf("    %i:[%24.16e;%24.16e *] %24.16e+i%24.16e //%24.16e+i%24.16e",
             i, depbr, depbi, dep[i].r, dep[i].i, depd[i].r, depd[i].i) ;
    }
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextTgt_conclude() {
  if (dbad_phase==2) {
    printf("[seed:%7.1e] Condensed result : %24.16e\n", dbad_seed, dbad_condensed_val) ;
    printf("[seed:%7.1e] Condensed tangent: %24.16e\n", dbad_seed, dbad_condensed_tgt) ;
  } else if (dbad_phase==1) {
    printf("[seed:%7.1e] Condensed perturbed result : %24.16e (epsilon:%7.1e)\n",
           dbad_seed, dbad_condensed_val, dbad_ddeps) ;
    printf("[seed:%7.1e] Condensed perturbed tangent: %24.16e\n", dbad_seed, dbad_condensed_tgt) ;
  }
}

void adContextAdj_init(double seed) {
  dbad_mode = 0 ;
  dbad_seed = seed ;
  char* phase = getenv("DBAD_PHASE") ;
  if (phase==NULL) {
    dbad_phase = 0 ;
  } else if (strcmp(phase,"99")==0) {
    dbad_phase = 99 ;
    printf("INTERNAL INTERFACE TESTS, seed=%7.1e\n", seed) ;
  } else {
    dbad_phase = 0 ;
  }
  printf("Adjoint code,  seed=%7.1e\n", seed) ;
  printf("===================================\n") ;
  dbad_currentSeed = 0.0 ;
}

void adContextAdj_initReal8(char* varname, double *dep, double *depb) {
  *depb = dbad_nextRandom() ;
  if (dbad_phase==99)
    printf("initReal8 of %s %24.16e\n", varname, *depb) ;
}

void adContextAdj_initReal8Array(char* varname, double *dep, double *depb, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    depb[i] = dbad_nextRandom() ;
  }
  if (dbad_phase==99) {
    printf("initReal8Array of %s, length=%i\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e", i, depb[i]) ;
    printf("\n") ;
  }
}

void adContextAdj_initReal4(char* varname, float *dep, float *depb) {
  *depb = (float)dbad_nextRandom() ;
  if (dbad_phase==99)
    printf("initReal4 of %s %24.16e\n", varname, *depb) ;
}

void adContextAdj_initReal4Array(char* varname, float *dep, float *depb, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    depb[i] = (float)dbad_nextRandom() ;
  }
  if (dbad_phase==99) {
    printf("initReal4Array of %s, length=%i\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e",i, depb[i]) ;
    printf("\n") ;
  }
}

void adContextAdj_initComplex16(char* varname, double complex *dep, double complex *depb) {
  double rbar =  dbad_nextRandom() ;
  double ibar =  dbad_nextRandom() ;
  *depb = rbar + I*ibar ;
  if (dbad_phase==99)
    printf("initComplex16 of %s %24.16e+i%24.16e\n", varname, creal(*depb), cimag(*depb)) ;
}

void adContextAdj_initComplex16Array(char* varname, double complex *dep, double complex *depb, int length) {
  double rbar, ibar ;
  int i ;
  for (i=0 ; i<length ; ++i) {
    rbar = dbad_nextRandom() ;
    ibar = dbad_nextRandom() ;
    depb[i] = rbar + I*ibar ;
  }
  if (dbad_phase==99) {
    printf("initComplex16Array of %s, length=%i\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e+i%24.16e",i, creal(depb[i]), cimag(depb[i])) ;
    printf("\n") ;
  }
}

void adContextAdj_initComplex8(char* varname, ccmplx *dep, ccmplx *depb) {
  depb->r = (float)dbad_nextRandom() ;
  depb->i = (float)dbad_nextRandom() ;
  if (dbad_phase==99)
    printf("initComplex8 of %s %24.16e+i%24.16e\n", varname, depb->r, depb->i) ;
}

void adContextAdj_initComplex8Array(char* varname, ccmplx *dep, ccmplx *depb, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    depb[i].r = (float)dbad_nextRandom() ;
    depb[i].i = (float)dbad_nextRandom() ;
  }
  if (dbad_phase==99) {
    printf("initComplex8Array of %s, length=%i\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e+i%24.16e", i, depb[i].r, depb[i].i) ;
    printf("\n") ;
  }
}

void adContextAdj_startConclude() {
  dbad_currentSeed= 0.0 ;
  dbad_condensed_adj = 0.0 ;
}

void adContextAdj_concludeReal8(char* varname, double dep, double depb) {
  double depd = dbad_nextRandom() ;
  dbad_condensed_adj += depd*depb ;
  if (dbad_phase==99)
    printf("concludeReal8 of %s [%24.16e *]%24.16e\n", varname, depd, depb) ;
}

void adContextAdj_concludeReal8Array(char* varname, double *dep, double *depb, int length) {
  int i ;
  double depd ;
  if (dbad_phase==99) printf("concludeReal8Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depd = dbad_nextRandom() ;
    dbad_condensed_adj += depd*depb[i] ;
    if (dbad_phase==99) printf("    %i:[%24.16e *] %24.16e",i,depd,depb[i]) ;
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextAdj_concludeReal4(char* varname, float dep, float depb) {
  float depd = (float)dbad_nextRandom() ;
  dbad_condensed_adj += depd*depb ;
  if (dbad_phase==99)
    printf("concludeReal4 of %s [%24.16e *]%24.16e\n", varname, depd, depb) ;
}

void adContextAdj_concludeReal4Array(char* varname, float *dep, float *depb, int length) {
  int i ;
  float depd ;
  if (dbad_phase==99) printf("concludeReal4Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depd = (float)dbad_nextRandom() ;
    dbad_condensed_adj += depd*depb[i] ;
    if (dbad_phase==99) printf("    %i:[%24.16e *] %24.16e",i,depd,depb[i]) ;
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextAdj_concludeComplex16(char* varname, double complex dep, double complex depb) {
  double depdr = dbad_nextRandom() ;
  double depdi = dbad_nextRandom() ;
  dbad_condensed_adj += depdr*creal(depb) + depdi*cimag(depb) ;
  if (dbad_phase==99)
    printf("concludeComplex16 of %s [%24.16e+i%24.16e *]%24.16e+i%24.16e\n", varname, depdr, depdi, creal(depb), cimag(depb)) ;
}

void adContextAdj_concludeComplex16Array(char* varname, double complex *dep, double complex *depb, int length) {
  int i ;
  double depdr, depdi ;
  if (dbad_phase==99) printf("concludeComplex16Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depdr = dbad_nextRandom() ;
    depdi = dbad_nextRandom() ;
    dbad_condensed_adj += depdr*creal(depb[i]) + depdi*cimag(depb[i]) ;
    if (dbad_phase==99) printf("    %i:[%24.16e+i%24.16e *] %24.16e+i%24.16e",i,depdr,depdi,creal(depb[i]),cimag(depb[i])) ;
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextAdj_concludeComplex8(char* varname, ccmplx *dep, ccmplx *depb) {
  float depdr = (float)dbad_nextRandom() ;
  float depdi = (float)dbad_nextRandom() ;
  dbad_condensed_adj += depdr*depb->r + depdi*depb->i ;
  if (dbad_phase==99)
    printf("concludeComplex8 of %s [%24.16e+i%24.16e *]%24.16e+i%24.16e\n", varname, depdr, depdi, depb->r, depb->i) ;
}

void adContextAdj_concludeComplex8Array(char* varname, ccmplx *dep, ccmplx *depb, int length) {
  int i ;
  float depdr, depdi ;
  if (dbad_phase==99) printf("concludeComplex8Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depdr = (float)dbad_nextRandom() ;
    depdi = (float)dbad_nextRandom() ;
    dbad_condensed_adj += depdr*depb[i].r + depdi*depb[i].i ;
    if (dbad_phase==99) printf("    %i:[%24.16e+i%24.16e *] %24.16e+i%24.16e",i,depdr,depdi,depb[i].r,depb[i].i) ;
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextAdj_conclude() {
  printf("[seed:%7.1e] Condensed adjoint: %24.16e\n", dbad_seed, dbad_condensed_adj) ;
}

//############## INTERFACE PROCEDURES CALLED FROM FORTRAN ################

void adcontexttgt_init_(double *epsilon, double *seed) {
  adContextTgt_init(*epsilon, *seed) ;
}

void adcontexttgt_initreal8_(char* varname, double *indep, double *indepd) {
  adContextTgt_initReal8(varname, indep, indepd) ;
}

void adcontexttgt_initreal8array_(char* varname, double *indep, double *indepd, int *length) {
  adContextTgt_initReal8Array(varname, indep, indepd, *length) ;
}

void adcontexttgt_initreal4_(char* varname, float *indep, float *indepd) {
  adContextTgt_initReal4(varname, indep, indepd) ;
}

void adcontexttgt_initreal4array_(char* varname, float *indep, float *indepd, int *length) {
  adContextTgt_initReal4Array(varname, indep, indepd, *length) ;
}

void adcontexttgt_initcomplex16_(char* varname, cdcmplx *indep, cdcmplx *indepd) {
  adContextTgt_initComplex16(varname, (double complex *)indep, (double complex *)indepd) ;
}

void adcontexttgt_initcomplex16array_(char* varname, cdcmplx *indep, cdcmplx *indepd, int *length) {
  adContextTgt_initComplex16Array(varname, (double complex *)indep, (double complex *)indepd, *length) ;
}

void adcontexttgt_initcomplex8_(char* varname, ccmplx *indep, ccmplx *indepd) {
  adContextTgt_initComplex8(varname, indep, indepd) ;
}

void adcontexttgt_initcomplex8array_(char* varname, ccmplx *indep, ccmplx *indepd, int *length) {
  adContextTgt_initComplex8Array(varname, indep, indepd, *length) ;
}

void adcontexttgt_startconclude_() {
  adContextTgt_startConclude() ;
}

void adcontexttgt_concludereal8_(char* varname, double *dep, double *depd) {
  if (dbad_phase==99)
      printf("concludereal8_ of %s: \n", varname);
  adContextTgt_concludeReal8(varname, *dep, *depd) ;
}

void adcontexttgt_concludereal8array_(char* varname, double *dep, double *depd, int *length) {
  if (dbad_phase==99)
      printf("concludereal8array_ of %s: \n", varname);
  adContextTgt_concludeReal8Array(varname, dep, depd, *length) ;
}

void adcontexttgt_concludereal4_(char* varname, float *dep, float *depd) {
  adContextTgt_concludeReal4(varname, *dep, *depd) ;
}

void adcontexttgt_concludereal4array_(char* varname, float *dep, float *depd, int *length) {
  adContextTgt_concludeReal4Array(varname, dep, depd, *length) ;
}

void adcontexttgt_concludecomplex16_(char* varname, cdcmplx *dep, cdcmplx *depd) {
  adContextTgt_concludeComplex16(varname, *((double complex *)dep), *((double complex *)depd)) ;
}

void adcontexttgt_concludecomplex16array_(char* varname, cdcmplx *dep, cdcmplx *depd, int *length) {
  adContextTgt_concludeComplex16Array(varname, (double complex *)dep, (double complex *)depd, *length) ;
}

void adcontexttgt_concludecomplex8_(char* varname, ccmplx *dep, ccmplx *depd) {
  if (dbad_phase==99)
      printf("concludecomplex8_ of %s: \n", varname);
  adContextTgt_concludeComplex8(varname, dep, depd) ;
}

void adcontexttgt_concludecomplex8array_(char* varname, ccmplx *dep, ccmplx *depd, int *length) {
  if (dbad_phase==99)
      printf("concludecomplex8array_ of %s: \n", varname);
  adContextTgt_concludeComplex8Array(varname, dep, depd, *length) ;
}

void adcontexttgt_conclude_() {
  adContextTgt_conclude() ;
}

void adcontextadj_init_(double *seed) {
  adContextAdj_init(*seed) ;
}

void adcontextadj_initreal8_(char* varname, double *dep, double *depb) {
  if (dbad_phase==99)
    printf("initreal8_ of %s \n", varname) ;
  adContextAdj_initReal8(varname, dep, depb) ;
}

void adcontextadj_initreal8array_(char* varname, double *dep, double *depb, int *length) {
  if (dbad_phase==99)
    printf("initreal8array_ of %s \n", varname) ;
  adContextAdj_initReal8Array(varname, dep, depb, *length) ;
}

void adcontextadj_initreal4_(char* varname, float *dep, float *depb) {
  adContextAdj_initReal4(varname, dep, depb) ;
}

void adcontextadj_initreal4array_(char* varname, float *dep, float *depb, int *length) {
  adContextAdj_initReal4Array(varname, dep, depb, *length) ;
}

void adcontextadj_initcomplex16_(char* varname, cdcmplx *dep, cdcmplx *depb) {
  adContextAdj_initComplex16(varname, (double complex *)dep, (double complex *)depb) ;
}

void adcontextadj_initcomplex16array_(char* varname, cdcmplx *dep, cdcmplx *depb, int *length) {
  adContextAdj_initComplex16Array(varname, (double complex *)dep, (double complex *)depb, *length) ;
}

void adcontextadj_initcomplex8_(char* varname, ccmplx *dep, ccmplx *depb) {
  adContextAdj_initComplex8(varname, dep, depb) ;
}

void adcontextadj_initcomplex8array_(char* varname, ccmplx *dep, ccmplx *depb, int *length) {
  adContextAdj_initComplex8Array(varname, dep, depb, *length) ;
}

void adcontextadj_startconclude_() {
  adContextAdj_startConclude() ;
}

void adcontextadj_concludereal8_(char* varname, double *dep, double *depb) {
  if (dbad_phase==99)
    printf("concludereal8_ of %s \n", varname) ;
  adContextAdj_concludeReal8(varname, *dep, *depb) ;
}

void adcontextadj_concludereal8array_(char* varname, double *dep, double *depb, int *length) {
  if (dbad_phase==99)
    printf("concludereal8array_ of %s \n", varname) ;
  adContextAdj_concludeReal8Array(varname, dep, depb, *length) ;
}

void adcontextadj_concludereal4_(char* varname, float *dep, float *depb) {
  if (dbad_phase==99)
    printf("concludereal4_ of %s \n", varname) ;
  adContextAdj_concludeReal4(varname, *dep, *depb) ;
}

void adcontextadj_concludereal4array_(char* varname, float *dep, float *depb, int *length) {
  if (dbad_phase==99)
    printf("concludereal4array_ of %s \n", varname) ;
  adContextAdj_concludeReal4Array(varname, dep, depb, *length) ;
}

void adcontextadj_concludecomplex16_(char* varname, cdcmplx *dep, cdcmplx *depb) {
  adContextAdj_concludeComplex16(varname, *((double complex *)dep), *((double complex *)depb)) ;
}

void adcontextadj_concludecomplex16array_(char* varname, cdcmplx *dep, cdcmplx *depb, int *length) {
  adContextAdj_concludeComplex16Array(varname, (double complex *)dep, (double complex *)depb, *length) ;
}

void adcontextadj_concludecomplex8_(char* varname, ccmplx *dep, ccmplx *depb) {
  adContextAdj_concludeComplex8(varname, dep, depb) ;
}

void adcontextadj_concludecomplex8array_(char* varname, ccmplx *dep, ccmplx *depb, int *length) {
  adContextAdj_concludeComplex8Array(varname, dep, depb, *length) ;
}

void adcontextadj_conclude_() {
  adContextAdj_conclude() ;
}
