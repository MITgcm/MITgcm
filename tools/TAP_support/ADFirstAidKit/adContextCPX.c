/*
 * TAPENADE Automatic Differentiation Engine
 * Copyright (C) 1999-2021 Inria
 * See the LICENSE.md file in the project root for more information.
 *
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "adContextCPX.h"

static int dbad_mode, dbad_phase ;
static double dbad_ddeps = 1.e-6 ;
static double dbad_seed = 0.137 ;
static double dbad_currentSeed = 0.0 ;
static double dbad_condensed_val, dbad_condensed_tgt ;

double dbad_nextRandom() {
  dbad_currentSeed += dbad_seed ;
  if (dbad_currentSeed>=1.0) dbad_currentSeed-=1.0 ;
  /* Return a value in range [1.0 2.0[ */
  return dbad_currentSeed+1.0 ;
}

void adContextCpx_init(double epsilon, double seed) {
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

void adContextCpx_initReal8(char* varname, cdcmplx *indep) {
  indep->di = dbad_ddeps*dbad_nextRandom() ;
  if (dbad_phase==1)
    indep->dr = indep->dr + indep->di ;
  else if (dbad_phase==99)
    printf("initReal8 of %s: %24.16e + i*%24.16e\n", varname, indep->dr, indep->di) ;
}

void adContextCpx_initReal8Array(char* varname, cdcmplx *indep, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    indep[i].di = dbad_ddeps*dbad_nextRandom() ;
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i].dr = indep[i].dr + indep[i].di ;
    }
  } else if (dbad_phase==99) {
    printf("initReal8Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e + i*%24.16e",i,indep[i].dr,indep[i].di) ;
    printf("\n") ;
  }
}

void adContextCpx_initReal4(char* varname, ccmplx *indep) {
  indep->i = dbad_ddeps*(float)dbad_nextRandom() ;
  if (dbad_phase==1)
    indep->r = indep->r + indep->i ;
  else if (dbad_phase==99)
    printf("initReal4 of %s: %24.16e + i*%24.16e\n", varname, indep->r, indep->i) ;
}

void adContextCpx_initReal4Array(char* varname, ccmplx *indep, int length) {
  int i ;
  for (i=0 ; i<length ; ++i) {
    indep[i].i = dbad_ddeps*(float)dbad_nextRandom() ;
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i].r = indep[i].r + indep[i].i ;
    }
  } else if (dbad_phase==99) {
    printf("initReal4Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e + i*%24.16e",i,indep[i].r,indep[i].i) ;
    printf("\n") ;
  }
}

// NO adContextCpx_initComplex* PRIMITIVES, BECAUSE COMPLEX-STEP MODE REQUIRES NO COMPLEX IN PRIMAL CODE

void adContextCpx_startConclude() {
  dbad_currentSeed= 0.0 ;
  dbad_condensed_val = 0.0 ;
  dbad_condensed_tgt = 0.0 ;
}

void adContextCpx_concludeReal8(char* varname, cdcmplx *dep) {
  double depb = dbad_nextRandom() ;
  dbad_condensed_val += depb*(dep->dr) ;
  if (dbad_phase==2 || dbad_phase==1)
    dbad_condensed_tgt += depb*(dep->di)/dbad_ddeps ;
  else if (dbad_phase==99)
    printf("concludeReal8 of %s [%24.16e *] %24.16e + i*%24.16e\n", varname, depb, dep->dr, dep->di) ;
}

void adContextCpx_concludeReal8Array(char* varname, cdcmplx *dep, int length) {
  int i ;
  double depb ;
  if (dbad_phase==99) printf("concludeReal8Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depb = dbad_nextRandom() ;
    dbad_condensed_val += depb*dep[i].dr ;
    if (dbad_phase==2 || dbad_phase==1) {
       dbad_condensed_tgt += depb*dep[i].di/dbad_ddeps ;
    } else if (dbad_phase==99) {
      printf("    %i:[%24.16e *] %24.16e + i*%24.16e",i,depb,dep[i].dr,dep[i].di) ;
    }
  }
  if (dbad_phase==99) printf("\n") ;
}

void adContextCpx_concludeReal4(char* varname, ccmplx *dep) {
  float depb = (float)dbad_nextRandom() ;
  dbad_condensed_val += depb*(dep->r) ;
  if (dbad_phase==2 || dbad_phase==1)
    dbad_condensed_tgt += depb*(dep->i)/dbad_ddeps ;
  else if (dbad_phase==99)
    printf("concludeReal4 of %s [%24.16e *] %24.16e + i*%24.16e\n", varname, depb, dep->r, dep->i) ;
}

void adContextCpx_concludeReal4Array(char* varname, ccmplx *dep, int length) {
  int i ;
  float depb ;
  if (dbad_phase==99) printf("concludeReal4Array of %s, length=%i:\n", varname, length) ;
  for (i=0 ; i<length ; ++i) {
    depb = (float)dbad_nextRandom() ;
    dbad_condensed_val += depb*dep[i].r ;
    if (dbad_phase==2 || dbad_phase==1) {
       dbad_condensed_tgt += depb*dep[i].i/dbad_ddeps ;
    } else if (dbad_phase==99) {
      printf("    %i:[%24.16e *] %24.16e + i*%24.16e",i,depb,dep[i].r,dep[i].i) ;
    }
  }
  if (dbad_phase==99) printf("\n") ;
}

// NO adContextCpx_concludeComplex* PRIMITIVES, BECAUSE COMPLEX-STEP MODE REQUIRES NO COMPLEX IN PRIMAL CODE

void adContextCpx_conclude() {
  if (dbad_phase==2) {
    printf("[seed:%7.1e] Condensed result : %24.16e\n", dbad_seed, dbad_condensed_val) ;
    printf("[seed:%7.1e] Condensed tangent: %24.16e\n", dbad_seed, dbad_condensed_tgt) ;
  } else if (dbad_phase==1) {
    printf("[seed:%7.1e] Condensed perturbed result : %24.16e (epsilon:%7.1e)\n",
           dbad_seed, dbad_condensed_val, dbad_ddeps) ;
    printf("[seed:%7.1e] Condensed perturbed tangent: %24.16e\n", dbad_seed, dbad_condensed_tgt) ;
  }
}

// NO adContextAdj_* PRIMITIVES, BECAUSE COMPLEX-STEP MODE WORKS ONLY IN TANGENT MODE!

//############## INTERFACE PROCEDURES CALLED FROM FORTRAN ################

void adcontextcpx_init_(double *epsilon, double *seed) {
  adContextCpx_init(*epsilon, *seed) ;
}

void adcontextcpx_initreal8_(char* varname, cdcmplx *indep) {
  adContextCpx_initReal8(varname, indep) ;
}

void adcontextcpx_initreal8array_(char* varname, cdcmplx *indep, int *length) {
  adContextCpx_initReal8Array(varname, indep, *length) ;
}

void adcontextcpx_initreal4_(char* varname, ccmplx *indep) {
  adContextCpx_initReal4(varname, indep) ;
}

void adcontextcpx_initreal4array_(char* varname, ccmplx *indep, int *length) {
  adContextCpx_initReal4Array(varname, indep, *length) ;
}

void adcontextcpx_startconclude_() {
  adContextCpx_startConclude() ;
}

void adcontextcpx_concludereal8_(char* varname, cdcmplx *dep) {
  if (dbad_phase==99)
      printf("concludereal8_ of %s: \n", varname);
  adContextCpx_concludeReal8(varname, dep) ;
}

void adcontextcpx_concludereal8array_(char* varname, cdcmplx *dep, int *length) {
  if (dbad_phase==99)
      printf("concludereal8array_ of %s: \n", varname);
  adContextCpx_concludeReal8Array(varname, dep, *length) ;
}

void adcontextcpx_concludereal4_(char* varname, ccmplx *dep) {
  adContextCpx_concludeReal4(varname, dep) ;
}

void adcontextcpx_concludereal4array_(char* varname, ccmplx *dep, int *length) {
  adContextCpx_concludeReal4Array(varname, dep, *length) ;
}

void adcontextcpx_conclude_() {
  adContextCpx_conclude() ;
}
