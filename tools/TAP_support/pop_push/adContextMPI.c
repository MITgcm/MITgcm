#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "adContext.h"

/* SPECIAL FOR THE NON-REGRESSION TESTS ON AMPI */
#include <mpi.h>

static int dbad_mode, dbad_phase ;
static double dbad_ddeps = 1.e-6 ;
static double dbad_seed = 0.137 ;
static double dbad_currentSeed = 0.0 ;
static double dbad_condensed_val, dbad_condensed_tgt, dbad_condensed_adj ;

double dbad_nextRandom() {
  dbad_currentSeed += dbad_seed ;
  if (dbad_currentSeed>1.0) dbad_currentSeed-=1.0 ;
  /* Return a value in range [1.0 2.0[ */
  return dbad_currentSeed+1.0 ;
}

void adContextTgt_init(double epsilon, double seed) {
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  dbad_mode = 1 ;
  dbad_ddeps = epsilon ;
  dbad_seed = seed ;
  char* phase = getenv("DBAD_PHASE") ;
  if (phase==NULL) {
    printf("Please set DBAD_PHASE environment variable to 1 (perturbed) or 2 (tangent)\n") ;
    exit(0) ;
  } else if (strcmp(phase,"2")==0) {
    if (world_rank == 0) {
      printf("Tangent code,  seed=%7.1e\n", seed) ;
      printf("=============================================\n") ;
    }
    dbad_phase = 2 ;
    dbad_currentSeed = 0.0 ;
  } else if (strcmp(phase,"1")==0) {
    if (world_rank == 0) {
      printf("Perturbed run, seed=%7.1e, epsilon=%7.1e\n", seed, epsilon) ;
      printf("=============================================\n") ;
    }
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

void adContextTgt_startConclude() {
  dbad_currentSeed= 0.0 ;
  dbad_condensed_val = 0.0 ;
  dbad_condensed_tgt = 0.0 ;
}

void adContextTgt_concludeReal8(char* varname, double dep, double depd) {
  double depb = dbad_nextRandom() ;
  dbad_condensed_val += depb*(dep) ;
  if (dbad_phase==2)
    dbad_condensed_tgt += depb*(depd) ;
  else if (dbad_phase==99)
    printf("concludeReal8 of %s %24.16e //%24.16e //%24.16e\n", varname, depb, dep, depd) ;
}

void adContextTgt_concludeReal8Array(char* varname, double *dep, double *depd, int length) {
  int i ;
  double depb ;
  for (i=0 ; i<length ; ++i) {
    depb = dbad_nextRandom() ;
    dbad_condensed_val += depb*dep[i] ;
    if (dbad_phase==2) {
       dbad_condensed_tgt += depb*depd[i] ;
    }
  }
}

void adContextTgt_concludeReal4(char* varname, float dep, float depd) {
  float depb = (float)dbad_nextRandom() ;
  dbad_condensed_val += depb*(dep) ;
  if (dbad_phase==2)
    dbad_condensed_tgt += depb*(depd) ;
  else if (dbad_phase==99)
    printf("concludeReal4 of %s %24.16e //%24.16e //%24.16e\n", varname, depb, dep, depd) ;
}

void adContextTgt_concludeReal4Array(char* varname, float *dep, float *depd, int length) {
  int i ;
  float depb ;
  for (i=0 ; i<length ; ++i) {
    depb = (float)dbad_nextRandom() ;
    dbad_condensed_val += depb*dep[i] ;
    if (dbad_phase==2) {
       dbad_condensed_tgt += depb*depd[i] ;
    } else if (dbad_phase==99)
       printf("concludeReal4Array of %s: \n", varname);
  }
}

void adContextTgt_conclude() {
  double reducedVal, reducedTgt ;
  MPI_Reduce(&dbad_condensed_val,&reducedVal,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);
  if (dbad_phase==2)
    MPI_Reduce(&dbad_condensed_tgt,&reducedTgt,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  if (world_rank == 0) {
    if (dbad_phase==2) {
      printf("[seed:%7.1e] Reduced Condensed result : %24.16e\n", dbad_seed, reducedVal) ;
      printf("[seed:%7.1e] Reduced Condensed tangent: %24.16e\n", dbad_seed, reducedTgt) ;
    } else if (dbad_phase==1) {
      printf("[seed:%7.1e] Reduced Condensed perturbed result : %24.16e (epsilon:%7.1e)\n",
             dbad_seed, reducedVal, dbad_ddeps) ;
    }
  }
}

void adContextAdj_init(double seed) {
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
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
  if (world_rank == 0) {
    printf("Adjoint code,  seed=%7.1e\n", seed) ;
    printf("===================================\n") ;
  }
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
    printf("initReal8Array length=%i\n", length) ;
    for (i=0 ; i<length ; ++i)
      printf("    of %s %i:%24.16e", varname, i, depb[i]) ;
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
    printf("initReal4Array length=%i\n", length) ;
    for (i=0 ; i<length ; ++i)
      printf("    of %s %i:%24.16e",varname, i, depb[i]) ;
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
    printf("concludeReal8 of %s %24.16e //%24.16e\n", varname, depb, depd) ;
}

void adContextAdj_concludeReal8Array(char* varname, double *dep, double *depb, int length) {
  int i ;
  double depd ;
  for (i=0 ; i<length ; ++i) {
    depd = dbad_nextRandom() ;
    dbad_condensed_adj += depd*depb[i] ;
  }
}

void adContextAdj_concludeReal4(char* varname, float dep, float depb) {
  float depd = (float)dbad_nextRandom() ;
  dbad_condensed_adj += depd*depb ;
  if (dbad_phase==99)
    printf("concludeReal4 of %s %24.16e //%24.16e\n", varname, depb, depd) ;
}

void adContextAdj_concludeReal4Array(char* varname, float *dep, float *depb, int length) {
  int i ;
  float depd ;
  for (i=0 ; i<length ; ++i) {
    depd = (float)dbad_nextRandom() ;
    dbad_condensed_adj += depd*depb[i] ;
  }
  if (dbad_phase==99)
    printf("concludeReal4Array of %s\n", varname) ;
}

void adContextAdj_conclude() {
  double reducedAdj ;
  MPI_Reduce(&dbad_condensed_adj,&reducedAdj,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  if (world_rank == 0) {
    printf("[seed:%7.1e] Reduced Condensed adjoint: %24.16e\n", dbad_seed, reducedAdj) ;
  }
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

void adcontextadj_conclude_() {
  adContextAdj_conclude() ;
}
