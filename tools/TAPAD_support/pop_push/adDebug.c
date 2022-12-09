#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h>
#include "adDebug.h"

extern void pushNArray(char *x, unsigned int nbChars, int checkReadOnly) ;
extern void popNArray(char *x, unsigned int nbChars, int checkReadOnly) ;
extern void pushCharacterArray(char *x, int n) ;
extern void popCharacterArray(char *x, int n) ;

/* The "call stack" used by debugging to
 * keep track of the position in the call tree */
typedef struct _DBAD_CallStackElem {
  char *funcname ;
  int   deltadepth ;
  int   code ;
  struct _DBAD_CallStackElem *context ;
} DBAD_CallStackElem ;

static DBAD_CallStackElem dbad_topContext ;
static DBAD_CallStackElem *dbad_callStack ;
static int dbad_calltracedepth = 1 ;

static int dbad_mode, dbad_phase, dbad_nberrors ;
static int dbad_trace = 0 ;
static int dbad_nocommunication = 0 ;
/** The rank of the current process (from 0 up), if it is being tested.
 * -2 means don't test the current process
 * -1 means test this process (not-MPI case) but don't print process number! */
static int dbad_testThisProcess = -1 ;
static FILE *dbad_file ;
static double dbad_errormax, dbad_ddeps = 1.e-6 ;
static double dbad_seed = 0.137 ;
static double dbad_currentSeed = 0.0 ;
static double dbad_condensed_dd, dbad_condensed_tgt, dbad_condensed_adj ;
static double dbad_refsum, dbad_nextrefsum ;

/** Buffers for the stack mechanism, redefined here after the model
 * of adBuffer.c, to avoid linking with adBuffer.c */
static double dbad_adr8buf[512] ;
static int dbad_adr8ibuf = 0 ;
static int dbad_adi4buf[512] ;
static int dbad_adi4ibuf = 0 ;

void dbad_pushReal8(double x) {
  if (dbad_adr8ibuf >= 511) {
    dbad_adr8buf[511] = x ;
    pushNArray((char *)dbad_adr8buf, 512*8, 1) ;
    dbad_adr8ibuf = 0 ;
  } else {
    dbad_adr8buf[dbad_adr8ibuf] = x ;
    ++dbad_adr8ibuf ;
  }
}

void dbad_popReal8(double *x) {
  if (dbad_adr8ibuf <= 0) {
    popNArray((char *)dbad_adr8buf, 512*8, 1) ;
    dbad_adr8ibuf = 511 ;
    *x = dbad_adr8buf[511] ;
  } else {
    --dbad_adr8ibuf ;
    *x = dbad_adr8buf[dbad_adr8ibuf] ;
  }
}

void dbad_pushinteger4(int x)  {
  if (dbad_adi4ibuf >= 511) {
    dbad_adi4buf[511] = x ;
    pushNArray((char *)dbad_adi4buf, 512*4, 1) ;
    dbad_adi4ibuf = 0 ;
  } else {
    dbad_adi4buf[dbad_adi4ibuf] = x ;
    ++dbad_adi4ibuf ;
  }
}

void dbad_popinteger4(int *x) {
  if (dbad_adi4ibuf <= 0) {
    popNArray((char *)dbad_adi4buf, 512*4, 1) ;
    dbad_adi4ibuf = 511 ;
    *x = dbad_adi4buf[511] ;
  } else {
    --dbad_adi4ibuf ;
    *x = dbad_adi4buf[dbad_adi4ibuf] ;
  }
}
/* End redefinition of the buffers for the stack mechanism */

void dbad_pushCallFrame(char* unitname, int deltadepth, int forcetraced) {
  DBAD_CallStackElem *newCallLevel = (DBAD_CallStackElem*)malloc(sizeof(DBAD_CallStackElem)) ;
  newCallLevel->funcname = (char*)malloc(100) ;
  sprintf(newCallLevel->funcname, "%s", unitname) ;
  newCallLevel->deltadepth = (dbad_calltracedepth>0?1-deltadepth:0) ;
  dbad_calltracedepth -= newCallLevel->deltadepth ;
  // forcing mechanism:
  if (forcetraced>0 && forcetraced>dbad_calltracedepth) {
    newCallLevel->deltadepth -= (forcetraced-dbad_calltracedepth) ;
    dbad_calltracedepth = forcetraced ;
  }
  newCallLevel->code = 0 ;
  newCallLevel->context = dbad_callStack ;
  dbad_callStack = newCallLevel ;
}

void dbad_popCallFrame() {
  dbad_calltracedepth += dbad_callStack->deltadepth ;
  DBAD_CallStackElem *newCallLevel = dbad_callStack->context ;
  free(dbad_callStack->funcname) ;
  free(dbad_callStack) ;
  dbad_callStack = newCallLevel ;
}

int dbad_debughere(int forcetraced) {
  return (dbad_calltracedepth>0 || forcetraced) ;
}

int dbad_debugabove() {
  return (dbad_calltracedepth+dbad_callStack->deltadepth)>0 ;
}

int dbad_callstackdepth() {
  DBAD_CallStackElem *incallstack = dbad_callStack ;
  int depth = 0 ;
  while (incallstack) {++depth ; incallstack=incallstack->context ;}
  return depth-1 ;
}

void dbad_resetCondensors() {
  dbad_currentSeed = 0.0 ;
  dbad_condensed_dd = 0.0 ;
  dbad_condensed_tgt = 0.0 ;
  dbad_condensed_adj = 0.0 ;
}

double dbad_nextRandom() {
  dbad_currentSeed += dbad_seed ;
  if (dbad_currentSeed>1.0) dbad_currentSeed-=1.0 ;
  /* Return a value in range [1.0 2.0[ */
  return dbad_currentSeed+1.0 ;
}

/** Scales the tangent direction *indepd (now in range [1.0 2.0[)
 * so that it has same magnitude as indep */
void dbad_scaleIndepDirection4(float *indepd, float indep) {
  int exponent ;
  frexpf(indep, &exponent);
  *indepd = ldexpf(*indepd, exponent) ; 
}

/** Scales the tangent direction *indepd (now in range [1.0 2.0[)
 * so that it has same magnitude as indep */
void dbad_scaleIndepDirection8(double *indepd, double indep) {
  int exponent ;
  frexp(indep, &exponent);
  *indepd = ldexp(*indepd, exponent) ; 
}

void dbad_putOneVarName(char *varname) {
  char buf[8]="        " ;
  int len = strlen(varname) ;
  if (len>8) len = 8 ;
  memcpy(buf, varname, len) ;
  fwrite(buf, sizeof(char), 8, dbad_file) ;
}

void dbad_ddcheckvarname(char* varname) {
  char localBuf[9]="        " ;
  char remoteBuf[9]="        " ;
  int len = strlen(varname) ;
  if (len>8) len = 8 ;
  memcpy(localBuf, varname, len) ;
  fread(remoteBuf, sizeof(char), 8, dbad_file) ;
  if (strcmp(localBuf, remoteBuf)!=0) {
    printf("Control mismatch, expecting a variable named \"%s\", got \"%s\"\n",localBuf,remoteBuf) ;
    exit(0) ;
  }
}

void dbad_putOne8(double var) {
  fwrite(&var, sizeof(double), 1, dbad_file) ;
}

void dbad_putOne4(float var) {
  float fl2[2] ;
  fl2[0] = var ;
  fwrite(fl2, sizeof(float), 2, dbad_file) ;
}

void dbad_getOne8(double *var) {
  fread(var, sizeof(double), 1, dbad_file) ;
}

void dbad_getOne4(float *var) {
  float fl2[2] ;
  fread(fl2, sizeof(float), 2, dbad_file) ;
  *var = fl2[0] ;
}

/* static int nbDebugDebug = 10 ; */

int dbad_discrepancy8(double vareps, double var, double vard, double *dd, float *diff) {
  int hasDifference = 0 ;
  int almostZero = -15;       // Tests more strict when almostZero goes down to -Inf.
  int errMaxBits = 8;         // Tests more strict when errMaxBits goes up to +Inf.
  int trustableMantissa = 53; // Tests more strict when trustableMantissa goes up to 52.
  *dd = 0.0 ;
  //TODO: test vareps and var for NaNs!
  int expoTG ;
  frexp(vard, &expoTG) ;
  double dv = vareps-var ;
/* printf("vard:%24.16e expoTG:%i almostZero:%i dv:%24.16e - %24.16e = %24.16e\n",vard,expoTG,almostZero,vareps,var,dv) ; */
  if (dv==0.0) {
    *dd = 0.0 ;
    if (vard==0.0 || expoTG<almostZero) {
      hasDifference = 0 ;
      *diff = 0.0 ;
    } else {
      hasDifference = 1;
      *diff = 100.0;
/* if (nbDebugDebug && hasDifference) { */
/*   nbDebugDebug-- ; */
/*   printf("vard:%24.16e (%i) %i\n", vard, expoTG, (expoTG<almostZero)) ; */
/* } */
    }
  } else {
    *dd = dv/dbad_ddeps ;
    int expoDD ;
    frexpf(*dd, &expoDD) ;
    double hv = dv - dbad_ddeps*vard ;
/* printf("dd:%24.16e expoDD:%i almostZero:%i dbad_ddeps:%8.1e hv:%24.16e\n",*dd,expoDD,almostZero,dbad_ddeps,hv) ; */
    if (hv==0.0 || expoTG<almostZero || expoDD<almostZero) {
      hasDifference = 0 ;
      *diff = 0.0 ;
    } else {
      int expoDV, expoV1, expoV2, expoV, expoHV ;
      frexp(dv, &expoDV) ;
      frexp(var, &expoV1) ;
      frexp(vareps, &expoV2) ;
      expoV = (var==0.0?expoV2:(vareps==0?expoV1:(expoV1>expoV2?expoV1:expoV2))) ;
      frexp(hv, &expoHV) ;
      int discrepancySmallness = expoDV-expoHV ;
      int discrepancyExactness = trustableMantissa-expoV+expoDV ;
      hasDifference = (discrepancySmallness<errMaxBits && discrepancySmallness<discrepancyExactness) ;

      double absvard = (vard>=0.0?vard:-vard) ;
      double absdd = (*dd>=0.0?*dd:-*dd) ;
      double maxabs = (absvard>absdd?absvard:absdd) ;
      double absvardmdd = vard-*dd ;
      if (absvardmdd<0.0) absvardmdd=-absvardmdd ;
      *diff = (float)((absvardmdd/maxabs)*100.0) ;
/* if (nbDebugDebug && hasDifference) { */
/*   nbDebugDebug-- ; */
/*   printf("vard:%24.16e [v-eps:%24.16e v-loc:%24.16e (%i)] dd:%24.16e (%i) -> %i\n", */
/*          vard, vareps, var, expoV, dv, expoDV, expoHV) ; */
/*   printf("n2:%i  52-M0+M1-6:%i different:%i %5.1f\n", */
/*          discrepancySmallness, discrepancyExactness, hasDifference, *diff) ; */
/* } */
    }
  }
  return hasDifference ;
}

int dbad_discrepancy4(float vareps, float var, float vard, float *dd, float *diff) {
  int hasDifference = 0 ;
  int almostZero = -15;       // Tests more strict when almostZero goes down to -Inf.
  int errMaxBits = 6;         // Tests more strict when errMaxBits goes up to +Inf.
  int trustableMantissa = 23; // Tests more strict when trustableMantissa goes up to 52.
  *dd = 0.0 ;
  //TODO: test vareps and var for NaNs!
  int expoTG ;
  frexpf(vard, &expoTG) ;
  float dv = vareps-var ;
  if (dv==0.0) {
    *dd = 0.0 ;
    if (vard==0.0 || expoTG<almostZero) {
      hasDifference = 0 ;
      *diff = 0.0 ;
    } else {
      hasDifference = 1;
      *diff = 100.0;
/* if (nbDebugDebug && hasDifference) { */
/*   nbDebugDebug-- ; */
/*   printf("vard:%24.16e (%i) %i\n", vard, expoTG, (expoTG<almostZero)) ; */
/* } */
    }
  } else {
    *dd = dv/dbad_ddeps ;
    int expoDD ;
    frexpf(*dd, &expoDD) ;
    float hv = dv - dbad_ddeps*vard ;
    if (hv==0.0 || expoTG<almostZero || expoDD<almostZero) {
      hasDifference = 0 ;
      *diff = 0.0 ;
    } else {
      int expoDV, expoV1, expoV2, expoV, expoHV ;
      frexpf(dv, &expoDV) ;
      frexpf(var, &expoV1) ;
      frexpf(vareps, &expoV2) ;
      expoV = (var==0.0?expoV2:(vareps==0?expoV1:(expoV1>expoV2?expoV1:expoV2))) ;
      frexpf(hv, &expoHV) ;
      int discrepancySmallness = expoDV-expoHV ;
      int discrepancyExactness = trustableMantissa-expoV+expoDV ;
      hasDifference = (discrepancySmallness<errMaxBits && discrepancySmallness<discrepancyExactness) ;

      float absvard = (vard>=0.0?vard:-vard) ;
      float absdd = (*dd>=0.0?*dd:-*dd) ;
      float maxabs = (absvard>absdd?absvard:absdd) ;
      float absvardmdd = vard-*dd ;
      if (absvardmdd<0.0) absvardmdd=-absvardmdd ;
      *diff = (absvardmdd/maxabs)*100.0 ;
/* if (nbDebugDebug && hasDifference) { */
/*   nbDebugDebug-- ; */
/*   printf("vard:%24.16e [v-eps:%24.16e v-loc:%24.16e (%i)] dd:%24.16e (%i) -> %i\n", */
/*          vard, vareps, var, expoV, dv, expoDV, expoHV) ; */
/*   printf("n2:%i  52-M0+M1-6:%i different:%i %5.1f\n", */
/*          discrepancySmallness, discrepancyExactness, hasDifference, *diff) ; */
/* } */
    }
  }
  return hasDifference ;
}

void dbad_display_location(char *placename) {
 if (dbad_testThisProcess!=-2) {
  int i ;
  char* enclosproc = dbad_callStack->funcname ;
  int callStackDepth = dbad_callstackdepth() ;
  char indentWhite[20] = "                    " ;
  if (callStackDepth<10) indentWhite[2*callStackDepth]='\0';
  if (dbad_testThisProcess==-1) {
    printf("%s[%2i]AT:%s OF %s\n", indentWhite, callStackDepth, placename, enclosproc) ;
  } else {
    printf("[process %i] %s[%2i]AT:%s OF %s\n", dbad_testThisProcess, indentWhite, callStackDepth, placename, enclosproc) ;
  }
 }
}

void dbad_adDebugTgt_testComplex16(char* varname, cdcmplx var, cdcmplx *vard, int conclude) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      dbad_putOne8(var.dr) ;
      dbad_putOne8(var.di) ;
    } else if (dbad_phase==2) {
      cdcmplx ddvar, dd ;
      float diffR, diffI ;
      int hasDifferenceR, hasDifferenceI ;
      dbad_ddcheckvarname(varname) ;
      dbad_getOne8(&(ddvar.dr)) ;
      dbad_getOne8(&(ddvar.di)) ;
      hasDifferenceR = dbad_discrepancy8(ddvar.dr, var.dr, vard->dr, &(dd.dr), &diffR) ;
      hasDifferenceI = dbad_discrepancy8(ddvar.di, var.di, vard->di, &(dd.di), &diffI) ;
      if (dbad_trace) {
        printf("%s [v-eps:(%24.16e,%24.16e) v-loc:(%24.16e,%24.16e)] (%24.16e,%24.16e)(dd (%5.1f,%5.1f)%% DIFF? WITH ad)(%24.16e,%24.16e)\n",
               varname,ddvar.dr,ddvar.di,var.dr,var.di,dd.dr,dd.di,diffR,diffI,vard->dr,vard->di) ;
      } else if (hasDifferenceR||hasDifferenceI) {
        printf("%s (%24.16e,%24.16e)(dd (%5.1f,%5.1f)%% DIFF WITH ad)(%24.16e,%24.16e)\n",
               varname,dd,dd,diffR,diffI,vard->dr,vard->di) ;
      }
      if (conclude==-1) {
        if (hasDifferenceR) {
          vard->dr = dd.dr ;
          printf("%s (.real) RESET:\n", varname) ;
        }
        if (hasDifferenceI) {
          vard->di = dd.di ;
          printf("%s (.imag) RESET:\n", varname) ;
        }
      }
      if (conclude==1) {
        cdcmplx varb ;
        varb.dr = dbad_nextRandom();
        varb.di = dbad_nextRandom();
        dbad_condensed_dd  += dd.dr*varb.dr   + dd.di*varb.di;
        dbad_condensed_tgt += vard->dr*varb.dr + vard->di*varb.di;
      }
    }
  }
}

void dbad_adDebugTgt_testReal8(char* varname, double var, double *vard, int conclude) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      dbad_putOne8(var) ;
    } else if (dbad_phase==2) {
      double ddvar, dd ;
      float diff ;
      dbad_ddcheckvarname(varname) ;
      dbad_getOne8(&ddvar) ;
      int hasDifference = dbad_discrepancy8(ddvar, var, *vard, &dd, &diff) ;
      if (dbad_trace) {
        printf("%s [v-eps:%24.16e v-loc:%24.16e] %24.16e(dd %5.1f%% DIFF? WITH ad)%24.16e\n",
               varname,ddvar,var,dd,diff,*vard) ;
      } else if (hasDifference) {
        printf("%s %24.16e(dd %5.1f%% DIFF WITH ad)%24.16e\n",
               varname,dd,diff,*vard) ;
      }
      if (conclude==-1 && hasDifference) {
        *vard = dd ;
        printf("%s RESET:\n", varname) ;
      }
      if (conclude==1) {
        double varb = dbad_nextRandom() ;
        dbad_condensed_dd += dd*varb;
        dbad_condensed_tgt += *vard*varb;
      }
    }
  }
}

void dbad_adDebugTgt_testReal4(char* varname, float var, float *vard, int conclude) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      dbad_putOne4(var) ;
    } else if (dbad_phase==2) {
      float ddvar, dd ;
      float diff ;
      dbad_ddcheckvarname(varname) ;
      dbad_getOne4(&ddvar) ;
      int hasDifference = dbad_discrepancy4(ddvar, var, *vard, &dd, &diff) ;
      if (dbad_trace) {
        printf("%s [v-eps:%18.10e v-loc:%18.10e] %18.10e(dd %5.1f%% DIFF? WITH ad)%18.10e\n",
               varname,ddvar,var,dd,diff,*vard) ;
      } else if (hasDifference) {
        printf("%s %18.10e(dd %5.1f%% DIFF WITH ad)%18.10e\n",
               varname,dd,diff,*vard) ;
      }
      if (conclude==-1 && hasDifference) {
        *vard = dd ;
        printf("%s RESET:\n", varname) ;
      }
      if (conclude==1) {
        double varb = dbad_nextRandom() ;
        dbad_condensed_dd += ((double)dd)*varb;
        dbad_condensed_tgt += ((double)*vard)*varb;
      }
    }
  }
}

void dbad_adDebugTgt_testComplex16Array(char* varname, cdcmplx* var, cdcmplx* vard, int length, int conclude) {
  if (!var || !vard) return ;
  if (dbad_testThisProcess!=-2) {
    int i ;
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      for (i=0 ; i<length ; ++i) {
        dbad_putOne8(var[i].dr) ;
        dbad_putOne8(var[i].di) ;
      }
    } else if (dbad_phase==2) {
      cdcmplx ddvar, dd, vardbuf[10], ddbuf[10], varepsbuf[10], varlocbuf[10] ;
      float diffR, diffI ;
      int hasDifferenceR, hasDifferenceI, printedheader = 0, indexbuf[10], ibuf = 0 ;
      dbad_ddcheckvarname(varname) ;
      for (i=0 ; i<length ; ++i) {
        dbad_getOne8(&ddvar.dr) ;
        dbad_getOne8(&ddvar.di) ;
        hasDifferenceR = dbad_discrepancy8(ddvar.dr, var[i].dr, vard[i].dr, &(dd.dr), &diffR) ;
        hasDifferenceI = dbad_discrepancy8(ddvar.di, var[i].di, vard[i].di, &(dd.di), &diffI) ;
        if (hasDifferenceR || hasDifferenceI || dbad_trace) {
          if (dbad_trace) {
            varepsbuf[ibuf] = ddvar ;
            varlocbuf[ibuf] = var[i] ;
          }
          vardbuf[ibuf] = vard[i] ;
          ddbuf[ibuf] = dd ;
          indexbuf[ibuf] = i ;
          ++ibuf ;
        }
        if (conclude==-1) { //Nudge the ad derivative back to divided-diff value!
          if (hasDifferenceR) vard[i].dr = dd.dr ;
          if (hasDifferenceI) vard[i].di = dd.di ;
        }
        if (ibuf>=5 || (i==length-1 && ibuf>0)) {
          int j ;
          if (!printedheader) {
            if (conclude==-1)
              printf("%s RESET:\n", varname) ;
            else
              printf("%s DIFFS:\n", varname) ;
            printedheader = 1 ;
          }
          printf("    ") ;
          for (j=0 ; j<ibuf ; ++j)
            printf(" %4i->(%11.4e,%11.4e)", indexbuf[j], vardbuf[j].dr, vardbuf[j].di) ;
          printf("\n    ") ;
          if (dbad_trace) {
            for (j=0 ; j<ibuf ; ++j)
              printf("  (eps)(%11.4e,%11.4e)", varepsbuf[j].dr, varepsbuf[j].di) ;
            printf("\n    ") ;
            for (j=0 ; j<ibuf ; ++j)
              printf("  (loc)(%11.4e,%11.4e)", varlocbuf[j].dr, varlocbuf[j].di) ;
            printf("\n    ") ;
          }
          for (j=0 ; j<ibuf ; ++j)
            printf("  (dd:)(%11.4e,%11.4e)", ddbuf[j].dr, ddbuf[j].di) ;
          printf("\n") ;
          ibuf = 0 ;
        }
        if (conclude==1) {
          cdcmplx varb ;
          varb.dr = dbad_nextRandom();
          varb.di = dbad_nextRandom();
          dbad_condensed_dd  += dd.dr*varb.dr      + dd.di*varb.di ;
          dbad_condensed_tgt += vard[i].dr*varb.dr + vard[i].di*varb.di ;
        }
      }
    }
  }
}

void dbad_adDebugTgt_testReal8Array(char* varname, double* var, double* vard, int length, int conclude) {
  if (!var || !vard) return ;
  if (dbad_testThisProcess!=-2) {
    int i ;
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      for (i=0 ; i<length ; ++i) {
        dbad_putOne8(var[i]) ;
      }
    } else if (dbad_phase==2) {
      double ddvar, dd, vardbuf[10], ddbuf[10], varepsbuf[10], varlocbuf[10] ;
      float diff ;
      int hasDifference, printedheader = 0, indexbuf[10], ibuf = 0 ;
      dbad_ddcheckvarname(varname) ;
      for (i=0 ; i<length ; ++i) {
        dbad_getOne8(&ddvar) ;
        hasDifference = dbad_discrepancy8(ddvar, var[i], vard[i], &dd, &diff) ;
        if (dbad_trace) {
          varepsbuf[ibuf] = ddvar ;
          varlocbuf[ibuf] = var[i] ;
        }
        if (hasDifference || dbad_trace) {
          vardbuf[ibuf] = vard[i] ;
          ddbuf[ibuf] = dd ;
          indexbuf[ibuf] = i ;
          ++ibuf ;
        }
        if (conclude==-1 && hasDifference) { //Nudge the ad derivative back to divided-diff value!
          vard[i] = dd ;
        }
        if (ibuf>=10 || (i==length-1 && ibuf>0)) {
          int j ;
          if (!printedheader) {
            if (conclude==-1)
              printf("%s RESET:\n", varname) ;
            else
              printf("%s DIFFS:\n", varname) ;
            printedheader = 1 ;
          }
          printf("    ") ;
          for (j=0 ; j<ibuf ; ++j)
            printf(" %4i->%11.4e", indexbuf[j], vardbuf[j]) ;
          printf("\n    ") ;
          if (dbad_trace) {
            for (j=0 ; j<ibuf ; ++j)
              printf("  (eps)%11.4e", varepsbuf[j]) ;
            printf("\n    ") ;
            for (j=0 ; j<ibuf ; ++j)
              printf("  (loc)%11.4e", varlocbuf[j]) ;
            printf("\n    ") ;
          }
          for (j=0 ; j<ibuf ; ++j)
            printf("  (dd:)%11.4e", ddbuf[j]) ;
          printf("\n") ;
          ibuf = 0 ;
        }
        if (conclude==1) {
          double varb = dbad_nextRandom() ;
          dbad_condensed_dd += dd*varb;
          dbad_condensed_tgt += vard[i]*varb;
        }
      }
    }
  }
}

void dbad_adDebugTgt_testReal4Array(char* varname, float* var, float* vard, int length, int conclude) {
  if (!var || !vard) return ;
  if (dbad_testThisProcess!=-2) {
    int i ;
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      for (i=0 ; i<length ; ++i) {
        dbad_putOne4(var[i]) ;
      }
    } else if (dbad_phase==2) {
      float ddvar, dd, vardbuf[10], ddbuf[10], varepsbuf[10], varlocbuf[10] ;
      float diff ;
      int hasDifference, printedheader = 0, indexbuf[10], ibuf = 0 ;
      dbad_ddcheckvarname(varname) ;
      for (i=0 ; i<length ; ++i) {
        dbad_getOne4(&ddvar) ;
        hasDifference = dbad_discrepancy4(ddvar, var[i], vard[i], &dd, &diff) ;
        if (dbad_trace) {
          varepsbuf[ibuf] = ddvar ;
          varlocbuf[ibuf] = var[i] ;
        }
        if (hasDifference || dbad_trace) {
          vardbuf[ibuf] = vard[i] ;
          ddbuf[ibuf] = dd ;
          indexbuf[ibuf] = i ;
          ++ibuf ;
        }
        if (conclude==-1 && hasDifference) { //Nudge the ad derivative back to divided-diff value!
          vard[i] = dd ;
        }
        if (ibuf>=10 || (i==length-1 && ibuf>0)) {
          int j ;
          if (!printedheader) {
            if (conclude==-1)
              printf("%s RESET:\n", varname) ;
            else
              printf("%s DIFFS:\n", varname) ;
            printedheader = 1 ;
          }
          printf("    ") ;
          for (j=0 ; j<ibuf ; ++j)
            printf(" %4i->%11.4e", indexbuf[j], vardbuf[j]) ;
          printf("\n    ") ;
          if (dbad_trace) {
            for (j=0 ; j<ibuf ; ++j)
              printf("  (eps)%11.4e", varepsbuf[j]) ;
            printf("\n    ") ;
            for (j=0 ; j<ibuf ; ++j)
              printf("  (loc)%11.4e", varlocbuf[j]) ;
            printf("\n    ") ;
          }
          for (j=0 ; j<ibuf ; ++j)
            printf("  (dd:)%11.4e", ddbuf[j]) ;
          printf("\n") ;
          ibuf = 0 ;
        }
        if (conclude==1) {
          double varb = dbad_nextRandom() ;
          dbad_condensed_dd += ((double)dd)*varb;
          dbad_condensed_tgt += ((double)vard[i])*varb;
        }
      }
    }
  }
}

//###################### DEBUG OF TANGENT ##############################

void adDebugTgt_init(double epsilon, double seed, int tested_process) {
  //argument "tested_process" is used only in the MPI case!!
  dbad_mode = 1 ;
  dbad_ddeps = epsilon ;
  dbad_seed = seed ;
  dbad_topContext.funcname = "TESTED CODE\0" ;
  dbad_topContext.deltadepth = 0 ;
  dbad_topContext.code = 0 ;
  dbad_calltracedepth = 1 ;
  dbad_callStack = &dbad_topContext ;
  char* phase = getenv("DBAD_PHASE") ;
  if (phase==NULL) {
    printf("Please set DBAD_PHASE environment variable to 1 (perturbed), 2 (tangent), or 0 (no debug)\n") ;
    exit(0) ;
  } else if (strcmp(phase,"0")==0) {
    dbad_phase = 0 ;
  } else if (strcmp(phase,"1")==0) {
    dbad_phase = 1 ;
  } else if (strcmp(phase,"2")==0) {
    dbad_phase = 2 ;
  } else if (strcmp(phase,"-1")==0) {
    dbad_phase = 1 ;
    dbad_trace = 1 ;
  } else if (strcmp(phase,"-2")==0) {
    dbad_phase = 2 ;
    dbad_trace = 1 ;
  } else {
    printf("DBAD_PHASE environment variable must be set to 1 (perturbed), 2 (tangent), or 0 (no debug)\n") ;
    exit(0) ;
  }
  char* fifoFileName = "/tmp/DBAD_fifo" ;
  dbad_testThisProcess = -1 ;
  if (dbad_testThisProcess!=-2) {
    if (dbad_trace) {
      printf("INTERNAL TESTS, epsilon=%7.1e\n", epsilon) ;
      printf("===========================================================\n") ;
    }
    if (dbad_phase==1) {
      if (dbad_testThisProcess==-1) {
        printf("Starting TGT test, phase one, epsilon=%7.1e [seed=%7.1e]\n",
               epsilon, seed) ;
      } else {
        printf("[process %i] Starting TGT test, phase one, epsilon=%7.1e [seed=%7.1e]\n",
               dbad_testThisProcess, epsilon, seed) ;
      }
      printf("===========================================================\n") ;
      dbad_resetCondensors() ;
      mkfifo(fifoFileName, S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH | S_IRWXU | S_IRWXO) ;
      dbad_file = fopen(fifoFileName, "a") ;
      if (dbad_file==NULL) {
        char errbuf[20] ;
        strerror_r(errno, errbuf, 20) ;
        printf("FIFO ERROR %i: %s  OR  %s\n",errno,strerror(errno),errbuf) ;
        exit(0) ;
      }
    } else if (dbad_phase==2) {
      if (dbad_testThisProcess==-1) {
        printf("Starting TGT test, phase two, epsilon=%7.1e [seed=%7.1e]\n",
               epsilon, seed) ;
      } else {
        printf("[process %i] Starting TGT test, phase two, epsilon=%7.1e [seed=%7.1e]\n",
               dbad_testThisProcess, epsilon, seed) ;
      }
      printf("===========================================================\n") ;
      dbad_resetCondensors() ;
      dbad_file = fopen(fifoFileName, "r") ;
    }
  }
}

void adDebugTgt_call(char* unitname, int deltadepth, int forcetraced) {
  if (dbad_trace) {
    printf("call %s deltadepth:%i forcetraced:%i\n", unitname, deltadepth, forcetraced) ;
  }
  if (dbad_phase!=0) {
    dbad_pushCallFrame(unitname, deltadepth, forcetraced) ;
  }
}

void adDebugTgt_exit() {
  if (dbad_trace) {
    printf("exit\n") ;
  }
  if (dbad_phase!=0) {
    dbad_popCallFrame() ;
  }
  dbad_resetCondensors() ;
}

int adDebugTgt_here(char* placename, int forcetraced) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_trace) {
      printf("here?? %s forcetraced:%i\n", placename, forcetraced) ;
      printf("    will return %i\n", (dbad_phase==0?0:dbad_debughere(forcetraced))) ;
    }
    if (dbad_phase==0) return 0 ;
    return dbad_debughere(forcetraced) ;
  } else {
    return 0 ;
  }
}

void adDebugTgt_initComplex16(char* varname, cdcmplx *indep, cdcmplx *indepd) {
  indepd->dr = dbad_nextRandom() ;
  indepd->di = dbad_nextRandom() ;
/*   dbad_scaleIndepDirection??(indepd, *indep) ; // One may prefer to comment this line out... */
  if (dbad_phase==1) {
    indep->dr = (indep->dr)+dbad_ddeps*(indepd->dr) ;
    indep->di = (indep->di)+dbad_ddeps*(indepd->di) ;
  }
  if (dbad_trace)
    printf("initComplex16 of %s: (%24.16e,%24.16e) //(%24.16e,%24.16e)\n", varname, indep->dr, indep->di, indepd->dr, indepd->di) ;
}

void adDebugTgt_initReal8(char* varname, double *indep, double *indepd) {
  *indepd = dbad_nextRandom() ;
/*   dbad_scaleIndepDirection8(indepd, *indep) ; // One may prefer to comment this line out... */
  if (dbad_phase==1)
    *indep = (*indep)+dbad_ddeps*(*indepd) ;
  if (dbad_trace)
    printf("initReal8 of %s: %24.16e //%24.16e\n", varname, *indep, *indepd) ;
}

void adDebugTgt_initReal4(char* varname, float *indep, float *indepd) {
  *indepd = (float)dbad_nextRandom() ;
/*   dbad_scaleIndepDirection4(indepd, *indep) ; // One may prefer to comment this line out... */
  if (dbad_phase==1)
    *indep = (*indep)+dbad_ddeps*(*indepd) ;
  if (dbad_trace)
    printf("initReal4 of %s: %24.16e //%24.16e\n", varname, (double)*indep, (double)*indepd) ;
}

void adDebugTgt_initComplex16Array(char* varname, cdcmplx *indep, cdcmplx *indepd, int length) {
  if (!indep || !indepd) return ;
  int i ;
  for (i=0 ; i<length ; ++i) {
    indepd[i].dr = dbad_nextRandom() ;
    indepd[i].di = dbad_nextRandom() ;
/* printf("nextRandom %i (%24.16e,%24.16e)\n", i, indepd[i].dr, indepd[i].di) ; */
/*     dbad_scaleIndepDirection??(&indepd[i], indep[i]) ; // One may prefer to comment this line out... */
/* printf(" -> scaled %i (%24.16e,%24.16e)\n", i, indepd[i].dr, indepd[i].di) ; */
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i].dr += dbad_ddeps*indepd[i].dr ;
      indep[i].di += dbad_ddeps*indepd[i].di ;
    }
  }
  if (dbad_trace) {
    printf("initComplex16Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:(%24.16e,%24.16e) //(%24.16e,%24.16e)",i,indep[i].dr,indep[i].di,indepd[i].dr,indepd[i].di) ;
    printf("\n") ;
  }
}

void adDebugTgt_initReal8Array(char* varname, double *indep, double *indepd, int length) {
  if (!indep || !indepd) return ;
  int i ;
  for (i=0 ; i<length ; ++i) {
    indepd[i] = dbad_nextRandom() ;
/* printf("nextRandom %i %24.16e\n", i, indepd[i]) ; */
/*     dbad_scaleIndepDirection8(&indepd[i], indep[i]) ; // One may prefer to comment this line out... */
/* printf(" -> scaled %i %24.16e\n", i, indepd[i]) ; */
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i] = indep[i]+dbad_ddeps*indepd[i] ;
    }
  }
  if (dbad_trace) {
    printf("initReal8Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e //%24.16e",i,indep[i],indepd[i]) ;
    printf("\n") ;
  }
}

void adDebugTgt_initReal4Array(char* varname, float *indep, float *indepd, int length) {
  if (!indep || !indepd) return ;
  int i ;
  for (i=0 ; i<length ; ++i) {
    indepd[i] = (float)dbad_nextRandom() ;
/*     dbad_scaleIndepDirection4(&indepd[i], indep[i]) ; // One may prefer to comment this line out... */
  }
  if (dbad_phase==1) {
    for (i=0 ; i<length ; ++i) {
      indep[i] = indep[i]+dbad_ddeps*indepd[i] ;
    }
  }
  if (dbad_trace) {
    printf("initReal4Array of %s, length=%i:\n", varname, length) ;
    for (i=0 ; i<length ; ++i)
      printf("    %i:%24.16e //%24.16e",i,(double)indep[i],(double)indepd[i]) ;
    printf("\n") ;
  }
}

void adDebugTgt_passiveComplex16(char *varname, cdcmplx var) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      dbad_putOne8(var.dr) ;
      dbad_putOne8(var.di) ;
    } else if (dbad_phase==2) {
      cdcmplx ddvar ;
      dbad_ddcheckvarname(varname) ;
      dbad_getOne8(&(ddvar.dr)) ;
      dbad_getOne8(&(ddvar.di)) ;
      if (dbad_trace) {
        printf("passiveComplex16 %s v-eps:(%24.16e,%24.16e) v-loc:(%24.16e,%24.16e) are %s\n",
               varname,ddvar.dr,ddvar.di,var.dr,var.di,(ddvar.dr==var.dr && ddvar.di==var.di?"equal":"different")) ;
      } else if (ddvar.dr!=var.dr || ddvar.di!=var.di) {
        printf("passiveComplex16 %s appears to be varied (v-eps:(%24.16e,%24.16e) v-loc:(%24.16e,%24.16e)). Hope it is really not useful!\n",
               varname,ddvar.dr,ddvar.di,var.dr,var.di) ;
      }
    }
  }
}

void adDebugTgt_passiveReal8(char *varname, double var) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      dbad_putOne8(var) ;
    } else if (dbad_phase==2) {
      double ddvar ;
      dbad_ddcheckvarname(varname) ;
      dbad_getOne8(&ddvar) ;
      if (dbad_trace) {
        printf("passiveReal8 %s v-eps:%24.16e v-loc:%24.16e are %s\n",
               varname,ddvar,var,(ddvar==var?"equal":"different")) ;
      } else if (ddvar!=var) {
        printf("passiveReal8 %s appears to be varied (v-eps:%24.16e v-loc:%24.16e). Hope it is really not useful!\n",
               varname,ddvar,var) ;
      }
    }
  }
}

void adDebugTgt_passiveReal4(char *varname, float var) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_phase==1) {
      dbad_putOneVarName(varname) ;
      dbad_putOne4(var) ;
    } else if (dbad_phase==2) {
      float ddvar ;
      dbad_ddcheckvarname(varname) ;
      dbad_getOne4(&ddvar) ;
      if (dbad_trace) {
        printf("passiveReal4 %s v-eps:%18.10 v-loc:%18.10 are %s\n",
               varname,ddvar,var,(ddvar==var?"equal":"different")) ;
      } else if (ddvar!=var) {
        printf("passiveReal4 %s appears to be varied (v-eps:%18.10 v-loc:%18.10). Hope it is really not useful!\n",
               varname,ddvar,var) ;
      }
    }
  }
}

void adDebugTgt_passiveComplex16Array(char *varname, cdcmplx *var, int length) {
  if (!var) return;
  if (dbad_testThisProcess!=-2) {
    int i ;
    double varsum = 0.0 ;
    for (i=0 ; i<length ; ++i) {
      varsum += var[i].dr + var[i].di ;
    }
    adDebugTgt_passiveReal8(varname, varsum) ;
  }
}

void adDebugTgt_passiveReal8Array(char *varname, double *var, int length) {
  if (!var) return;
  if (dbad_testThisProcess!=-2) {
    int i ;
    double varsum = 0.0 ;
    for (i=0 ; i<length ; ++i) {
      varsum += var[i] ;
    }
    adDebugTgt_passiveReal8(varname, varsum) ;
  }
}

void adDebugTgt_passiveReal4Array(char *varname, float *var, int length) {
  if (!var) return;
  if (dbad_testThisProcess!=-2) {
    int i ;
    float varsum = 0.0 ;
    for (i=0 ; i<length ; ++i) {
      varsum += var[i] ;
    }
    adDebugTgt_passiveReal4(varname, varsum) ;
  }
}

void adDebugTgt_testComplex16(char *varname, cdcmplx var, cdcmplx *vard) {
  dbad_adDebugTgt_testComplex16(varname, var, vard, 0) ; //replace 0 with -1 to nudge vard
}

void adDebugTgt_testReal8(char *varname, double var, double *vard) {
  dbad_adDebugTgt_testReal8(varname, var, vard, 0) ; //replace 0 with -1 to nudge vard
}

void adDebugTgt_testReal4(char *varname, float var, float *vard) {
  dbad_adDebugTgt_testReal4(varname, var, vard, 0) ; //replace 0 with -1 to nudge vard
}

void adDebugTgt_testComplex16Array(char *varname, cdcmplx* var, cdcmplx* vard, int length) {
  dbad_adDebugTgt_testComplex16Array(varname, var, vard, length, 0) ;
}

void adDebugTgt_testReal8Array(char *varname, double* var, double* vard, int length) {
  dbad_adDebugTgt_testReal8Array(varname, var, vard, length, 0) ; //replace 0 with -1 to nudge vard
}

void adDebugTgt_testReal4Array(char *varname, float* var, float* vard, int length) {
  dbad_adDebugTgt_testReal4Array(varname, var, vard, length, 0) ; //replace 0 with -1 to nudge vard
}

void adDebugTgt_concludeComplex16(char* varname, cdcmplx var, cdcmplx *vard) {
  dbad_adDebugTgt_testComplex16(varname, var, vard, 1) ;
}

void adDebugTgt_concludeReal8(char* varname, double var, double *vard) {
  dbad_adDebugTgt_testReal8(varname, var, vard, 1) ;
}

void adDebugTgt_concludeReal4(char* varname, float var, float *vard) {
  dbad_adDebugTgt_testReal4(varname, var, vard, 1) ;
}

void adDebugTgt_concludeComplex16Array(char* varname, cdcmplx *var, cdcmplx *vard, int length) {
  dbad_adDebugTgt_testComplex16Array(varname, var, vard, length, 1) ;
}

void adDebugTgt_concludeReal8Array(char* varname, double *var, double *vard, int length) {
//printf(" C :%24.16e d:%24.16e\n", var[0], vard[0]) ;
  dbad_adDebugTgt_testReal8Array(varname, var, vard, length, 1) ;
}

void adDebugTgt_concludeReal4Array(char* varname, float *var, float *vard, int length) {
  dbad_adDebugTgt_testReal4Array(varname, var, vard, length, 1) ;
}

void adDebugTgt_conclude() {
  if (dbad_testThisProcess!=-2) {
    printf("===========================================================\n") ;
    if (dbad_trace) {
      if (dbad_testThisProcess==-1) {
        printf("Condensed results: %24.16e //%24.16e",dbad_condensed_tgt,dbad_condensed_dd) ;
      } else {
        printf("[process %i] Condensed results: %24.16e //%24.16e",dbad_testThisProcess,dbad_condensed_tgt,dbad_condensed_dd) ;
      }
    }
    if (dbad_phase==2) {
      double abstgt, absdd, maxabs, abserror, diff ;
      abstgt = (dbad_condensed_tgt>=0.0?dbad_condensed_tgt:-dbad_condensed_tgt) ;
      absdd = (dbad_condensed_dd>=0.0?dbad_condensed_dd:-dbad_condensed_dd) ;
      maxabs = (abstgt>absdd?abstgt:absdd) ;
      abserror = dbad_condensed_tgt-dbad_condensed_dd ;
      if (abserror<0.0) abserror=-abserror ;
      diff = (abserror*100.0)/ maxabs ;
      if (dbad_testThisProcess==-1) {
        printf("Condensed tangent: %24.16e (ad)%5.1f%% DIFF WITH (dd)%24.16e [seed:%7.1e]\n",
               dbad_condensed_tgt,diff,dbad_condensed_dd,dbad_seed) ;
      } else {
        printf("[process %i] Condensed tangent: %24.16e (ad)%5.1f%% DIFF WITH (dd)%24.16e [seed:%7.1e]\n",
               dbad_testThisProcess,dbad_condensed_tgt,diff,dbad_condensed_dd,dbad_seed) ;
      }
    }
  }
}

void adDebugTgt_display(char *placename) {
  if (dbad_testThisProcess!=-2) {
    if (dbad_trace) {
      if (dbad_testThisProcess==-1) {
        printf("display %s\n", placename) ;
      } else {
        printf("[process %i] display %s\n", dbad_testThisProcess, placename) ;
      }
    }
    if (dbad_phase==2) {
      dbad_display_location(placename) ;
    }
  }
}

//############## DEBUG OF ADJOINT, FIRST SWEEP: ADJOINT RUN ################

void adDebugBwd_init(double errmax, double seed) {
  dbad_mode = -1 ;
  dbad_phase = 1 ;
  dbad_errormax = errmax ;
  dbad_seed = seed ;
  dbad_topContext.funcname =  "TESTED CODE\0" ;
  dbad_topContext.deltadepth = 0 ;
  dbad_topContext.code = 0 ;
  dbad_calltracedepth = 1 ;
  dbad_callStack = &dbad_topContext ;
  char* phase = getenv("DBAD_PHASE") ;
  if (phase==NULL) {
    printf("Please set DBAD_PHASE environment variable to 0 (no debug), 1 (sendToTgt), or -1 (plusTraces)\n") ;
    dbad_phase = 1 ;
  } else if (strcmp(phase,"0")==0) {
    dbad_phase = 1 ;
    dbad_nocommunication = 1 ;
  } else if (strcmp(phase,"1")==0) {
    dbad_phase = 1 ;
  } else if (strcmp(phase,"-1")==0) {
    dbad_phase = 1 ;
    dbad_trace = 1 ;
  } else {
    printf("DBAD_PHASE environment variable must be set to 0 (no debug), 1 (sendToTgt), or -1 (plusTraces)\n") ;
    exit(0) ;
  }
  printf("Starting ADJ test, phase one (bwd), errmax=%4.1f% [seed=%7.1e]\n", errmax, seed) ;
  printf("===========================================================\n") ;
  if (dbad_nocommunication) {
    dbad_file = NULL ;
    printf("FIFO COMMUNICATION TURNED OFF !\n") ;
  } else {
    mkfifo("/tmp/DBAD_fifo", S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH | S_IRWXU | S_IRWXO) ;
    dbad_file = fopen("/tmp/DBAD_fifo", "a") ;
    if (dbad_file==NULL) {
      char errbuf[20] ;
      strerror_r(errno, errbuf, 20) ;
      printf("FIFO ERROR %i: %s  OR  %s\n",errno,strerror(errno),errbuf) ;
      exit(0) ;
    }
  }
  dbad_resetCondensors() ;
}

void adDebugBwd_call(char *funcname, int deltadepth) {
  dbad_pushCallFrame(funcname, deltadepth, 0) ;
}

void adDebugBwd_exit() {
  dbad_resetCondensors() ;
  if (dbad_debugabove()) {
    if (dbad_nocommunication) {
      printf("adDebug would send (%i %s)\n", (dbad_debughere(0)?2:-2), dbad_callStack->funcname) ;
    } else {
      fprintf(dbad_file, "%i\n", (dbad_debughere(0)?2:-2)) ;
      fprintf(dbad_file, "%s\n", dbad_callStack->funcname) ;
    }
  }
  dbad_popCallFrame() ;
}

int adDebugBwd_here(char* placename) {
  dbad_resetCondensors() ;
  return adDebugTgt_here(placename, 0) ;
}

//############## DEBUG OF ADJOINT, SECOND SWEEP: TANGENT RUN ################

void adDebugFwd_init(double errmax, double seed) {
  dbad_mode = -1 ;
  dbad_phase = 2 ;
  dbad_errormax = errmax ;
  dbad_seed = seed ;
  dbad_topContext.funcname = "TESTED CODE\0" ;
  dbad_topContext.deltadepth = 0 ;
  dbad_topContext.code = 0 ;
  dbad_calltracedepth = 1 ;
  dbad_callStack = &dbad_topContext ;
  char* phase = getenv("DBAD_PHASE") ;
  if (phase==NULL) {
    printf("Please set DBAD_PHASE environment variable to 0 (no debug), 2 (readFromAdj), or -2 (plusTraces)\n") ;
    dbad_phase = 2 ;
  } else if (strcmp(phase,"0")==0) {
    dbad_phase = 2 ;
    dbad_nocommunication = 1 ;
  } else if (strcmp(phase,"2")==0) {
    dbad_phase = 2 ;
  } else if (strcmp(phase,"-2")==0) {
    dbad_phase = 2 ;
    dbad_trace = 1 ;
  } else {
    printf("DBAD_PHASE environment variable must be set to 0 (no debug), 2 (readFromAdj), or -2 (plusTraces)\n") ;
    exit(0) ;
  }
  dbad_nberrors = 0 ;
  printf("Starting ADJ test, phase two (fwd), errmax=%4.1f% [seed=%7.1e]\n", errmax, seed) ;
  printf("===========================================================\n") ;
  if (dbad_nocommunication) {
    dbad_file = NULL ;
    printf("FIFO COMMUNICATION TURNED OFF !\n") ;
  } else {
    dbad_file = fopen("/tmp/DBAD_fifo", "r") ;
    dbad_resetCondensors() ;
    /* Convention on the meaning of labels:
       -1 -> a debug point, skipped
        0 -> a debug point, traced but no associated value.
        1 -> a debug point, traced, with an associated value.
       -2 -> a call, skipped
        2 -> a call, traced
    */
    int ret=0 ;
    int label ;
    char placename[40] ;
    double value ;
    while (1) {
      ret = fscanf(dbad_file, "%i\n", &label) ;
      if (ret!=1) break ;
      ret = fscanf(dbad_file, "%s\n", placename) ;
      if (label==1) {
        ret = fscanf(dbad_file, "%lf\n", &value) ;
        dbad_pushReal8(value) ;
      }
      pushCharacterArray(placename, 40) ;
      dbad_pushinteger4(label) ;
    }
  }
}

void adDebugFwd_call(char *funcname) {
  int label ;
  char funcnamefrom[40] ;
  char funcnamehere[40] ;
  // In the special debug^2 case, on the 2nd phase (tangent) of the debugAdj, with DBAD_PHASE=0,
  // push the call frame but do essentially nothing!
  if (dbad_debughere(0) && !(dbad_nocommunication && dbad_phase==2)) {
    dbad_popinteger4(&label) ;
    if (label!=2 && label!=-2) {
      printf("Control mismatch, expecting a trace (-2or2) from %s bwd call exit, got %i\n",funcname,label) ;
      exit(0) ;
    }
    popCharacterArray(funcnamefrom, 40) ;
    sprintf(funcnamehere,"%s",funcname) ;
    if (strcmp(funcnamefrom,funcnamehere)!=0) {
      printf("Control mismatch, expecting a call to %s, got %s\n",funcnamehere,funcnamefrom) ;
      exit(0) ;
    }
    dbad_pushCallFrame(funcname, 0, 0) ;
    if (label==2) { // then the call is traced:
      dbad_callStack->deltadepth += (dbad_calltracedepth-1) ;
      dbad_calltracedepth = 1 ;
    } else { // then label==-2: the call is not traced:
      dbad_callStack->deltadepth += dbad_calltracedepth ;
      dbad_calltracedepth = 0 ;
    }
  } else {
    dbad_pushCallFrame(funcname, 0, 0) ;
  }
}

void adDebugFwd_exit() {
  dbad_popCallFrame() ;
}

int adDebugFwd_here(char* placename) {
  // In the special debug^2 case, on the 2nd phase (tangent) of the debugAdj, with DBAD_PHASE=0,
  // never go into the derivative manipulation body, except to st the inputs at the very "start"
  // and to print the result at the very "end".
  if (dbad_nocommunication && dbad_phase==2) {
    if (strcmp(placename,"end")==0 || strcmp(placename,"start")==0)
      return 1 ;
    else
      return 0 ;
  } else {
    if (dbad_debughere(0)) {
      int label ;
      char placenamefrom[40] ;
      char placenamehere[40] ;
      dbad_resetCondensors() ;
      dbad_popinteger4(&label) ;
      if (label!=1 && label!=-1 && label!=0) {
        printf("Control mismatch, expecting a trace (-1or0or1) from place %s, got %i\n",placename,label) ;
        exit(0) ;
      }
      popCharacterArray(placenamefrom, 40) ;
      sprintf(placenamehere,"%s",placename) ;
      if (strcmp(placenamefrom,placenamehere)!=0) {
        printf("Control mismatch, expecting place %s, got %s\n",placenamehere,placenamefrom) ;
        exit(0) ;
      }
      if (label==1) {
        dbad_popReal8(&dbad_nextrefsum) ;
      }
      return label!=-1 ;
    } else {
      return 0 ;
    }
  }
}

//############## DEBUG OF ADJOINT, FOR BOTH SWEEPS: ################

void adDebugAdj_rwComplex16(cdcmplx *vard) {
  double varbR = dbad_nextRandom() ;
  double varbI = dbad_nextRandom() ;
  dbad_condensed_adj += varbR*(vard->dr) + varbI*(vard->di) ;
  vard->dr = varbR ;
  vard->di = varbI ;
}

void adDebugAdj_rwReal8(double *vard) {
  double varb = dbad_nextRandom() ;
  dbad_condensed_adj += varb*(*vard) ;
  *vard = varb ;
}

void adDebugAdj_rwReal4(float *vard) {
  double varb = dbad_nextRandom() ;
  dbad_condensed_adj += varb*(*vard) ;
  *vard = (float)varb ;
}

void adDebugAdj_rComplex16(cdcmplx *vard) {
  double varbR = dbad_nextRandom() ;
  double varbI = dbad_nextRandom() ;
  dbad_condensed_adj += varbR*(vard->dr) + varbI*(vard->di) ;
}

/** Although at present this routine doesn't modify its argument,
 * we still expect a reference for consistency
 * with adDebugAdj_wReal8() and also adDebugAdj_rComplex16() */
void adDebugAdj_rReal8(double *vard) {
  double varb = dbad_nextRandom() ;
  dbad_condensed_adj += varb*(*vard) ;
}

/** Although at present this routine doesn't modify its argument,
 * we still expect a reference for consistency
 * with adDebugAdj_wReal4() and also adDebugAdj_rComplex16() */
void adDebugAdj_rReal4(float *vard) {
  double varb = dbad_nextRandom() ;
  dbad_condensed_adj += varb*(*vard) ;
}

void adDebugAdj_wComplex16(cdcmplx *vard) {
  vard->dr = dbad_nextRandom() ;
  vard->di = dbad_nextRandom() ;
}

void adDebugAdj_wReal8(double *vard) {
  *vard = dbad_nextRandom() ;
}

void adDebugAdj_wReal4(float *vard) {
  *vard = (float)dbad_nextRandom() ;
}

void adDebugAdj_rwComplex16Array(cdcmplx *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_rwComplex16(&(vard[i])) ;
}

void adDebugAdj_rwReal8Array(double *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_rwReal8(&(vard[i])) ;
}

void adDebugAdj_rwReal4Array(float *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_rwReal4(&(vard[i])) ;
}

void adDebugAdj_rComplex16Array(cdcmplx *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_rComplex16(&(vard[i])) ;
}

void adDebugAdj_rReal8Array(double *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_rReal8(&(vard[i])) ;
}

void adDebugAdj_rReal4Array(float *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_rReal4(&(vard[i])) ;
}

void adDebugAdj_wComplex16Array(cdcmplx *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_wComplex16(&(vard[i])) ;
}

void adDebugAdj_wReal8Array(double *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_wReal8(&(vard[i])) ;
}

void adDebugAdj_wReal4Array(float *vard, int length) {
  int i ;
  if (vard)
    for (i=0 ; i<length ; ++i)
      adDebugAdj_wReal4(&(vard[i])) ;
}

void adDebugAdj_rwDisplay(char *placename, int indent) {
  adDebugAdj_rDisplay(placename, indent) ;
  if (dbad_phase==2)
    dbad_refsum = dbad_nextrefsum ;
}

void adDebugAdj_rDisplay(char *placename, int indent) {
  if (dbad_phase==1) {
    if (dbad_nocommunication) {
      printf("adDebug would send (1 %s %24.16e)\n", placename, dbad_condensed_adj) ;
    } else {
      fprintf(dbad_file, "1\n") ;
      fprintf(dbad_file, "%s\n", placename) ;
      fprintf(dbad_file, "%24.16e\n", dbad_condensed_adj) ;
    }
  } else if (dbad_phase==2) {
    // In the special debug^2 case, on the 2nd phase (tangent) of the debugAdj, with DBAD_PHASE=0,
    // adDebugAdj_wdisplay is called ony on the "end" location. Print the tangent result:
    if (dbad_nocommunication) {
      printf("Condensed tangent result is %24.16e\n", dbad_condensed_adj) ;
    } else {
      double absref = (dbad_refsum>=0.0?dbad_refsum:-dbad_refsum) ;
      double absadj = (dbad_condensed_adj>=0.0?dbad_condensed_adj:-dbad_condensed_adj) ;
      double absdiff = dbad_refsum - dbad_condensed_adj ;
      if (absdiff<0.0) absdiff = -absdiff ;
      double reldiff = (absdiff*200.0)/(absref+absadj) ;
      if (reldiff>dbad_errormax) {
        printf("                         %5.1f%% DIFFERENCE!!  tgt:%24.16e  adj:%24.16e\n",
               reldiff, dbad_condensed_adj, dbad_refsum) ;
        ++dbad_nberrors ;
      } else if (strcmp(placename,"end")==0 && dbad_nberrors==0) {
        // When we are at end and no errors were found, always show the compared values
        printf("                         difference is just %7.3f% between tgt:%24.16e and adj:%24.16e\n",
               reldiff, dbad_condensed_adj, dbad_refsum) ;
      }
      if (indent==0) dbad_display_location(placename) ;
    }
  }
  dbad_resetCondensors() ;
}

void adDebugAdj_wDisplay(char *placename, int indent) {
  if (dbad_phase==1) {
    if (dbad_nocommunication) {
      printf("adDebug would send (0 %s)\n", placename) ;
    } else {
      fprintf(dbad_file, "0\n") ;
      fprintf(dbad_file, "%s\n", placename) ;
    }
  } else if (dbad_phase==2) {
    if (indent==0) dbad_display_location(placename) ;
    dbad_refsum = dbad_nextrefsum ;
  }
  dbad_resetCondensors() ;
}

void adDebugAdj_skip(char *placename) {
  if (dbad_phase==1 && dbad_debughere(0)) {
    if (dbad_nocommunication) {
      printf("adDebug would send (-1 %s)\n", placename) ;
    } else {
      fprintf(dbad_file, "-1\n") ;
      fprintf(dbad_file, "%s\n", placename) ;
    }
  }
}

void adDebugAdj_conclude() {
  if (dbad_phase==2) {
    if (!dbad_nocommunication) {
      // In the special debug^2 case, on the 2nd phase (tangent) of the debugAdj, with DBAD_PHASE=0,
      // don't claim that any testing has been done!! but show the expected condensed tangent:
      printf("End of ADJ test, %i error(s) found.\n", dbad_nberrors) ;
      printf("===========================================================\n") ;
    }
  }
}

/* void adDebugAdj_show() { */
/*   printf("Present sum %24.16e, current seed is %f (%f)\n", dbad_condensed_adj, dbad_currentSeed, dbad_seed) ; */
/* } */

//############## INTERFACE PROCEDURES CALLED FROM FORTRAN ################

void addebugtgt_init_(double *epsilon, double *seed, int *tested_process) {
  adDebugTgt_init(*epsilon, *seed, *tested_process) ;
}

void addebugtgt_call_(char* unitname, int *deltadepth, int *forcetraced) {
  adDebugTgt_call(unitname, *deltadepth, *forcetraced) ;
}

void addebugtgt_exit_() {
  adDebugTgt_exit() ;
}

int addebugtgt_here_(char* placename, int *forcetraced) {
  return adDebugTgt_here(placename, *forcetraced) ;
}

void addebugtgt_initcomplex16_(char* varname, cdcmplx *indep, cdcmplx *indepd) {
  adDebugTgt_initComplex16(varname, indep, indepd) ;
}

void addebugtgt_initreal8_(char* varname, double *indep, double *indepd) {
  adDebugTgt_initReal8(varname, indep, indepd) ;
}

void addebugtgt_initreal4_(char* varname, float *indep, float *indepd) {
  adDebugTgt_initReal4(varname, indep, indepd) ;
}

void addebugtgt_initcomplex16array_(char* varname, cdcmplx *indep, cdcmplx *indepd, int *length) {
  adDebugTgt_initComplex16Array(varname, indep, indepd, *length) ;
}

void addebugtgt_initreal8array_(char* varname, double *indep, double *indepd, int *length) {
  adDebugTgt_initReal8Array(varname, indep, indepd, *length) ;
}

void addebugtgt_initreal4array_(char* varname, float *indep, float *indepd, int *length) {
  adDebugTgt_initReal4Array(varname, indep, indepd, *length) ;
}

void addebugtgt_passivecomplex16_(char *varname, cdcmplx *var) {
  adDebugTgt_passiveComplex16(varname, *var) ;
}

void addebugtgt_passivereal8_(char *varname, double *var) {
  adDebugTgt_passiveReal8(varname, *var) ;
}

void addebugtgt_passivereal4_(char *varname, float *var) {
  adDebugTgt_passiveReal4(varname, *var) ;
}

void addebugtgt_passivecomplex16array_(char *varname, cdcmplx *var, int *length) {
  adDebugTgt_passiveComplex16Array(varname, var, *length) ;
}

void addebugtgt_passivereal8array_(char *varname, double *var, int *length) {
  adDebugTgt_passiveReal8Array(varname, var, *length) ;
}

void addebugtgt_passivereal4array_(char *varname, float *var, int *length) {
  adDebugTgt_passiveReal4Array(varname, var, *length) ;
}

void addebugtgt_testcomplex16_(char *varname, cdcmplx *var, cdcmplx *vard) {
  adDebugTgt_testComplex16(varname, *var, vard) ;
}

void addebugtgt_testreal8_(char *varname, double *var, double *vard) {
  adDebugTgt_testReal8(varname, *var, vard) ;
}

void addebugtgt_testreal4_(char *varname, float *var, float *vard) {
  adDebugTgt_testReal4(varname, *var, vard) ;
}

void addebugtgt_testcomplex16array_(char *varname, cdcmplx* var, cdcmplx* vard, int *length) {
  adDebugTgt_testComplex16Array(varname, var, vard, *length) ;
}

void addebugtgt_testreal8array_(char *varname, double* var, double* vard, int *length) {
  adDebugTgt_testReal8Array(varname, var, vard, *length) ;
}

void addebugtgt_testreal4array_(char *varname, float* var, float* vard, int *length) {
  adDebugTgt_testReal4Array(varname, var, vard, *length) ;
}

void addebugtgt_concludecomplex16_(char* varname, cdcmplx *dep, cdcmplx *depd) {
  adDebugTgt_concludeComplex16(varname, *dep, depd) ;
}

void addebugtgt_concludereal8_(char* varname, double *dep, double *depd) {
  adDebugTgt_concludeReal8(varname, *dep, depd) ;
}

void addebugtgt_concludereal4_(char* varname, float *dep, float *depd) {
  adDebugTgt_concludeReal4(varname, *dep, depd) ;
}

void addebugtgt_concludecomplex16array_(char* varname, cdcmplx *dep, cdcmplx *depd, int *length) {
  adDebugTgt_concludeComplex16Array(varname, dep, depd, *length) ;
}

void addebugtgt_concludereal8array_(char* varname, double *dep, double *depd, int *length) {
  adDebugTgt_concludeReal8Array(varname, dep, depd, *length) ;
}

void addebugtgt_concludereal4array_(char* varname, float *dep, float *depd, int *length) {
  adDebugTgt_concludeReal4Array(varname, dep, depd, *length) ;
}

void addebugtgt_conclude_() {
  adDebugTgt_conclude() ;
}

void addebugtgt_display_(char *placename) {
  adDebugTgt_display(placename) ;
}

void addebugbwd_init_(double *errmax, double *seed) {
  adDebugBwd_init(*errmax, *seed) ;
}

void addebugbwd_call_(char *funcname, int *deltadepth) {
  adDebugBwd_call(funcname, *deltadepth) ;
}

void addebugbwd_exit_() {
  adDebugBwd_exit() ;
}

int addebugbwd_here_(char* placename) {
  return adDebugBwd_here(placename) ;
}

void addebugfwd_init_(double *errmax, double *seed) {
  adDebugFwd_init(*errmax, *seed) ;
}

void addebugfwd_call_(char *funcname) {
  adDebugFwd_call(funcname) ;
}

void addebugfwd_exit_() {
  adDebugFwd_exit() ;
}

int addebugfwd_here_(char* placename) {
  return adDebugFwd_here(placename) ;
}

void addebugadj_rwreal4_(float *vard) {
  adDebugAdj_rwReal4(vard) ;
}

void addebugadj_rwreal8_(double *vard) {
  adDebugAdj_rwReal8(vard) ;
}

void addebugadj_rwcomplex16_(cdcmplx *vard) {
  adDebugAdj_rwComplex16(vard) ;
}

void addebugadj_rreal4_(float *vard) {
  adDebugAdj_rReal4(vard) ;
}

void addebugadj_rreal8_(double *vard) {
  adDebugAdj_rReal8(vard) ;
}

void addebugadj_rcomplex16_(cdcmplx *vard) {
  adDebugAdj_rComplex16(vard) ;
}

void addebugadj_wreal4_(float *vard) {
  adDebugAdj_wReal4(vard) ;
}

void addebugadj_wreal8_(double *vard) {
  adDebugAdj_wReal8(vard) ;
}

void addebugadj_wcomplex16_(cdcmplx *vard) {
  adDebugAdj_wComplex16(vard) ;
}

void addebugadj_rwreal4array_(float *vard, int *length) {
  adDebugAdj_rwReal4Array(vard, *length) ;
}

void addebugadj_rwreal8array_(double *vard, int *length) {
  adDebugAdj_rwReal8Array(vard, *length) ;
}

void addebugadj_rwcomplex16array_(cdcmplx *vard, int *length) {
  adDebugAdj_rwComplex16Array(vard, *length) ;
}

void addebugadj_rreal4array_(float *vard, int *length) {
  adDebugAdj_rReal4Array(vard, *length) ;
}

void addebugadj_rreal8array_(double *vard, int *length) {
  adDebugAdj_rReal8Array(vard, *length) ;
}

void addebugadj_rcomplex16array_(cdcmplx *vard, int *length) {
  adDebugAdj_rComplex16Array(vard, *length) ;
}

void addebugadj_wreal4array_(float *vard, int *length) {
  adDebugAdj_wReal4Array(vard, *length) ;
}

void addebugadj_wreal8array_(double *vard, int *length) {
  adDebugAdj_wReal8Array(vard, *length) ;
}

void addebugadj_wcomplex16array_(cdcmplx *vard, int *length) {
  adDebugAdj_wComplex16Array(vard, *length) ;
}

void addebugadj_rwdisplay_(char *placename, int *indent) {
  adDebugAdj_rwDisplay(placename, *indent) ;
}

void addebugadj_rdisplay_(char *placename, int *indent) {
  adDebugAdj_rDisplay(placename, *indent) ;
}

void addebugadj_wdisplay_(char *placename, int *indent) {
  adDebugAdj_wDisplay(placename, *indent) ;
}

void addebugadj_skip_(char* placename) {
  adDebugAdj_skip(placename) ;
}

void addebugadj_conclude_() {
  adDebugAdj_conclude() ;
}
