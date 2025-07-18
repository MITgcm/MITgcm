#include "adFixedPoint.h"
#include <stdio.h>

// Up to 5 levels of nested fixed-point loops should be enough:
double refCumuls[5] = {0.0, 0.0, 0.0, 0.0, 0.0} ;
double prevCumuls[5] = {0.0, 0.0, 0.0, 0.0, 0.0} ;
int adjIters[5] = {0, 0, 0, 0, 0} ;
int fpDepth = -1 ;

/*
int adFixedPoint_notReduced(double cumul, float reduction) {
  // Dan's method: stop when cumul has reduced by a factor "reduction"
  if (cumul<0.0) {
    // Begin 1st iteration of a new adjoint FP loop.
    // Given cumul is always -1.0.
    // Just prepare for 2nd iteration, that will set the reference refCumuls[] :
    if (fpDepth>=(5-1)) return 0 ; // protect from going out of bounds.
    ++fpDepth ;
    refCumuls[fpDepth] = -1.0 ;
    prevCumuls[fpDepth] = -1.0 ;
    adjIters[fpDepth] = 1 ;
    return 1 ;
  } else {
    // Begin 2nd of any following iteration:
    int iterate ;
    int growth ;
    if (refCumuls[fpDepth]<0.0) {
      // Begin 2nd iteration of current adjoint FP loop.
      // Set reference refCumuls[] to the given cumul:
      refCumuls[fpDepth] = cumul ;
      prevCumuls[fpDepth] = cumul ;
      // Almost always iterate, except if cumul is really small (quadratic convergence?):
      iterate = (cumul >  1.e-10) ;
      growth = 0 ;
    } else {
      // Begin 3rd or any following iteration.
      // Compare with reference refCumuls:
      iterate = (cumul > reduction*refCumuls[fpDepth]) ;
      growth = (adjIters[fpDepth]>5 && cumul>prevCumuls[fpDepth]) ;
      prevCumuls[fpDepth]=cumul ;
    }
    if (iterate && !growth) {
      ++(adjIters[fpDepth]) ;
      printf("%i adjoint iterations (reduced %e -> %e)\n", adjIters[fpDepth], refCumuls[fpDepth], cumul) ;
      return 1 ;
    } else {
      if (growth) {
          printf("%i adjoint iterations (reduced %e -> %e, TERMINATED)\n", adjIters[fpDepth], refCumuls[fpDepth], cumul) ; }
      else {
          printf("%i adjoint iterations (reduced %e -> %e), CONVERGED\n", adjIters[fpDepth], refCumuls[fpDepth], cumul) ; }

      if (fpDepth<0) return 0 ; // protect from going out of bounds.
      --fpDepth ;
      return 0 ;
    }
  }
}
*/

int adFixedPoint_tooLarge(double cumul, float minCumul) {
  // Naive method: stop when cumul becomes less than given minCumul
  if (cumul<0.0) {
    // Begin 1st iteration of a new adjoint FP loop.
    // Given cumul is always -1.0.
    if (fpDepth>=(5-1)) return 0 ; // protect from going out of bounds.
    ++fpDepth ;
    adjIters[fpDepth] = 1 ;
    return 1 ;
  } else {
    // Begin 2nd of any following iteration:
    int iterate = (cumul>minCumul) ;
    if (iterate) {
      ++(adjIters[fpDepth]) ;
      return 1 ;
    } else {
/*       printf("%i adjoint iterations (residual %e < %e)\n", adjIters[fpDepth], cumul, minCumul) ; */
      if (fpDepth<0) return 0 ; // protect from going out of bounds.
      --fpDepth ;
      return 0 ;
    }
  }
}

/****************** INTERFACE CALLED FROM FORTRAN *******************/

/*
int adfixedpoint_notreduced_(double *cumul, float *reduction) {
  return adFixedPoint_notReduced(*cumul, *reduction) ;
}
*/

int adfixedpoint_toolarge_(double *cumul, float *minCumul) {
  return adFixedPoint_tooLarge(*cumul, *minCumul) ;
}
