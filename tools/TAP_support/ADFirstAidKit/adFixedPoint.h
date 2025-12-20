#ifndef ADFIXEDPOINT_LOADED
#define ADFIXEDPOINT_LOADED 1

/** Returns true when adjoint fixed point must iterate once more
 * because reduction factor of variation "cumul" is still too large */
extern int adFixedPoint_notReduced(double cumul, float reduction) ;

/** Returns true when adjoint fixed point must iterate once more
 * because the variation "cumul" is considered still too large */
extern int adFixedPoint_tooLarge(double cumul, float minCumul) ;

#endif
