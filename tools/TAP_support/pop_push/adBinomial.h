#ifndef ADBINOMIAL_LOADED
#define ADBINOMIAL_LOADED 1

#define PUSHSNAP  1
#define LOOKSNAP  2
#define POPSNAP   3
#define ADVANCE   4
#define FIRSTTURN 5
#define TURN      6

/******************************* Exported Functions: */

/** Initializes a (possibly nested) binomial checkpointing
 * session for "length" steps using at most "nbSnap" snapshots.
 * "firstStep' is the offset, i.e. the index of the 1st step. */
extern void adBinomial_init(int length, int nbSnap, int firstStep) ;

/** Returns in *action the code for the next binomial
 * checkpointing action to perform, and in *step the index of
 * the step to which this action refers to. If no action is left,
 * restores the stack to the enclosing binomial session (if any).
 * Returns 0 if some action is found, 0 otherwise. */
extern int adBinomial_next(int *action, int *step) ;

/** Must be called when an "ADVANCE" order was issued,
 * but then the iterative loop reaches past its exit point. */
extern void adBinomial_resize() ;

#endif
