C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_tile.F,v 1.3 2001/09/28 15:56:14 heimbach Exp $

#include "CPP_OPTIONS.h"

CBOI
C
C !TITLE: COST FUNCTION
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: cost function evaluation
C \bv
c The cost function package is connected to the differntiability
c of the code. Differentiability refers to computing the derivative
c of a cost function with respect to a set of control variables
c (initial state, boundary values, model parameters).
c The cost function may be an element of the final state,
c a diagonstic thereof, a quantity that is integrated over the
c model trajectory, or some model vs. data misfit.
c This routine controls the cost function evaluation
c at each time step.
c Different contributions to the cost function are called from here.
c The present package contains the basic handling of the
c cost function evaluation along with two prototype cost terms.
c The cost package is connected with the mitgcm code as follows:
c
C     !CALLING SEQUENCE:
c    ...
c     |-- initialise_fixed
c         |
c         |-- packages_readparms
c             |
c             |-- cost_readparms
c    ...
c     |-- initialise_varia
c         |
c         |-- packages_init_variables
c             |
c             |-- cost_init
c    ...
c     |-- the_main_loop
c         |
c         |-- do iloop = 1,nTimeSteps
c         |     forward_step
c         |     cost_tile
c         |     |
c         |     |-- cost_tracer
c         |
c         |   enddo
c         |
c         |-- cost_final
C \ev
CEOI

CBOP
C     !ROUTINE: THE_MAIN_LOOP
C     !INTERFACE:
      subroutine cost_tile( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | subroutine cost_tile                                     
C     | o this routine computes is called at each time step to
C     |   accumulate the cost function for the tiles of this processor 
C     *================================================================*
C     \ev

C     !USES:
      implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer bi, bj
CEOP

C--   TAMC need a stmt in front of a HPF directive

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_COST_TRACER
          CALL COST_TRACER( bi, bj, myThid )
#endif

        ENDDO
      ENDDO

#endif /* ALLOW_COST */

      RETURN
      END
