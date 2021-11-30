#include "COST_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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
      subroutine cost_tile( mytime, myiter, myThid )

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
#include "cost.h"
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_FIELDS.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      integer myiter
      integer myThid

#ifdef ALLOW_COST

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer bi, bj
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta         = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE uvel, vvel    = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hfacw, hfacs  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif
# ifdef ALLOW_PTRACERS
#  ifdef NONLIN_FRSURF
CADJ STORE hFacC  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE ptracer = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#  endif
# endif
#endif

      IF ( myTime .GT. (EndTime - lastinterval) ) THEN

#if (defined (ALLOW_COST_TEST) || defined (ALLOW_COST_ATLANTIC_HEAT) || defined (ALLOW_COST_TEMP) )
         CALL COST_ACCUMULATE_MEAN (myThid)
#endif

#ifdef ALLOW_COST_SHELFICE
         CALL SHELFICE_COST_ACCUMULATE (myThid)
#endif

#if (defined ALLOW_THSICE && defined ALLOW_THSICE_COST_TEST)
         IF (useThSIce)
     &    CALL THSICE_COST_DRIVER (myTime, myIter, myThid)
#endif

      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_COST_TRACER
           CALL COST_TRACER( bi, bj, myThid )
#endif

#ifdef ALLOW_COST_TRANSPORT
           CALL COST_TRANSPORT( bi, bj, myTime, myIter, myThid )
#endif /* ALLOW_COST_TRANSPORT */

        ENDDO
      ENDDO

#endif /* ALLOW_COST */

      RETURN
      END
