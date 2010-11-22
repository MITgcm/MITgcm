C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/forward_step.F,v 1.2 2010/11/22 23:48:33 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FORWARD_STEP
C     !INTERFACE:
      SUBROUTINE FORWARD_STEP( iloop, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | SUBROUTINE forward_step
C     | o Run the ocean model and, optionally, evaluate a cost function.
C     *==================================================================
C     |
C     | THE_MAIN_LOOP is the toplevel routine for the Tangent Linear and
C     | Adjoint Model Compiler (TAMC). For this purpose the initialization
C     | of the model was split into two parts. Those parameters that do
C     | not depend on a specific model run are set in INITIALISE_FIXED,
C     | whereas those that do depend on the specific realization are
C     | initialized in INITIALISE_VARIA.
C     |
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !LOCAL VARIABLES:
C     == Routine arguments ==
C     note: under the multi-threaded model myiter and
C           mytime are local variables passed around as routine
C           arguments. Although this is fiddly it saves the need to
C           impose additional synchronisation points when they are
C           updated.
C     myiter - iteration counter for this thread
C     mytime - time counter for this thread
C     mythid - thread number for this instance of the routine.
      integer iloop
      integer mythid
      integer myiter
      _RL     mytime

CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('FORWARD_STEP',myThid)
#endif

c--  This is a placeholder routine for the ecco setup.
c--  In the ECCO configuration this routine is inlined
c--  in the_main_loop routine for AD-reasons.

      STOP 'S/R forward_step should never be called when ALLOW_ECCO'

      END
