C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_sensi.F,v 1.2 2010/03/15 16:08:29 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_sensi( myTime, myIter, myThid )
C     *==========================================================*
C     | subroutine seaice_cost_sensi
C     | o driver for seaice sensitivity cost functions
C     *==========================================================*

      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      integer myIter
      integer myThid

#ifdef ALLOW_COST
C     == Local variables

      CALL COST_ICE_TEST ( myTime, myIter, myThid )

      CALL SEAICE_COST_ACCUMULATE_MEAN (
     &    myTime, myIter, myThid)

#endif

      END
