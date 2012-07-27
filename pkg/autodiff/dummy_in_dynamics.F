C $Header: /u/gcmpack/MITgcm/pkg/autodiff/dummy_in_dynamics.F,v 1.2 2012/07/27 21:12:02 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: DUMMY_IN_DYNAMICS
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_DYNAMICS( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DUMMY_IN_DYNAMICS                             |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

      RETURN
      END
