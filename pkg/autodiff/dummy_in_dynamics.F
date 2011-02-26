C $Header: /u/gcmpack/MITgcm/pkg/autodiff/dummy_in_dynamics.F,v 1.1 2011/02/26 00:41:20 heimbach Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine dummy_in_dynamics( mytime, myiter, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE dummy_in_stepping                             |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myiter
      _RL     mytime

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      end









