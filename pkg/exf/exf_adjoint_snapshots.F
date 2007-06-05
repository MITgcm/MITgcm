C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_adjoint_snapshots.F,v 1.1 2007/06/05 21:51:22 heimbach Exp $

#include "AUTODIFF_OPTIONS.h"

      subroutine exf_adjoint_snapshots( iwhen, mytime, myiter, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE exf_adjoint_snapshots                         |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer iwhen
      integer myThid
      integer myiter
      _RL     mytime 

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      end









