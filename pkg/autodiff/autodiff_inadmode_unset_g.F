C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_inadmode_unset_g.F,v 1.1 2005/03/29 21:51:47 heimbach Exp $

#include "AUTODIFF_OPTIONS.h"

      subroutine g_autodiff_inadmode_unset( myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE autodiff_inadmode_unset                       |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

cph      inAdMode = inAdFALSE
      inAdMode = .false.

      end









