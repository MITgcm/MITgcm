C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_inadmode_set_g.F,v 1.2 2007/10/08 23:50:53 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine g_autodiff_inadmode_set( myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE adautodiff_inadmode_set                       |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

cph      inAdmode = inAdTrue
      inAdmode = .FALSE.

      end









