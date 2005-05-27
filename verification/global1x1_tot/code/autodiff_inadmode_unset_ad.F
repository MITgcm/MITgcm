C $Header: /u/gcmpack/MITgcm/verification/global1x1_tot/code/Attic/autodiff_inadmode_unset_ad.F,v 1.1 2005/05/27 23:30:13 heimbach Exp $

#include "AUTODIFF_OPTIONS.h"

      subroutine adautodiff_inadmode_unset( myThid )
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

      inAdMode = inAdFALSE

      useKPP = .TRUE.
      useGMREDI = .TRUE.

      end









